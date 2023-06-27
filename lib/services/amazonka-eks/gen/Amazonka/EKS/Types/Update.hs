{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Types.Update
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Update where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.ErrorDetail
import Amazonka.EKS.Types.UpdateParam
import Amazonka.EKS.Types.UpdateStatus
import Amazonka.EKS.Types.UpdateType
import qualified Amazonka.Prelude as Prelude

-- | An object representing an asynchronous update.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | The Unix epoch timestamp in seconds for when the update was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Any errors associated with a @Failed@ update.
    errors :: Prelude.Maybe [ErrorDetail],
    -- | A UUID that is used to track the update.
    id :: Prelude.Maybe Prelude.Text,
    -- | A key-value map that contains the parameters associated with the update.
    params :: Prelude.Maybe [UpdateParam],
    -- | The current status of the update.
    status :: Prelude.Maybe UpdateStatus,
    -- | The type of the update.
    type' :: Prelude.Maybe UpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Update' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'update_createdAt' - The Unix epoch timestamp in seconds for when the update was created.
--
-- 'errors', 'update_errors' - Any errors associated with a @Failed@ update.
--
-- 'id', 'update_id' - A UUID that is used to track the update.
--
-- 'params', 'update_params' - A key-value map that contains the parameters associated with the update.
--
-- 'status', 'update_status' - The current status of the update.
--
-- 'type'', 'update_type' - The type of the update.
newUpdate ::
  Update
newUpdate =
  Update'
    { createdAt = Prelude.Nothing,
      errors = Prelude.Nothing,
      id = Prelude.Nothing,
      params = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Unix epoch timestamp in seconds for when the update was created.
update_createdAt :: Lens.Lens' Update (Prelude.Maybe Prelude.UTCTime)
update_createdAt = Lens.lens (\Update' {createdAt} -> createdAt) (\s@Update' {} a -> s {createdAt = a} :: Update) Prelude.. Lens.mapping Data._Time

-- | Any errors associated with a @Failed@ update.
update_errors :: Lens.Lens' Update (Prelude.Maybe [ErrorDetail])
update_errors = Lens.lens (\Update' {errors} -> errors) (\s@Update' {} a -> s {errors = a} :: Update) Prelude.. Lens.mapping Lens.coerced

-- | A UUID that is used to track the update.
update_id :: Lens.Lens' Update (Prelude.Maybe Prelude.Text)
update_id = Lens.lens (\Update' {id} -> id) (\s@Update' {} a -> s {id = a} :: Update)

-- | A key-value map that contains the parameters associated with the update.
update_params :: Lens.Lens' Update (Prelude.Maybe [UpdateParam])
update_params = Lens.lens (\Update' {params} -> params) (\s@Update' {} a -> s {params = a} :: Update) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the update.
update_status :: Lens.Lens' Update (Prelude.Maybe UpdateStatus)
update_status = Lens.lens (\Update' {status} -> status) (\s@Update' {} a -> s {status = a} :: Update)

-- | The type of the update.
update_type :: Lens.Lens' Update (Prelude.Maybe UpdateType)
update_type = Lens.lens (\Update' {type'} -> type') (\s@Update' {} a -> s {type' = a} :: Update)

instance Data.FromJSON Update where
  parseJSON =
    Data.withObject
      "Update"
      ( \x ->
          Update'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "errors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "params" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Update where
  hashWithSalt _salt Update' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` params
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Update where
  rnf Update' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf params
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
