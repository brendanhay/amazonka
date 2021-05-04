{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EKS.Types.Update
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Update where

import Network.AWS.EKS.Types.ErrorDetail
import Network.AWS.EKS.Types.UpdateParam
import Network.AWS.EKS.Types.UpdateStatus
import Network.AWS.EKS.Types.UpdateType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an asynchronous update.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | The current status of the update.
    status :: Prelude.Maybe UpdateStatus,
    -- | The Unix epoch timestamp in seconds for when the update was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | A UUID that is used to track the update.
    id :: Prelude.Maybe Prelude.Text,
    -- | A key-value map that contains the parameters associated with the update.
    params :: Prelude.Maybe [UpdateParam],
    -- | Any errors associated with a @Failed@ update.
    errors :: Prelude.Maybe [ErrorDetail],
    -- | The type of the update.
    type' :: Prelude.Maybe UpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Update' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'update_status' - The current status of the update.
--
-- 'createdAt', 'update_createdAt' - The Unix epoch timestamp in seconds for when the update was created.
--
-- 'id', 'update_id' - A UUID that is used to track the update.
--
-- 'params', 'update_params' - A key-value map that contains the parameters associated with the update.
--
-- 'errors', 'update_errors' - Any errors associated with a @Failed@ update.
--
-- 'type'', 'update_type' - The type of the update.
newUpdate ::
  Update
newUpdate =
  Update'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      params = Prelude.Nothing,
      errors = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The current status of the update.
update_status :: Lens.Lens' Update (Prelude.Maybe UpdateStatus)
update_status = Lens.lens (\Update' {status} -> status) (\s@Update' {} a -> s {status = a} :: Update)

-- | The Unix epoch timestamp in seconds for when the update was created.
update_createdAt :: Lens.Lens' Update (Prelude.Maybe Prelude.UTCTime)
update_createdAt = Lens.lens (\Update' {createdAt} -> createdAt) (\s@Update' {} a -> s {createdAt = a} :: Update) Prelude.. Lens.mapping Prelude._Time

-- | A UUID that is used to track the update.
update_id :: Lens.Lens' Update (Prelude.Maybe Prelude.Text)
update_id = Lens.lens (\Update' {id} -> id) (\s@Update' {} a -> s {id = a} :: Update)

-- | A key-value map that contains the parameters associated with the update.
update_params :: Lens.Lens' Update (Prelude.Maybe [UpdateParam])
update_params = Lens.lens (\Update' {params} -> params) (\s@Update' {} a -> s {params = a} :: Update) Prelude.. Lens.mapping Prelude._Coerce

-- | Any errors associated with a @Failed@ update.
update_errors :: Lens.Lens' Update (Prelude.Maybe [ErrorDetail])
update_errors = Lens.lens (\Update' {errors} -> errors) (\s@Update' {} a -> s {errors = a} :: Update) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of the update.
update_type :: Lens.Lens' Update (Prelude.Maybe UpdateType)
update_type = Lens.lens (\Update' {type'} -> type') (\s@Update' {} a -> s {type' = a} :: Update)

instance Prelude.FromJSON Update where
  parseJSON =
    Prelude.withObject
      "Update"
      ( \x ->
          Update'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "params" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "errors" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable Update

instance Prelude.NFData Update
