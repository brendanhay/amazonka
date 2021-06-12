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

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.ErrorDetail
import Network.AWS.EKS.Types.UpdateParam
import Network.AWS.EKS.Types.UpdateStatus
import Network.AWS.EKS.Types.UpdateType
import qualified Network.AWS.Lens as Lens

-- | An object representing an asynchronous update.
--
-- /See:/ 'newUpdate' smart constructor.
data Update = Update'
  { -- | The current status of the update.
    status :: Core.Maybe UpdateStatus,
    -- | The Unix epoch timestamp in seconds for when the update was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | A UUID that is used to track the update.
    id :: Core.Maybe Core.Text,
    -- | A key-value map that contains the parameters associated with the update.
    params :: Core.Maybe [UpdateParam],
    -- | Any errors associated with a @Failed@ update.
    errors :: Core.Maybe [ErrorDetail],
    -- | The type of the update.
    type' :: Core.Maybe UpdateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      createdAt = Core.Nothing,
      id = Core.Nothing,
      params = Core.Nothing,
      errors = Core.Nothing,
      type' = Core.Nothing
    }

-- | The current status of the update.
update_status :: Lens.Lens' Update (Core.Maybe UpdateStatus)
update_status = Lens.lens (\Update' {status} -> status) (\s@Update' {} a -> s {status = a} :: Update)

-- | The Unix epoch timestamp in seconds for when the update was created.
update_createdAt :: Lens.Lens' Update (Core.Maybe Core.UTCTime)
update_createdAt = Lens.lens (\Update' {createdAt} -> createdAt) (\s@Update' {} a -> s {createdAt = a} :: Update) Core.. Lens.mapping Core._Time

-- | A UUID that is used to track the update.
update_id :: Lens.Lens' Update (Core.Maybe Core.Text)
update_id = Lens.lens (\Update' {id} -> id) (\s@Update' {} a -> s {id = a} :: Update)

-- | A key-value map that contains the parameters associated with the update.
update_params :: Lens.Lens' Update (Core.Maybe [UpdateParam])
update_params = Lens.lens (\Update' {params} -> params) (\s@Update' {} a -> s {params = a} :: Update) Core.. Lens.mapping Lens._Coerce

-- | Any errors associated with a @Failed@ update.
update_errors :: Lens.Lens' Update (Core.Maybe [ErrorDetail])
update_errors = Lens.lens (\Update' {errors} -> errors) (\s@Update' {} a -> s {errors = a} :: Update) Core.. Lens.mapping Lens._Coerce

-- | The type of the update.
update_type :: Lens.Lens' Update (Core.Maybe UpdateType)
update_type = Lens.lens (\Update' {type'} -> type') (\s@Update' {} a -> s {type' = a} :: Update)

instance Core.FromJSON Update where
  parseJSON =
    Core.withObject
      "Update"
      ( \x ->
          Update'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "params" Core..!= Core.mempty)
            Core.<*> (x Core..:? "errors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable Update

instance Core.NFData Update
