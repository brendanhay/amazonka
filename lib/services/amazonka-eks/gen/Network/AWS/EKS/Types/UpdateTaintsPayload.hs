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
-- Module      : Network.AWS.EKS.Types.UpdateTaintsPayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateTaintsPayload where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.Taint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the details of an update to a taints payload.
--
-- /See:/ 'newUpdateTaintsPayload' smart constructor.
data UpdateTaintsPayload = UpdateTaintsPayload'
  { -- | Kubernetes taints to be added or updated.
    addOrUpdateTaints :: Prelude.Maybe [Taint],
    -- | Kubernetes taints to be removed.
    removeTaints :: Prelude.Maybe [Taint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaintsPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOrUpdateTaints', 'updateTaintsPayload_addOrUpdateTaints' - Kubernetes taints to be added or updated.
--
-- 'removeTaints', 'updateTaintsPayload_removeTaints' - Kubernetes taints to be removed.
newUpdateTaintsPayload ::
  UpdateTaintsPayload
newUpdateTaintsPayload =
  UpdateTaintsPayload'
    { addOrUpdateTaints =
        Prelude.Nothing,
      removeTaints = Prelude.Nothing
    }

-- | Kubernetes taints to be added or updated.
updateTaintsPayload_addOrUpdateTaints :: Lens.Lens' UpdateTaintsPayload (Prelude.Maybe [Taint])
updateTaintsPayload_addOrUpdateTaints = Lens.lens (\UpdateTaintsPayload' {addOrUpdateTaints} -> addOrUpdateTaints) (\s@UpdateTaintsPayload' {} a -> s {addOrUpdateTaints = a} :: UpdateTaintsPayload) Prelude.. Lens.mapping Lens.coerced

-- | Kubernetes taints to be removed.
updateTaintsPayload_removeTaints :: Lens.Lens' UpdateTaintsPayload (Prelude.Maybe [Taint])
updateTaintsPayload_removeTaints = Lens.lens (\UpdateTaintsPayload' {removeTaints} -> removeTaints) (\s@UpdateTaintsPayload' {} a -> s {removeTaints = a} :: UpdateTaintsPayload) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UpdateTaintsPayload

instance Prelude.NFData UpdateTaintsPayload

instance Core.ToJSON UpdateTaintsPayload where
  toJSON UpdateTaintsPayload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("addOrUpdateTaints" Core..=)
              Prelude.<$> addOrUpdateTaints,
            ("removeTaints" Core..=) Prelude.<$> removeTaints
          ]
      )
