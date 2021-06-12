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
-- Module      : Network.AWS.EKS.Types.UpdateLabelsPayload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateLabelsPayload where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing a Kubernetes label change for a managed node
-- group.
--
-- /See:/ 'newUpdateLabelsPayload' smart constructor.
data UpdateLabelsPayload = UpdateLabelsPayload'
  { -- | Kubernetes labels to be removed.
    removeLabels :: Core.Maybe [Core.Text],
    -- | Kubernetes labels to be added or updated.
    addOrUpdateLabels :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLabelsPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeLabels', 'updateLabelsPayload_removeLabels' - Kubernetes labels to be removed.
--
-- 'addOrUpdateLabels', 'updateLabelsPayload_addOrUpdateLabels' - Kubernetes labels to be added or updated.
newUpdateLabelsPayload ::
  UpdateLabelsPayload
newUpdateLabelsPayload =
  UpdateLabelsPayload'
    { removeLabels = Core.Nothing,
      addOrUpdateLabels = Core.Nothing
    }

-- | Kubernetes labels to be removed.
updateLabelsPayload_removeLabels :: Lens.Lens' UpdateLabelsPayload (Core.Maybe [Core.Text])
updateLabelsPayload_removeLabels = Lens.lens (\UpdateLabelsPayload' {removeLabels} -> removeLabels) (\s@UpdateLabelsPayload' {} a -> s {removeLabels = a} :: UpdateLabelsPayload) Core.. Lens.mapping Lens._Coerce

-- | Kubernetes labels to be added or updated.
updateLabelsPayload_addOrUpdateLabels :: Lens.Lens' UpdateLabelsPayload (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateLabelsPayload_addOrUpdateLabels = Lens.lens (\UpdateLabelsPayload' {addOrUpdateLabels} -> addOrUpdateLabels) (\s@UpdateLabelsPayload' {} a -> s {addOrUpdateLabels = a} :: UpdateLabelsPayload) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable UpdateLabelsPayload

instance Core.NFData UpdateLabelsPayload

instance Core.ToJSON UpdateLabelsPayload where
  toJSON UpdateLabelsPayload' {..} =
    Core.object
      ( Core.catMaybes
          [ ("removeLabels" Core..=) Core.<$> removeLabels,
            ("addOrUpdateLabels" Core..=)
              Core.<$> addOrUpdateLabels
          ]
      )
