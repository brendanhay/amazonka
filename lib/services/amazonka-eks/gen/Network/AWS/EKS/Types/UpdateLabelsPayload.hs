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
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a Kubernetes label change for a managed node
-- group.
--
-- /See:/ 'newUpdateLabelsPayload' smart constructor.
data UpdateLabelsPayload = UpdateLabelsPayload'
  { -- | Kubernetes labels to be removed.
    removeLabels :: Prelude.Maybe [Prelude.Text],
    -- | Kubernetes labels to be added or updated.
    addOrUpdateLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { removeLabels =
        Prelude.Nothing,
      addOrUpdateLabels = Prelude.Nothing
    }

-- | Kubernetes labels to be removed.
updateLabelsPayload_removeLabels :: Lens.Lens' UpdateLabelsPayload (Prelude.Maybe [Prelude.Text])
updateLabelsPayload_removeLabels = Lens.lens (\UpdateLabelsPayload' {removeLabels} -> removeLabels) (\s@UpdateLabelsPayload' {} a -> s {removeLabels = a} :: UpdateLabelsPayload) Prelude.. Lens.mapping Lens._Coerce

-- | Kubernetes labels to be added or updated.
updateLabelsPayload_addOrUpdateLabels :: Lens.Lens' UpdateLabelsPayload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateLabelsPayload_addOrUpdateLabels = Lens.lens (\UpdateLabelsPayload' {addOrUpdateLabels} -> addOrUpdateLabels) (\s@UpdateLabelsPayload' {} a -> s {addOrUpdateLabels = a} :: UpdateLabelsPayload) Prelude.. Lens.mapping Lens._Coerce

instance Prelude.Hashable UpdateLabelsPayload

instance Prelude.NFData UpdateLabelsPayload

instance Core.ToJSON UpdateLabelsPayload where
  toJSON UpdateLabelsPayload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("removeLabels" Core..=) Prelude.<$> removeLabels,
            ("addOrUpdateLabels" Core..=)
              Prelude.<$> addOrUpdateLabels
          ]
      )
