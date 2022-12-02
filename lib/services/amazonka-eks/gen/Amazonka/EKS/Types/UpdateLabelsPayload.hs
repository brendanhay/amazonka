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
-- Module      : Amazonka.EKS.Types.UpdateLabelsPayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.UpdateLabelsPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a Kubernetes label change for a managed node
-- group.
--
-- /See:/ 'newUpdateLabelsPayload' smart constructor.
data UpdateLabelsPayload = UpdateLabelsPayload'
  { -- | Kubernetes labels to be added or updated.
    addOrUpdateLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Kubernetes labels to be removed.
    removeLabels :: Prelude.Maybe [Prelude.Text]
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
-- 'addOrUpdateLabels', 'updateLabelsPayload_addOrUpdateLabels' - Kubernetes labels to be added or updated.
--
-- 'removeLabels', 'updateLabelsPayload_removeLabels' - Kubernetes labels to be removed.
newUpdateLabelsPayload ::
  UpdateLabelsPayload
newUpdateLabelsPayload =
  UpdateLabelsPayload'
    { addOrUpdateLabels =
        Prelude.Nothing,
      removeLabels = Prelude.Nothing
    }

-- | Kubernetes labels to be added or updated.
updateLabelsPayload_addOrUpdateLabels :: Lens.Lens' UpdateLabelsPayload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateLabelsPayload_addOrUpdateLabels = Lens.lens (\UpdateLabelsPayload' {addOrUpdateLabels} -> addOrUpdateLabels) (\s@UpdateLabelsPayload' {} a -> s {addOrUpdateLabels = a} :: UpdateLabelsPayload) Prelude.. Lens.mapping Lens.coerced

-- | Kubernetes labels to be removed.
updateLabelsPayload_removeLabels :: Lens.Lens' UpdateLabelsPayload (Prelude.Maybe [Prelude.Text])
updateLabelsPayload_removeLabels = Lens.lens (\UpdateLabelsPayload' {removeLabels} -> removeLabels) (\s@UpdateLabelsPayload' {} a -> s {removeLabels = a} :: UpdateLabelsPayload) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UpdateLabelsPayload where
  hashWithSalt _salt UpdateLabelsPayload' {..} =
    _salt `Prelude.hashWithSalt` addOrUpdateLabels
      `Prelude.hashWithSalt` removeLabels

instance Prelude.NFData UpdateLabelsPayload where
  rnf UpdateLabelsPayload' {..} =
    Prelude.rnf addOrUpdateLabels
      `Prelude.seq` Prelude.rnf removeLabels

instance Data.ToJSON UpdateLabelsPayload where
  toJSON UpdateLabelsPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addOrUpdateLabels" Data..=)
              Prelude.<$> addOrUpdateLabels,
            ("removeLabels" Data..=) Prelude.<$> removeLabels
          ]
      )
