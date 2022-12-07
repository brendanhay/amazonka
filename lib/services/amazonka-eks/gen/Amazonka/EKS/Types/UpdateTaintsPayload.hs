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
-- Module      : Amazonka.EKS.Types.UpdateTaintsPayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.UpdateTaintsPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.Taint
import qualified Amazonka.Prelude as Prelude

-- | An object representing the details of an update to a taints payload. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
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

instance Prelude.Hashable UpdateTaintsPayload where
  hashWithSalt _salt UpdateTaintsPayload' {..} =
    _salt `Prelude.hashWithSalt` addOrUpdateTaints
      `Prelude.hashWithSalt` removeTaints

instance Prelude.NFData UpdateTaintsPayload where
  rnf UpdateTaintsPayload' {..} =
    Prelude.rnf addOrUpdateTaints
      `Prelude.seq` Prelude.rnf removeTaints

instance Data.ToJSON UpdateTaintsPayload where
  toJSON UpdateTaintsPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addOrUpdateTaints" Data..=)
              Prelude.<$> addOrUpdateTaints,
            ("removeTaints" Data..=) Prelude.<$> removeTaints
          ]
      )
