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
-- Module      : Amazonka.Outposts.Types.ComputeAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ComputeAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.ComputeAssetState
import qualified Amazonka.Prelude as Prelude

-- | Information about compute hardware assets.
--
-- /See:/ 'newComputeAttributes' smart constructor.
data ComputeAttributes = ComputeAttributes'
  { -- | The host ID of the Dedicated Host on the asset.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The state.
    --
    -- -   ACTIVE - The asset is available and can provide capacity for new
    --     compute resources.
    --
    -- -   ISOLATED - The asset is undergoing maintenance and can\'t provide
    --     capacity for new compute resources. Existing compute resources on
    --     the asset are not affected.
    --
    -- -   RETIRING - The underlying hardware for the asset is degraded.
    --     Capacity for new compute resources is reduced. Amazon Web Services
    --     sends notifications for resources that must be stopped before the
    --     asset can be replaced.
    state :: Prelude.Maybe ComputeAssetState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostId', 'computeAttributes_hostId' - The host ID of the Dedicated Host on the asset.
--
-- 'state', 'computeAttributes_state' - The state.
--
-- -   ACTIVE - The asset is available and can provide capacity for new
--     compute resources.
--
-- -   ISOLATED - The asset is undergoing maintenance and can\'t provide
--     capacity for new compute resources. Existing compute resources on
--     the asset are not affected.
--
-- -   RETIRING - The underlying hardware for the asset is degraded.
--     Capacity for new compute resources is reduced. Amazon Web Services
--     sends notifications for resources that must be stopped before the
--     asset can be replaced.
newComputeAttributes ::
  ComputeAttributes
newComputeAttributes =
  ComputeAttributes'
    { hostId = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The host ID of the Dedicated Host on the asset.
computeAttributes_hostId :: Lens.Lens' ComputeAttributes (Prelude.Maybe Prelude.Text)
computeAttributes_hostId = Lens.lens (\ComputeAttributes' {hostId} -> hostId) (\s@ComputeAttributes' {} a -> s {hostId = a} :: ComputeAttributes)

-- | The state.
--
-- -   ACTIVE - The asset is available and can provide capacity for new
--     compute resources.
--
-- -   ISOLATED - The asset is undergoing maintenance and can\'t provide
--     capacity for new compute resources. Existing compute resources on
--     the asset are not affected.
--
-- -   RETIRING - The underlying hardware for the asset is degraded.
--     Capacity for new compute resources is reduced. Amazon Web Services
--     sends notifications for resources that must be stopped before the
--     asset can be replaced.
computeAttributes_state :: Lens.Lens' ComputeAttributes (Prelude.Maybe ComputeAssetState)
computeAttributes_state = Lens.lens (\ComputeAttributes' {state} -> state) (\s@ComputeAttributes' {} a -> s {state = a} :: ComputeAttributes)

instance Data.FromJSON ComputeAttributes where
  parseJSON =
    Data.withObject
      "ComputeAttributes"
      ( \x ->
          ComputeAttributes'
            Prelude.<$> (x Data..:? "HostId")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ComputeAttributes where
  hashWithSalt _salt ComputeAttributes' {..} =
    _salt `Prelude.hashWithSalt` hostId
      `Prelude.hashWithSalt` state

instance Prelude.NFData ComputeAttributes where
  rnf ComputeAttributes' {..} =
    Prelude.rnf hostId `Prelude.seq` Prelude.rnf state
