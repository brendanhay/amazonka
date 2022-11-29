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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactView
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactView where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProductViewSummary
import Amazonka.ServiceCatalog.Types.ProvisioningArtifact

-- | An object that contains summary information about a product view and a
-- provisioning artifact.
--
-- /See:/ 'newProvisioningArtifactView' smart constructor.
data ProvisioningArtifactView = ProvisioningArtifactView'
  { -- | Information about a provisioning artifact. A provisioning artifact is
    -- also known as a product version.
    provisioningArtifact :: Prelude.Maybe ProvisioningArtifact,
    -- | Summary information about a product view.
    productViewSummary :: Prelude.Maybe ProductViewSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifact', 'provisioningArtifactView_provisioningArtifact' - Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
--
-- 'productViewSummary', 'provisioningArtifactView_productViewSummary' - Summary information about a product view.
newProvisioningArtifactView ::
  ProvisioningArtifactView
newProvisioningArtifactView =
  ProvisioningArtifactView'
    { provisioningArtifact =
        Prelude.Nothing,
      productViewSummary = Prelude.Nothing
    }

-- | Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
provisioningArtifactView_provisioningArtifact :: Lens.Lens' ProvisioningArtifactView (Prelude.Maybe ProvisioningArtifact)
provisioningArtifactView_provisioningArtifact = Lens.lens (\ProvisioningArtifactView' {provisioningArtifact} -> provisioningArtifact) (\s@ProvisioningArtifactView' {} a -> s {provisioningArtifact = a} :: ProvisioningArtifactView)

-- | Summary information about a product view.
provisioningArtifactView_productViewSummary :: Lens.Lens' ProvisioningArtifactView (Prelude.Maybe ProductViewSummary)
provisioningArtifactView_productViewSummary = Lens.lens (\ProvisioningArtifactView' {productViewSummary} -> productViewSummary) (\s@ProvisioningArtifactView' {} a -> s {productViewSummary = a} :: ProvisioningArtifactView)

instance Core.FromJSON ProvisioningArtifactView where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactView"
      ( \x ->
          ProvisioningArtifactView'
            Prelude.<$> (x Core..:? "ProvisioningArtifact")
            Prelude.<*> (x Core..:? "ProductViewSummary")
      )

instance Prelude.Hashable ProvisioningArtifactView where
  hashWithSalt _salt ProvisioningArtifactView' {..} =
    _salt `Prelude.hashWithSalt` provisioningArtifact
      `Prelude.hashWithSalt` productViewSummary

instance Prelude.NFData ProvisioningArtifactView where
  rnf ProvisioningArtifactView' {..} =
    Prelude.rnf provisioningArtifact
      `Prelude.seq` Prelude.rnf productViewSummary
