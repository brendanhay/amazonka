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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact

-- | An object that contains summary information about a product view and a
-- provisioning artifact.
--
-- /See:/ 'newProvisioningArtifactView' smart constructor.
data ProvisioningArtifactView = ProvisioningArtifactView'
  { -- | Summary information about a product view.
    productViewSummary :: Prelude.Maybe ProductViewSummary,
    -- | Information about a provisioning artifact. A provisioning artifact is
    -- also known as a product version.
    provisioningArtifact :: Prelude.Maybe ProvisioningArtifact
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productViewSummary', 'provisioningArtifactView_productViewSummary' - Summary information about a product view.
--
-- 'provisioningArtifact', 'provisioningArtifactView_provisioningArtifact' - Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
newProvisioningArtifactView ::
  ProvisioningArtifactView
newProvisioningArtifactView =
  ProvisioningArtifactView'
    { productViewSummary =
        Prelude.Nothing,
      provisioningArtifact = Prelude.Nothing
    }

-- | Summary information about a product view.
provisioningArtifactView_productViewSummary :: Lens.Lens' ProvisioningArtifactView (Prelude.Maybe ProductViewSummary)
provisioningArtifactView_productViewSummary = Lens.lens (\ProvisioningArtifactView' {productViewSummary} -> productViewSummary) (\s@ProvisioningArtifactView' {} a -> s {productViewSummary = a} :: ProvisioningArtifactView)

-- | Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
provisioningArtifactView_provisioningArtifact :: Lens.Lens' ProvisioningArtifactView (Prelude.Maybe ProvisioningArtifact)
provisioningArtifactView_provisioningArtifact = Lens.lens (\ProvisioningArtifactView' {provisioningArtifact} -> provisioningArtifact) (\s@ProvisioningArtifactView' {} a -> s {provisioningArtifact = a} :: ProvisioningArtifactView)

instance Prelude.FromJSON ProvisioningArtifactView where
  parseJSON =
    Prelude.withObject
      "ProvisioningArtifactView"
      ( \x ->
          ProvisioningArtifactView'
            Prelude.<$> (x Prelude..:? "ProductViewSummary")
            Prelude.<*> (x Prelude..:? "ProvisioningArtifact")
      )

instance Prelude.Hashable ProvisioningArtifactView

instance Prelude.NFData ProvisioningArtifactView
