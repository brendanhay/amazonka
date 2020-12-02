{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact

-- | An object that contains summary information about a product view and a provisioning artifact.
--
--
--
-- /See:/ 'provisioningArtifactView' smart constructor.
data ProvisioningArtifactView = ProvisioningArtifactView'
  { _pavProductViewSummary ::
      !(Maybe ProductViewSummary),
    _pavProvisioningArtifact ::
      !(Maybe ProvisioningArtifact)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pavProductViewSummary' - Summary information about a product view.
--
-- * 'pavProvisioningArtifact' - Information about a provisioning artifact. A provisioning artifact is also known as a product version.
provisioningArtifactView ::
  ProvisioningArtifactView
provisioningArtifactView =
  ProvisioningArtifactView'
    { _pavProductViewSummary = Nothing,
      _pavProvisioningArtifact = Nothing
    }

-- | Summary information about a product view.
pavProductViewSummary :: Lens' ProvisioningArtifactView (Maybe ProductViewSummary)
pavProductViewSummary = lens _pavProductViewSummary (\s a -> s {_pavProductViewSummary = a})

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
pavProvisioningArtifact :: Lens' ProvisioningArtifactView (Maybe ProvisioningArtifact)
pavProvisioningArtifact = lens _pavProvisioningArtifact (\s a -> s {_pavProvisioningArtifact = a})

instance FromJSON ProvisioningArtifactView where
  parseJSON =
    withObject
      "ProvisioningArtifactView"
      ( \x ->
          ProvisioningArtifactView'
            <$> (x .:? "ProductViewSummary") <*> (x .:? "ProvisioningArtifact")
      )

instance Hashable ProvisioningArtifactView

instance NFData ProvisioningArtifactView
