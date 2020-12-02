{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A self-service action association consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
--
--
-- /See:/ 'serviceActionAssociation' smart constructor.
data ServiceActionAssociation = ServiceActionAssociation'
  { _saaServiceActionId ::
      !Text,
    _saaProductId :: !Text,
    _saaProvisioningArtifactId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceActionAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saaServiceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- * 'saaProductId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- * 'saaProvisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
serviceActionAssociation ::
  -- | 'saaServiceActionId'
  Text ->
  -- | 'saaProductId'
  Text ->
  -- | 'saaProvisioningArtifactId'
  Text ->
  ServiceActionAssociation
serviceActionAssociation
  pServiceActionId_
  pProductId_
  pProvisioningArtifactId_ =
    ServiceActionAssociation'
      { _saaServiceActionId =
          pServiceActionId_,
        _saaProductId = pProductId_,
        _saaProvisioningArtifactId = pProvisioningArtifactId_
      }

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
saaServiceActionId :: Lens' ServiceActionAssociation Text
saaServiceActionId = lens _saaServiceActionId (\s a -> s {_saaServiceActionId = a})

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
saaProductId :: Lens' ServiceActionAssociation Text
saaProductId = lens _saaProductId (\s a -> s {_saaProductId = a})

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
saaProvisioningArtifactId :: Lens' ServiceActionAssociation Text
saaProvisioningArtifactId = lens _saaProvisioningArtifactId (\s a -> s {_saaProvisioningArtifactId = a})

instance Hashable ServiceActionAssociation

instance NFData ServiceActionAssociation

instance ToJSON ServiceActionAssociation where
  toJSON ServiceActionAssociation' {..} =
    object
      ( catMaybes
          [ Just ("ServiceActionId" .= _saaServiceActionId),
            Just ("ProductId" .= _saaProductId),
            Just ("ProvisioningArtifactId" .= _saaProvisioningArtifactId)
          ]
      )
