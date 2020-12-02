{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provisioning artifact output.
--
--
--
-- /See:/ 'provisioningArtifactOutput' smart constructor.
data ProvisioningArtifactOutput = ProvisioningArtifactOutput'
  { _paoKey ::
      !(Maybe Text),
    _paoDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paoKey' - The provisioning artifact output key.
--
-- * 'paoDescription' - Description of the provisioning artifact output key.
provisioningArtifactOutput ::
  ProvisioningArtifactOutput
provisioningArtifactOutput =
  ProvisioningArtifactOutput'
    { _paoKey = Nothing,
      _paoDescription = Nothing
    }

-- | The provisioning artifact output key.
paoKey :: Lens' ProvisioningArtifactOutput (Maybe Text)
paoKey = lens _paoKey (\s a -> s {_paoKey = a})

-- | Description of the provisioning artifact output key.
paoDescription :: Lens' ProvisioningArtifactOutput (Maybe Text)
paoDescription = lens _paoDescription (\s a -> s {_paoDescription = a})

instance FromJSON ProvisioningArtifactOutput where
  parseJSON =
    withObject
      "ProvisioningArtifactOutput"
      ( \x ->
          ProvisioningArtifactOutput'
            <$> (x .:? "Key") <*> (x .:? "Description")
      )

instance Hashable ProvisioningArtifactOutput

instance NFData ProvisioningArtifactOutput
