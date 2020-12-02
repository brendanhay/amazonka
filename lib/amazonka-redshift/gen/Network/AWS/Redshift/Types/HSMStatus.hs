{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of changes to HSM settings.
--
--
--
-- /See:/ 'hsmStatus' smart constructor.
data HSMStatus = HSMStatus'
  { _hsStatus :: !(Maybe Text),
    _hsHSMConfigurationIdentifier :: !(Maybe Text),
    _hsHSMClientCertificateIdentifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HSMStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsStatus' - Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command. Values: active, applying
--
-- * 'hsHSMConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- * 'hsHSMClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
hsmStatus ::
  HSMStatus
hsmStatus =
  HSMStatus'
    { _hsStatus = Nothing,
      _hsHSMConfigurationIdentifier = Nothing,
      _hsHSMClientCertificateIdentifier = Nothing
    }

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command. Values: active, applying
hsStatus :: Lens' HSMStatus (Maybe Text)
hsStatus = lens _hsStatus (\s a -> s {_hsStatus = a})

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHSMConfigurationIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMConfigurationIdentifier = lens _hsHSMConfigurationIdentifier (\s a -> s {_hsHSMConfigurationIdentifier = a})

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
hsHSMClientCertificateIdentifier :: Lens' HSMStatus (Maybe Text)
hsHSMClientCertificateIdentifier = lens _hsHSMClientCertificateIdentifier (\s a -> s {_hsHSMClientCertificateIdentifier = a})

instance FromXML HSMStatus where
  parseXML x =
    HSMStatus'
      <$> (x .@? "Status")
      <*> (x .@? "HsmConfigurationIdentifier")
      <*> (x .@? "HsmClientCertificateIdentifier")

instance Hashable HSMStatus

instance NFData HSMStatus
