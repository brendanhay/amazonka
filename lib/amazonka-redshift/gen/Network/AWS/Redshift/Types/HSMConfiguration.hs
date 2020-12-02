{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.
--
--
--
-- /See:/ 'hsmConfiguration' smart constructor.
data HSMConfiguration = HSMConfiguration'
  { _hcHSMConfigurationIdentifier ::
      !(Maybe Text),
    _hcHSMPartitionName :: !(Maybe Text),
    _hcDescription :: !(Maybe Text),
    _hcTags :: !(Maybe [Tag]),
    _hcHSMIPAddress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HSMConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcHSMConfigurationIdentifier' - The name of the Amazon Redshift HSM configuration.
--
-- * 'hcHSMPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- * 'hcDescription' - A text description of the HSM configuration.
--
-- * 'hcTags' - The list of tags for the HSM configuration.
--
-- * 'hcHSMIPAddress' - The IP address that the Amazon Redshift cluster must use to access the HSM.
hsmConfiguration ::
  HSMConfiguration
hsmConfiguration =
  HSMConfiguration'
    { _hcHSMConfigurationIdentifier = Nothing,
      _hcHSMPartitionName = Nothing,
      _hcDescription = Nothing,
      _hcTags = Nothing,
      _hcHSMIPAddress = Nothing
    }

-- | The name of the Amazon Redshift HSM configuration.
hcHSMConfigurationIdentifier :: Lens' HSMConfiguration (Maybe Text)
hcHSMConfigurationIdentifier = lens _hcHSMConfigurationIdentifier (\s a -> s {_hcHSMConfigurationIdentifier = a})

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
hcHSMPartitionName :: Lens' HSMConfiguration (Maybe Text)
hcHSMPartitionName = lens _hcHSMPartitionName (\s a -> s {_hcHSMPartitionName = a})

-- | A text description of the HSM configuration.
hcDescription :: Lens' HSMConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\s a -> s {_hcDescription = a})

-- | The list of tags for the HSM configuration.
hcTags :: Lens' HSMConfiguration [Tag]
hcTags = lens _hcTags (\s a -> s {_hcTags = a}) . _Default . _Coerce

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHSMIPAddress :: Lens' HSMConfiguration (Maybe Text)
hcHSMIPAddress = lens _hcHSMIPAddress (\s a -> s {_hcHSMIPAddress = a})

instance FromXML HSMConfiguration where
  parseXML x =
    HSMConfiguration'
      <$> (x .@? "HsmConfigurationIdentifier")
      <*> (x .@? "HsmPartitionName")
      <*> (x .@? "Description")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))
      <*> (x .@? "HsmIpAddress")

instance Hashable HSMConfiguration

instance NFData HSMConfiguration
