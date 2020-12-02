{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceEntry where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.PortInfoSourceType
import Network.AWS.Prelude

-- | Describes the Amazon Elastic Compute Cloud instance and related resources to be created using the @create cloud formation stack@ operation.
--
--
--
-- /See:/ 'instanceEntry' smart constructor.
data InstanceEntry = InstanceEntry'
  { _ieUserData :: !(Maybe Text),
    _ieSourceName :: !Text,
    _ieInstanceType :: !Text,
    _iePortInfoSource :: !PortInfoSourceType,
    _ieAvailabilityZone :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieUserData' - A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- * 'ieSourceName' - The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance. Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
--
-- * 'ieInstanceType' - The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
--
-- * 'iePortInfoSource' - The port configuration to use for the new Amazon EC2 instance. The following configuration options are available:     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.     * @NONE@ - Use the default Amazon EC2 security group.     * @CLOSED@ - All ports closed.
--
-- * 'ieAvailabilityZone' - The Availability Zone for the new Amazon EC2 instance.
instanceEntry ::
  -- | 'ieSourceName'
  Text ->
  -- | 'ieInstanceType'
  Text ->
  -- | 'iePortInfoSource'
  PortInfoSourceType ->
  -- | 'ieAvailabilityZone'
  Text ->
  InstanceEntry
instanceEntry
  pSourceName_
  pInstanceType_
  pPortInfoSource_
  pAvailabilityZone_ =
    InstanceEntry'
      { _ieUserData = Nothing,
        _ieSourceName = pSourceName_,
        _ieInstanceType = pInstanceType_,
        _iePortInfoSource = pPortInfoSource_,
        _ieAvailabilityZone = pAvailabilityZone_
      }

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
ieUserData :: Lens' InstanceEntry (Maybe Text)
ieUserData = lens _ieUserData (\s a -> s {_ieUserData = a})

-- | The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance. Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
ieSourceName :: Lens' InstanceEntry Text
ieSourceName = lens _ieSourceName (\s a -> s {_ieSourceName = a})

-- | The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
ieInstanceType :: Lens' InstanceEntry Text
ieInstanceType = lens _ieInstanceType (\s a -> s {_ieInstanceType = a})

-- | The port configuration to use for the new Amazon EC2 instance. The following configuration options are available:     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.     * @NONE@ - Use the default Amazon EC2 security group.     * @CLOSED@ - All ports closed.
iePortInfoSource :: Lens' InstanceEntry PortInfoSourceType
iePortInfoSource = lens _iePortInfoSource (\s a -> s {_iePortInfoSource = a})

-- | The Availability Zone for the new Amazon EC2 instance.
ieAvailabilityZone :: Lens' InstanceEntry Text
ieAvailabilityZone = lens _ieAvailabilityZone (\s a -> s {_ieAvailabilityZone = a})

instance Hashable InstanceEntry

instance NFData InstanceEntry

instance ToJSON InstanceEntry where
  toJSON InstanceEntry' {..} =
    object
      ( catMaybes
          [ ("userData" .=) <$> _ieUserData,
            Just ("sourceName" .= _ieSourceName),
            Just ("instanceType" .= _ieInstanceType),
            Just ("portInfoSource" .= _iePortInfoSource),
            Just ("availabilityZone" .= _ieAvailabilityZone)
          ]
      )
