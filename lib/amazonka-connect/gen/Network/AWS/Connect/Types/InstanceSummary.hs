{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceSummary where

import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.InstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the instance.
--
--
--
-- /See:/ 'instanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { _isARN :: !(Maybe Text),
    _isCreatedTime :: !(Maybe POSIX),
    _isOutboundCallsEnabled :: !(Maybe Bool),
    _isInboundCallsEnabled :: !(Maybe Bool),
    _isInstanceAlias :: !(Maybe (Sensitive Text)),
    _isId :: !(Maybe Text),
    _isInstanceStatus :: !(Maybe InstanceStatus),
    _isIdentityManagementType :: !(Maybe DirectoryType),
    _isServiceRole :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isARN' - The Amazon Resource Name (ARN) of the instance.
--
-- * 'isCreatedTime' - When the instance was created.
--
-- * 'isOutboundCallsEnabled' - Whether outbound calls are enabled.
--
-- * 'isInboundCallsEnabled' - Whether inbound calls are enabled.
--
-- * 'isInstanceAlias' - The alias of the instance.
--
-- * 'isId' - The identifier of the instance.
--
-- * 'isInstanceStatus' - The state of the instance.
--
-- * 'isIdentityManagementType' - The identity management type of the instance.
--
-- * 'isServiceRole' - The service role of the instance.
instanceSummary ::
  InstanceSummary
instanceSummary =
  InstanceSummary'
    { _isARN = Nothing,
      _isCreatedTime = Nothing,
      _isOutboundCallsEnabled = Nothing,
      _isInboundCallsEnabled = Nothing,
      _isInstanceAlias = Nothing,
      _isId = Nothing,
      _isInstanceStatus = Nothing,
      _isIdentityManagementType = Nothing,
      _isServiceRole = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
isARN :: Lens' InstanceSummary (Maybe Text)
isARN = lens _isARN (\s a -> s {_isARN = a})

-- | When the instance was created.
isCreatedTime :: Lens' InstanceSummary (Maybe UTCTime)
isCreatedTime = lens _isCreatedTime (\s a -> s {_isCreatedTime = a}) . mapping _Time

-- | Whether outbound calls are enabled.
isOutboundCallsEnabled :: Lens' InstanceSummary (Maybe Bool)
isOutboundCallsEnabled = lens _isOutboundCallsEnabled (\s a -> s {_isOutboundCallsEnabled = a})

-- | Whether inbound calls are enabled.
isInboundCallsEnabled :: Lens' InstanceSummary (Maybe Bool)
isInboundCallsEnabled = lens _isInboundCallsEnabled (\s a -> s {_isInboundCallsEnabled = a})

-- | The alias of the instance.
isInstanceAlias :: Lens' InstanceSummary (Maybe Text)
isInstanceAlias = lens _isInstanceAlias (\s a -> s {_isInstanceAlias = a}) . mapping _Sensitive

-- | The identifier of the instance.
isId :: Lens' InstanceSummary (Maybe Text)
isId = lens _isId (\s a -> s {_isId = a})

-- | The state of the instance.
isInstanceStatus :: Lens' InstanceSummary (Maybe InstanceStatus)
isInstanceStatus = lens _isInstanceStatus (\s a -> s {_isInstanceStatus = a})

-- | The identity management type of the instance.
isIdentityManagementType :: Lens' InstanceSummary (Maybe DirectoryType)
isIdentityManagementType = lens _isIdentityManagementType (\s a -> s {_isIdentityManagementType = a})

-- | The service role of the instance.
isServiceRole :: Lens' InstanceSummary (Maybe Text)
isServiceRole = lens _isServiceRole (\s a -> s {_isServiceRole = a})

instance FromJSON InstanceSummary where
  parseJSON =
    withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            <$> (x .:? "Arn")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "OutboundCallsEnabled")
            <*> (x .:? "InboundCallsEnabled")
            <*> (x .:? "InstanceAlias")
            <*> (x .:? "Id")
            <*> (x .:? "InstanceStatus")
            <*> (x .:? "IdentityManagementType")
            <*> (x .:? "ServiceRole")
      )

instance Hashable InstanceSummary

instance NFData InstanceSummary
