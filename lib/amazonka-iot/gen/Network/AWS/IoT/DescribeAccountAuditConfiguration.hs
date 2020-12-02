{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.DescribeAccountAuditConfiguration
  ( -- * Creating a Request
    describeAccountAuditConfiguration,
    DescribeAccountAuditConfiguration,

    -- * Destructuring the Response
    describeAccountAuditConfigurationResponse,
    DescribeAccountAuditConfigurationResponse,

    -- * Response Lenses
    daacrsAuditCheckConfigurations,
    daacrsAuditNotificationTargetConfigurations,
    daacrsRoleARN,
    daacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAccountAuditConfiguration' smart constructor.
data DescribeAccountAuditConfiguration = DescribeAccountAuditConfiguration'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccountAuditConfiguration' with the minimum fields required to make a request.
describeAccountAuditConfiguration ::
  DescribeAccountAuditConfiguration
describeAccountAuditConfiguration =
  DescribeAccountAuditConfiguration'

instance AWSRequest DescribeAccountAuditConfiguration where
  type
    Rs DescribeAccountAuditConfiguration =
      DescribeAccountAuditConfigurationResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeAccountAuditConfigurationResponse'
            <$> (x .?> "auditCheckConfigurations" .!@ mempty)
            <*> (x .?> "auditNotificationTargetConfigurations" .!@ mempty)
            <*> (x .?> "roleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAccountAuditConfiguration

instance NFData DescribeAccountAuditConfiguration

instance ToHeaders DescribeAccountAuditConfiguration where
  toHeaders = const mempty

instance ToPath DescribeAccountAuditConfiguration where
  toPath = const "/audit/configuration"

instance ToQuery DescribeAccountAuditConfiguration where
  toQuery = const mempty

-- | /See:/ 'describeAccountAuditConfigurationResponse' smart constructor.
data DescribeAccountAuditConfigurationResponse = DescribeAccountAuditConfigurationResponse'
  { _daacrsAuditCheckConfigurations ::
      !( Maybe
           ( Map
               Text
               (AuditCheckConfiguration)
           )
       ),
    _daacrsAuditNotificationTargetConfigurations ::
      !( Maybe
           ( Map
               AuditNotificationType
               (AuditNotificationTarget)
           )
       ),
    _daacrsRoleARN ::
      !( Maybe
           Text
       ),
    _daacrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daacrsAuditCheckConfigurations' - Which audit checks are enabled and disabled for this account.
--
-- * 'daacrsAuditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent for this account.
--
-- * 'daacrsRoleARN' - The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit. On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
--
-- * 'daacrsResponseStatus' - -- | The response status code.
describeAccountAuditConfigurationResponse ::
  -- | 'daacrsResponseStatus'
  Int ->
  DescribeAccountAuditConfigurationResponse
describeAccountAuditConfigurationResponse pResponseStatus_ =
  DescribeAccountAuditConfigurationResponse'
    { _daacrsAuditCheckConfigurations =
        Nothing,
      _daacrsAuditNotificationTargetConfigurations =
        Nothing,
      _daacrsRoleARN = Nothing,
      _daacrsResponseStatus = pResponseStatus_
    }

-- | Which audit checks are enabled and disabled for this account.
daacrsAuditCheckConfigurations :: Lens' DescribeAccountAuditConfigurationResponse (HashMap Text (AuditCheckConfiguration))
daacrsAuditCheckConfigurations = lens _daacrsAuditCheckConfigurations (\s a -> s {_daacrsAuditCheckConfigurations = a}) . _Default . _Map

-- | Information about the targets to which audit notifications are sent for this account.
daacrsAuditNotificationTargetConfigurations :: Lens' DescribeAccountAuditConfigurationResponse (HashMap AuditNotificationType (AuditNotificationTarget))
daacrsAuditNotificationTargetConfigurations = lens _daacrsAuditNotificationTargetConfigurations (\s a -> s {_daacrsAuditNotificationTargetConfigurations = a}) . _Default . _Map

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates, and other items as required when performing an audit. On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required.
daacrsRoleARN :: Lens' DescribeAccountAuditConfigurationResponse (Maybe Text)
daacrsRoleARN = lens _daacrsRoleARN (\s a -> s {_daacrsRoleARN = a})

-- | -- | The response status code.
daacrsResponseStatus :: Lens' DescribeAccountAuditConfigurationResponse Int
daacrsResponseStatus = lens _daacrsResponseStatus (\s a -> s {_daacrsResponseStatus = a})

instance NFData DescribeAccountAuditConfigurationResponse
