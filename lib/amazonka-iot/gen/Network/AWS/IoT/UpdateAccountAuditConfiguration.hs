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
-- Module      : Network.AWS.IoT.UpdateAccountAuditConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures or reconfigures the Device Defender audit settings for this account. Settings include how audit notifications are sent and which audit checks are enabled or disabled.
module Network.AWS.IoT.UpdateAccountAuditConfiguration
  ( -- * Creating a Request
    updateAccountAuditConfiguration,
    UpdateAccountAuditConfiguration,

    -- * Request Lenses
    uaacAuditCheckConfigurations,
    uaacAuditNotificationTargetConfigurations,
    uaacRoleARN,

    -- * Destructuring the Response
    updateAccountAuditConfigurationResponse,
    UpdateAccountAuditConfigurationResponse,

    -- * Response Lenses
    uaacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAccountAuditConfiguration' smart constructor.
data UpdateAccountAuditConfiguration = UpdateAccountAuditConfiguration'
  { _uaacAuditCheckConfigurations ::
      !( Maybe
           ( Map
               Text
               (AuditCheckConfiguration)
           )
       ),
    _uaacAuditNotificationTargetConfigurations ::
      !( Maybe
           ( Map
               AuditNotificationType
               (AuditNotificationTarget)
           )
       ),
    _uaacRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAccountAuditConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaacAuditCheckConfigurations' - Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled. Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted. You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself. On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
--
-- * 'uaacAuditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent.
--
-- * 'uaacRoleARN' - The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
updateAccountAuditConfiguration ::
  UpdateAccountAuditConfiguration
updateAccountAuditConfiguration =
  UpdateAccountAuditConfiguration'
    { _uaacAuditCheckConfigurations =
        Nothing,
      _uaacAuditNotificationTargetConfigurations = Nothing,
      _uaacRoleARN = Nothing
    }

-- | Specifies which audit checks are enabled and disabled for this account. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are currently enabled. Some data collection might start immediately when certain checks are enabled. When a check is disabled, any data collected so far in relation to the check is deleted. You cannot disable a check if it is used by any scheduled audit. You must first delete the check from the scheduled audit or delete the scheduled audit itself. On the first call to @UpdateAccountAuditConfiguration@ , this parameter is required and must specify at least one enabled check.
uaacAuditCheckConfigurations :: Lens' UpdateAccountAuditConfiguration (HashMap Text (AuditCheckConfiguration))
uaacAuditCheckConfigurations = lens _uaacAuditCheckConfigurations (\s a -> s {_uaacAuditCheckConfigurations = a}) . _Default . _Map

-- | Information about the targets to which audit notifications are sent.
uaacAuditNotificationTargetConfigurations :: Lens' UpdateAccountAuditConfiguration (HashMap AuditNotificationType (AuditNotificationTarget))
uaacAuditNotificationTargetConfigurations = lens _uaacAuditNotificationTargetConfigurations (\s a -> s {_uaacAuditNotificationTargetConfigurations = a}) . _Default . _Map

-- | The ARN of the role that grants permission to AWS IoT to access information about your devices, policies, certificates and other items as required when performing an audit.
uaacRoleARN :: Lens' UpdateAccountAuditConfiguration (Maybe Text)
uaacRoleARN = lens _uaacRoleARN (\s a -> s {_uaacRoleARN = a})

instance AWSRequest UpdateAccountAuditConfiguration where
  type
    Rs UpdateAccountAuditConfiguration =
      UpdateAccountAuditConfigurationResponse
  request = patchJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          UpdateAccountAuditConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateAccountAuditConfiguration

instance NFData UpdateAccountAuditConfiguration

instance ToHeaders UpdateAccountAuditConfiguration where
  toHeaders = const mempty

instance ToJSON UpdateAccountAuditConfiguration where
  toJSON UpdateAccountAuditConfiguration' {..} =
    object
      ( catMaybes
          [ ("auditCheckConfigurations" .=) <$> _uaacAuditCheckConfigurations,
            ("auditNotificationTargetConfigurations" .=)
              <$> _uaacAuditNotificationTargetConfigurations,
            ("roleArn" .=) <$> _uaacRoleARN
          ]
      )

instance ToPath UpdateAccountAuditConfiguration where
  toPath = const "/audit/configuration"

instance ToQuery UpdateAccountAuditConfiguration where
  toQuery = const mempty

-- | /See:/ 'updateAccountAuditConfigurationResponse' smart constructor.
newtype UpdateAccountAuditConfigurationResponse = UpdateAccountAuditConfigurationResponse'
  { _uaacrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateAccountAuditConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaacrsResponseStatus' - -- | The response status code.
updateAccountAuditConfigurationResponse ::
  -- | 'uaacrsResponseStatus'
  Int ->
  UpdateAccountAuditConfigurationResponse
updateAccountAuditConfigurationResponse pResponseStatus_ =
  UpdateAccountAuditConfigurationResponse'
    { _uaacrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uaacrsResponseStatus :: Lens' UpdateAccountAuditConfigurationResponse Int
uaacrsResponseStatus = lens _uaacrsResponseStatus (\s a -> s {_uaacrsResponseStatus = a})

instance NFData UpdateAccountAuditConfigurationResponse
