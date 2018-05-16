{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetSendingEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending for messages sent using a specific configuration set. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) reach certain thresholds.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.UpdateConfigurationSetSendingEnabled
    (
    -- * Creating a Request
      updateConfigurationSetSendingEnabled
    , UpdateConfigurationSetSendingEnabled
    -- * Request Lenses
    , ucsseConfigurationSetName
    , ucsseEnabled

    -- * Destructuring the Response
    , updateConfigurationSetSendingEnabledResponse
    , UpdateConfigurationSetSendingEnabledResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to enable or disable the email sending capabilities for a specific configuration set.
--
--
--
-- /See:/ 'updateConfigurationSetSendingEnabled' smart constructor.
data UpdateConfigurationSetSendingEnabled = UpdateConfigurationSetSendingEnabled'
  { _ucsseConfigurationSetName :: !Text
  , _ucsseEnabled              :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetSendingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsseConfigurationSetName' - The name of the configuration set that you want to update.
--
-- * 'ucsseEnabled' - Describes whether email sending is enabled or disabled for the configuration set.
updateConfigurationSetSendingEnabled
    :: Text -- ^ 'ucsseConfigurationSetName'
    -> Bool -- ^ 'ucsseEnabled'
    -> UpdateConfigurationSetSendingEnabled
updateConfigurationSetSendingEnabled pConfigurationSetName_ pEnabled_ =
  UpdateConfigurationSetSendingEnabled'
    { _ucsseConfigurationSetName = pConfigurationSetName_
    , _ucsseEnabled = pEnabled_
    }


-- | The name of the configuration set that you want to update.
ucsseConfigurationSetName :: Lens' UpdateConfigurationSetSendingEnabled Text
ucsseConfigurationSetName = lens _ucsseConfigurationSetName (\ s a -> s{_ucsseConfigurationSetName = a})

-- | Describes whether email sending is enabled or disabled for the configuration set.
ucsseEnabled :: Lens' UpdateConfigurationSetSendingEnabled Bool
ucsseEnabled = lens _ucsseEnabled (\ s a -> s{_ucsseEnabled = a})

instance AWSRequest
           UpdateConfigurationSetSendingEnabled
         where
        type Rs UpdateConfigurationSetSendingEnabled =
             UpdateConfigurationSetSendingEnabledResponse
        request = postQuery ses
        response
          = receiveNull
              UpdateConfigurationSetSendingEnabledResponse'

instance Hashable
           UpdateConfigurationSetSendingEnabled
         where

instance NFData UpdateConfigurationSetSendingEnabled
         where

instance ToHeaders
           UpdateConfigurationSetSendingEnabled
         where
        toHeaders = const mempty

instance ToPath UpdateConfigurationSetSendingEnabled
         where
        toPath = const "/"

instance ToQuery UpdateConfigurationSetSendingEnabled
         where
        toQuery UpdateConfigurationSetSendingEnabled'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationSetSendingEnabled" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _ucsseConfigurationSetName,
               "Enabled" =: _ucsseEnabled]

-- | /See:/ 'updateConfigurationSetSendingEnabledResponse' smart constructor.
data UpdateConfigurationSetSendingEnabledResponse =
  UpdateConfigurationSetSendingEnabledResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetSendingEnabledResponse' with the minimum fields required to make a request.
--
updateConfigurationSetSendingEnabledResponse
    :: UpdateConfigurationSetSendingEnabledResponse
updateConfigurationSetSendingEnabledResponse =
  UpdateConfigurationSetSendingEnabledResponse'


instance NFData
           UpdateConfigurationSetSendingEnabledResponse
         where
