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
-- Module      : Network.AWS.SES.UpdateAccountSendingEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending across your entire Amazon SES account. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending across your Amazon SES account when reputation metrics (such as your bounce on complaint rate) reach certain thresholds.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.UpdateAccountSendingEnabled
    (
    -- * Creating a Request
      updateAccountSendingEnabled
    , UpdateAccountSendingEnabled
    -- * Request Lenses
    , uaseEnabled

    -- * Destructuring the Response
    , updateAccountSendingEnabledResponse
    , UpdateAccountSendingEnabledResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to enable or disable the email sending capabilities for your entire Amazon SES account.
--
--
--
-- /See:/ 'updateAccountSendingEnabled' smart constructor.
newtype UpdateAccountSendingEnabled = UpdateAccountSendingEnabled'
  { _uaseEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccountSendingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaseEnabled' - Describes whether email sending is enabled or disabled for your Amazon SES account.
updateAccountSendingEnabled
    :: UpdateAccountSendingEnabled
updateAccountSendingEnabled =
  UpdateAccountSendingEnabled' {_uaseEnabled = Nothing}


-- | Describes whether email sending is enabled or disabled for your Amazon SES account.
uaseEnabled :: Lens' UpdateAccountSendingEnabled (Maybe Bool)
uaseEnabled = lens _uaseEnabled (\ s a -> s{_uaseEnabled = a})

instance AWSRequest UpdateAccountSendingEnabled where
        type Rs UpdateAccountSendingEnabled =
             UpdateAccountSendingEnabledResponse
        request = postQuery ses
        response
          = receiveNull UpdateAccountSendingEnabledResponse'

instance Hashable UpdateAccountSendingEnabled where

instance NFData UpdateAccountSendingEnabled where

instance ToHeaders UpdateAccountSendingEnabled where
        toHeaders = const mempty

instance ToPath UpdateAccountSendingEnabled where
        toPath = const "/"

instance ToQuery UpdateAccountSendingEnabled where
        toQuery UpdateAccountSendingEnabled'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAccountSendingEnabled" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Enabled" =: _uaseEnabled]

-- | /See:/ 'updateAccountSendingEnabledResponse' smart constructor.
data UpdateAccountSendingEnabledResponse =
  UpdateAccountSendingEnabledResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccountSendingEnabledResponse' with the minimum fields required to make a request.
--
updateAccountSendingEnabledResponse
    :: UpdateAccountSendingEnabledResponse
updateAccountSendingEnabledResponse = UpdateAccountSendingEnabledResponse'


instance NFData UpdateAccountSendingEnabledResponse
         where
