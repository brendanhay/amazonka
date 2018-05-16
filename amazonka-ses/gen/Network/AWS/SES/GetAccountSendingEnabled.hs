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
-- Module      : Network.AWS.SES.GetAccountSendingEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the email sending status of the Amazon SES account.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.GetAccountSendingEnabled
    (
    -- * Creating a Request
      getAccountSendingEnabled
    , GetAccountSendingEnabled

    -- * Destructuring the Response
    , getAccountSendingEnabledResponse
    , GetAccountSendingEnabledResponse
    -- * Response Lenses
    , gasersEnabled
    , gasersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | /See:/ 'getAccountSendingEnabled' smart constructor.
data GetAccountSendingEnabled =
  GetAccountSendingEnabled'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSendingEnabled' with the minimum fields required to make a request.
--
getAccountSendingEnabled
    :: GetAccountSendingEnabled
getAccountSendingEnabled = GetAccountSendingEnabled'


instance AWSRequest GetAccountSendingEnabled where
        type Rs GetAccountSendingEnabled =
             GetAccountSendingEnabledResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "GetAccountSendingEnabledResult"
              (\ s h x ->
                 GetAccountSendingEnabledResponse' <$>
                   (x .@? "Enabled") <*> (pure (fromEnum s)))

instance Hashable GetAccountSendingEnabled where

instance NFData GetAccountSendingEnabled where

instance ToHeaders GetAccountSendingEnabled where
        toHeaders = const mempty

instance ToPath GetAccountSendingEnabled where
        toPath = const "/"

instance ToQuery GetAccountSendingEnabled where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("GetAccountSendingEnabled" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | Represents a request to return the email sending status for your Amazon SES account.
--
--
--
-- /See:/ 'getAccountSendingEnabledResponse' smart constructor.
data GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse'
  { _gasersEnabled        :: !(Maybe Bool)
  , _gasersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSendingEnabledResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasersEnabled' - Describes whether email sending is enabled or disabled for your Amazon SES account.
--
-- * 'gasersResponseStatus' - -- | The response status code.
getAccountSendingEnabledResponse
    :: Int -- ^ 'gasersResponseStatus'
    -> GetAccountSendingEnabledResponse
getAccountSendingEnabledResponse pResponseStatus_ =
  GetAccountSendingEnabledResponse'
    {_gasersEnabled = Nothing, _gasersResponseStatus = pResponseStatus_}


-- | Describes whether email sending is enabled or disabled for your Amazon SES account.
gasersEnabled :: Lens' GetAccountSendingEnabledResponse (Maybe Bool)
gasersEnabled = lens _gasersEnabled (\ s a -> s{_gasersEnabled = a})

-- | -- | The response status code.
gasersResponseStatus :: Lens' GetAccountSendingEnabledResponse Int
gasersResponseStatus = lens _gasersResponseStatus (\ s a -> s{_gasersResponseStatus = a})

instance NFData GetAccountSendingEnabledResponse
         where
