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
-- Module      : Network.AWS.SES.GetSendQuota
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the user\'s current sending limits.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.GetSendQuota
    (
    -- * Creating a Request
      getSendQuota
    , GetSendQuota

    -- * Destructuring the Response
    , getSendQuotaResponse
    , GetSendQuotaResponse
    -- * Response Lenses
    , gsqrsMaxSendRate
    , gsqrsSentLast24Hours
    , gsqrsMax24HourSend
    , gsqrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'getSendQuota' smart constructor.
data GetSendQuota =
    GetSendQuota'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSendQuota' with the minimum fields required to make a request.
--
getSendQuota
    :: GetSendQuota
getSendQuota = GetSendQuota'

instance AWSRequest GetSendQuota where
        type Rs GetSendQuota = GetSendQuotaResponse
        request = postQuery sES
        response
          = receiveXMLWrapper "GetSendQuotaResult"
              (\ s h x ->
                 GetSendQuotaResponse' <$>
                   (x .@? "MaxSendRate") <*> (x .@? "SentLast24Hours")
                     <*> (x .@? "Max24HourSend")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetSendQuota where
        toHeaders = const mempty

instance ToPath GetSendQuota where
        toPath = const "/"

instance ToQuery GetSendQuota where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetSendQuota" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | /See:/ 'getSendQuotaResponse' smart constructor.
data GetSendQuotaResponse = GetSendQuotaResponse'
    { _gsqrsMaxSendRate     :: !(Maybe Double)
    , _gsqrsSentLast24Hours :: !(Maybe Double)
    , _gsqrsMax24HourSend   :: !(Maybe Double)
    , _gsqrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSendQuotaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqrsMaxSendRate'
--
-- * 'gsqrsSentLast24Hours'
--
-- * 'gsqrsMax24HourSend'
--
-- * 'gsqrsResponseStatus'
getSendQuotaResponse
    :: Int -- ^ 'gsqrsResponseStatus'
    -> GetSendQuotaResponse
getSendQuotaResponse pResponseStatus_ =
    GetSendQuotaResponse'
    { _gsqrsMaxSendRate = Nothing
    , _gsqrsSentLast24Hours = Nothing
    , _gsqrsMax24HourSend = Nothing
    , _gsqrsResponseStatus = pResponseStatus_
    }

-- | The maximum number of emails that Amazon SES can accept from the user\'s
-- account per second.
--
-- The rate at which Amazon SES accepts the user\'s messages might be less
-- than the maximum send rate.
gsqrsMaxSendRate :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrsMaxSendRate = lens _gsqrsMaxSendRate (\ s a -> s{_gsqrsMaxSendRate = a});

-- | The number of emails sent during the previous 24 hours.
gsqrsSentLast24Hours :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrsSentLast24Hours = lens _gsqrsSentLast24Hours (\ s a -> s{_gsqrsSentLast24Hours = a});

-- | The maximum number of emails the user is allowed to send in a 24-hour
-- interval. A value of -1 signifies an unlimited quota.
gsqrsMax24HourSend :: Lens' GetSendQuotaResponse (Maybe Double)
gsqrsMax24HourSend = lens _gsqrsMax24HourSend (\ s a -> s{_gsqrsMax24HourSend = a});

-- | The response status code.
gsqrsResponseStatus :: Lens' GetSendQuotaResponse Int
gsqrsResponseStatus = lens _gsqrsResponseStatus (\ s a -> s{_gsqrsResponseStatus = a});
