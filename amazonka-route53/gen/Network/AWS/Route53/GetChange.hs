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
-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns the current status of a change batch request. The status is one of the following values:
--
-- - 'PENDING' indicates that the changes in this request have not replicated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.
--
-- - 'INSYNC' indicates that the changes have replicated to all Amazon Route 53 DNS servers.
module Network.AWS.Route53.GetChange
    (
    -- * Creating a Request
      getChange
    , GetChange
    -- * Request Lenses
    , gcId

    -- * Destructuring the Response
    , getChangeResponse
    , GetChangeResponse
    -- * Response Lenses
    , gcrsResponseStatus
    , gcrsChangeInfo
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a GetChange request.
--
-- /See:/ 'getChange' smart constructor.
newtype GetChange = GetChange'
    { _gcId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcId'
getChange
    :: Text -- ^ 'gcId'
    -> GetChange
getChange pId_ =
    GetChange'
    { _gcId = pId_
    }

-- | The ID of the change batch request. The value that you specify here is the value that 'ChangeResourceRecordSets' returned in the Id element when you submitted the request.
gcId :: Lens' GetChange Text
gcId = lens _gcId (\ s a -> s{_gcId = a});

instance AWSRequest GetChange where
        type Rs GetChange = GetChangeResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetChangeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance Hashable GetChange

instance NFData GetChange

instance ToHeaders GetChange where
        toHeaders = const mempty

instance ToPath GetChange where
        toPath GetChange'{..}
          = mconcat ["/2013-04-01/change/", toBS _gcId]

instance ToQuery GetChange where
        toQuery = const mempty

-- | A complex type that contains the 'ChangeInfo' element.
--
-- /See:/ 'getChangeResponse' smart constructor.
data GetChangeResponse = GetChangeResponse'
    { _gcrsResponseStatus :: !Int
    , _gcrsChangeInfo     :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetChangeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsResponseStatus'
--
-- * 'gcrsChangeInfo'
getChangeResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> ChangeInfo -- ^ 'gcrsChangeInfo'
    -> GetChangeResponse
getChangeResponse pResponseStatus_ pChangeInfo_ =
    GetChangeResponse'
    { _gcrsResponseStatus = pResponseStatus_
    , _gcrsChangeInfo = pChangeInfo_
    }

-- | The response status code.
gcrsResponseStatus :: Lens' GetChangeResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a});

-- | A complex type that contains information about the specified change batch, including the change batch ID, the status of the change, and the date and time of the request.
gcrsChangeInfo :: Lens' GetChangeResponse ChangeInfo
gcrsChangeInfo = lens _gcrsChangeInfo (\ s a -> s{_gcrsChangeInfo = a});

instance NFData GetChangeResponse
