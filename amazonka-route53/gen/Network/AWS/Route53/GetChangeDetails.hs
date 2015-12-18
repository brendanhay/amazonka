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
-- Module      : Network.AWS.Route53.GetChangeDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns the status and changes of a change batch request.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChangeDetails.html AWS API Reference> for GetChangeDetails.
module Network.AWS.Route53.GetChangeDetails
    (
    -- * Creating a Request
      getChangeDetails
    , GetChangeDetails
    -- * Request Lenses
    , gcdId

    -- * Destructuring the Response
    , getChangeDetailsResponse
    , GetChangeDetailsResponse
    -- * Response Lenses
    , gcdrsResponseStatus
    , gcdrsChangeBatchRecord
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a GetChangeDetails request.
--
-- /See:/ 'getChangeDetails' smart constructor.
newtype GetChangeDetails = GetChangeDetails'
    { _gcdId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetChangeDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdId'
getChangeDetails
    :: Text -- ^ 'gcdId'
    -> GetChangeDetails
getChangeDetails pId_ =
    GetChangeDetails'
    { _gcdId = pId_
    }

-- | The ID of the change batch request. The value that you specify here is
-- the value that 'ChangeResourceRecordSets' returned in the Id element
-- when you submitted the request.
gcdId :: Lens' GetChangeDetails Text
gcdId = lens _gcdId (\ s a -> s{_gcdId = a});

instance AWSRequest GetChangeDetails where
        type Rs GetChangeDetails = GetChangeDetailsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetChangeDetailsResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeBatchRecord"))

instance ToHeaders GetChangeDetails where
        toHeaders = const mempty

instance ToPath GetChangeDetails where
        toPath GetChangeDetails'{..}
          = mconcat ["/2013-04-01/changedetails/", toBS _gcdId]

instance ToQuery GetChangeDetails where
        toQuery = const mempty

-- | A complex type that contains the 'ChangeBatchRecord' element.
--
-- /See:/ 'getChangeDetailsResponse' smart constructor.
data GetChangeDetailsResponse = GetChangeDetailsResponse'
    { _gcdrsResponseStatus    :: !Int
    , _gcdrsChangeBatchRecord :: !ChangeBatchRecord
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetChangeDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdrsResponseStatus'
--
-- * 'gcdrsChangeBatchRecord'
getChangeDetailsResponse
    :: Int -- ^ 'gcdrsResponseStatus'
    -> ChangeBatchRecord -- ^ 'gcdrsChangeBatchRecord'
    -> GetChangeDetailsResponse
getChangeDetailsResponse pResponseStatus_ pChangeBatchRecord_ =
    GetChangeDetailsResponse'
    { _gcdrsResponseStatus = pResponseStatus_
    , _gcdrsChangeBatchRecord = pChangeBatchRecord_
    }

-- | The response status code.
gcdrsResponseStatus :: Lens' GetChangeDetailsResponse Int
gcdrsResponseStatus = lens _gcdrsResponseStatus (\ s a -> s{_gcdrsResponseStatus = a});

-- | A complex type that contains information about the specified change
-- batch, including the change batch ID, the status of the change, and the
-- contained changes.
gcdrsChangeBatchRecord :: Lens' GetChangeDetailsResponse ChangeBatchRecord
gcdrsChangeBatchRecord = lens _gcdrsChangeBatchRecord (\ s a -> s{_gcdrsChangeBatchRecord = a});
