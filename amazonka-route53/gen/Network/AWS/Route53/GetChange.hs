{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action returns the current status of a change batch request. The
-- status is one of the following values:
--
-- - @PENDING@ indicates that the changes in this request have not
-- replicated to all Route 53 DNS servers. This is the initial status of
-- all change batch requests.
--
-- - @INSYNC@ indicates that the changes have replicated to all Amazon
-- Route 53 DNS servers.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html AWS API Reference> for GetChange.
module Network.AWS.Route53.GetChange
    (
    -- * Creating a Request
      GetChange
    , getChange
    -- * Request Lenses
    , gcId

    -- * Destructuring the Response
    , GetChangeResponse
    , getChangeResponse
    -- * Response Lenses
    , gcrsStatus
    , gcrsChangeInfo
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | The input for a GetChange request.
--
-- /See:/ 'getChange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcId'
newtype GetChange = GetChange'
    { _gcId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetChange' smart constructor.
getChange :: Text -> GetChange
getChange pId_ =
    GetChange'
    { _gcId = pId_
    }

-- | The ID of the change batch request. The value that you specify here is
-- the value that @ChangeResourceRecordSets@ returned in the Id element
-- when you submitted the request.
gcId :: Lens' GetChange Text
gcId = lens _gcId (\ s a -> s{_gcId = a});

instance AWSRequest GetChange where
        type Sv GetChange = Route53
        type Rs GetChange = GetChangeResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetChangeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance ToHeaders GetChange where
        toHeaders = const mempty

instance ToPath GetChange where
        toPath GetChange'{..}
          = mconcat ["/2013-04-01/change/", toBS _gcId]

instance ToQuery GetChange where
        toQuery = const mempty

-- | A complex type that contains the @ChangeInfo@ element.
--
-- /See:/ 'getChangeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcrsStatus'
--
-- * 'gcrsChangeInfo'
data GetChangeResponse = GetChangeResponse'
    { _gcrsStatus     :: !Int
    , _gcrsChangeInfo :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetChangeResponse' smart constructor.
getChangeResponse :: Int -> ChangeInfo -> GetChangeResponse
getChangeResponse pStatus_ pChangeInfo_ =
    GetChangeResponse'
    { _gcrsStatus = pStatus_
    , _gcrsChangeInfo = pChangeInfo_
    }

-- | Undocumented member.
gcrsStatus :: Lens' GetChangeResponse Int
gcrsStatus = lens _gcrsStatus (\ s a -> s{_gcrsStatus = a});

-- | A complex type that contains information about the specified change
-- batch, including the change batch ID, the status of the change, and the
-- date and time of the request.
gcrsChangeInfo :: Lens' GetChangeResponse ChangeInfo
gcrsChangeInfo = lens _gcrsChangeInfo (\ s a -> s{_gcrsChangeInfo = a});
