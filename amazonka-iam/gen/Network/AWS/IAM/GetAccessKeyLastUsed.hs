{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about when the specified access key was last used.
-- The information includes the date and time of last use, along with the
-- AWS service and region that were specified in the last request made with
-- that key.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccessKeyLastUsed.html>
module Network.AWS.IAM.GetAccessKeyLastUsed
    (
    -- * Request
      GetAccessKeyLastUsed
    -- ** Request constructor
    , getAccessKeyLastUsed
    -- ** Request lenses
    , gakluAccessKeyId

    -- * Response
    , GetAccessKeyLastUsedResponse
    -- ** Response constructor
    , getAccessKeyLastUsedResponse
    -- ** Response lenses
    , gaklurUserName
    , gaklurAccessKeyLastUsed
    , gaklurStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAccessKeyLastUsed' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gakluAccessKeyId'
newtype GetAccessKeyLastUsed = GetAccessKeyLastUsed'
    { _gakluAccessKeyId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetAccessKeyLastUsed' smart constructor.
getAccessKeyLastUsed :: Text -> GetAccessKeyLastUsed
getAccessKeyLastUsed pAccessKeyId =
    GetAccessKeyLastUsed'
    { _gakluAccessKeyId = pAccessKeyId
    }

-- | The identifier of an access key.
gakluAccessKeyId :: Lens' GetAccessKeyLastUsed Text
gakluAccessKeyId = lens _gakluAccessKeyId (\ s a -> s{_gakluAccessKeyId = a});

instance AWSRequest GetAccessKeyLastUsed where
        type Sv GetAccessKeyLastUsed = IAM
        type Rs GetAccessKeyLastUsed =
             GetAccessKeyLastUsedResponse
        request = post
        response
          = receiveXMLWrapper "GetAccessKeyLastUsedResult"
              (\ s h x ->
                 GetAccessKeyLastUsedResponse' <$>
                   (x .@? "UserName") <*> (x .@? "AccessKeyLastUsed")
                     <*> (pure s))

instance ToHeaders GetAccessKeyLastUsed where
        toHeaders = const mempty

instance ToPath GetAccessKeyLastUsed where
        toPath = const "/"

instance ToQuery GetAccessKeyLastUsed where
        toQuery GetAccessKeyLastUsed'{..}
          = mconcat
              ["Action" =: ("GetAccessKeyLastUsed" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AccessKeyId" =: _gakluAccessKeyId]

-- | Contains the response to a successful GetAccessKeyLastUsed request. It
-- is also returned as a member of the AccessKeyMetaData structure returned
-- by the ListAccessKeys action.
--
-- /See:/ 'getAccessKeyLastUsedResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaklurUserName'
--
-- * 'gaklurAccessKeyLastUsed'
--
-- * 'gaklurStatus'
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
    { _gaklurUserName          :: !(Maybe Text)
    , _gaklurAccessKeyLastUsed :: !(Maybe AccessKeyLastUsed)
    , _gaklurStatus            :: !Status
    } deriving (Eq,Show)

-- | 'GetAccessKeyLastUsedResponse' smart constructor.
getAccessKeyLastUsedResponse :: Status -> GetAccessKeyLastUsedResponse
getAccessKeyLastUsedResponse pStatus =
    GetAccessKeyLastUsedResponse'
    { _gaklurUserName = Nothing
    , _gaklurAccessKeyLastUsed = Nothing
    , _gaklurStatus = pStatus
    }

-- | The name of the AWS IAM user that owns this access key.
gaklurUserName :: Lens' GetAccessKeyLastUsedResponse (Maybe Text)
gaklurUserName = lens _gaklurUserName (\ s a -> s{_gaklurUserName = a});

-- | Contains information about the last time the access key was used.
gaklurAccessKeyLastUsed :: Lens' GetAccessKeyLastUsedResponse (Maybe AccessKeyLastUsed)
gaklurAccessKeyLastUsed = lens _gaklurAccessKeyLastUsed (\ s a -> s{_gaklurAccessKeyLastUsed = a});

-- | FIXME: Undocumented member.
gaklurStatus :: Lens' GetAccessKeyLastUsedResponse Status
gaklurStatus = lens _gaklurStatus (\ s a -> s{_gaklurStatus = a});
