{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used.
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
    , gaklursUserName
    , gaklursAccessKeyLastUsed
    , gaklursStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAccessKeyLastUsed' smart constructor.
getAccessKeyLastUsed :: Text -> GetAccessKeyLastUsed
getAccessKeyLastUsed pAccessKeyId_ =
    GetAccessKeyLastUsed'
    { _gakluAccessKeyId = pAccessKeyId_
    }

-- | The identifier of an access key.
gakluAccessKeyId :: Lens' GetAccessKeyLastUsed Text
gakluAccessKeyId = lens _gakluAccessKeyId (\ s a -> s{_gakluAccessKeyId = a});

instance AWSRequest GetAccessKeyLastUsed where
        type Sv GetAccessKeyLastUsed = IAM
        type Rs GetAccessKeyLastUsed =
             GetAccessKeyLastUsedResponse
        request = post "GetAccessKeyLastUsed"
        response
          = receiveXMLWrapper "GetAccessKeyLastUsedResult"
              (\ s h x ->
                 GetAccessKeyLastUsedResponse' <$>
                   (x .@? "UserName") <*> (x .@? "AccessKeyLastUsed")
                     <*> (pure (fromEnum s)))

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
-- * 'gaklursUserName'
--
-- * 'gaklursAccessKeyLastUsed'
--
-- * 'gaklursStatus'
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
    { _gaklursUserName          :: !(Maybe Text)
    , _gaklursAccessKeyLastUsed :: !(Maybe AccessKeyLastUsed)
    , _gaklursStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAccessKeyLastUsedResponse' smart constructor.
getAccessKeyLastUsedResponse :: Int -> GetAccessKeyLastUsedResponse
getAccessKeyLastUsedResponse pStatus_ =
    GetAccessKeyLastUsedResponse'
    { _gaklursUserName = Nothing
    , _gaklursAccessKeyLastUsed = Nothing
    , _gaklursStatus = pStatus_
    }

-- | The name of the AWS IAM user that owns this access key.
gaklursUserName :: Lens' GetAccessKeyLastUsedResponse (Maybe Text)
gaklursUserName = lens _gaklursUserName (\ s a -> s{_gaklursUserName = a});

-- | Contains information about the last time the access key was used.
gaklursAccessKeyLastUsed :: Lens' GetAccessKeyLastUsedResponse (Maybe AccessKeyLastUsed)
gaklursAccessKeyLastUsed = lens _gaklursAccessKeyLastUsed (\ s a -> s{_gaklursAccessKeyLastUsed = a});

-- | FIXME: Undocumented member.
gaklursStatus :: Lens' GetAccessKeyLastUsedResponse Int
gaklursStatus = lens _gaklursStatus (\ s a -> s{_gaklursStatus = a});
