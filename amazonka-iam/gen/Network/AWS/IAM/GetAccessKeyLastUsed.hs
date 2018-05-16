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
-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used. The information includes the date and time of last use, along with the AWS service and region that were specified in the last request made with that key.
--
--
module Network.AWS.IAM.GetAccessKeyLastUsed
    (
    -- * Creating a Request
      getAccessKeyLastUsed
    , GetAccessKeyLastUsed
    -- * Request Lenses
    , gakluAccessKeyId

    -- * Destructuring the Response
    , getAccessKeyLastUsedResponse
    , GetAccessKeyLastUsedResponse
    -- * Response Lenses
    , gaklursUserName
    , gaklursAccessKeyLastUsed
    , gaklursResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccessKeyLastUsed' smart constructor.
newtype GetAccessKeyLastUsed = GetAccessKeyLastUsed'
  { _gakluAccessKeyId :: AccessKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccessKeyLastUsed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gakluAccessKeyId' - The identifier of an access key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
getAccessKeyLastUsed
    :: AccessKey -- ^ 'gakluAccessKeyId'
    -> GetAccessKeyLastUsed
getAccessKeyLastUsed pAccessKeyId_ =
  GetAccessKeyLastUsed' {_gakluAccessKeyId = pAccessKeyId_}


-- | The identifier of an access key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
gakluAccessKeyId :: Lens' GetAccessKeyLastUsed AccessKey
gakluAccessKeyId = lens _gakluAccessKeyId (\ s a -> s{_gakluAccessKeyId = a})

instance AWSRequest GetAccessKeyLastUsed where
        type Rs GetAccessKeyLastUsed =
             GetAccessKeyLastUsedResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetAccessKeyLastUsedResult"
              (\ s h x ->
                 GetAccessKeyLastUsedResponse' <$>
                   (x .@? "UserName") <*> (x .@? "AccessKeyLastUsed")
                     <*> (pure (fromEnum s)))

instance Hashable GetAccessKeyLastUsed where

instance NFData GetAccessKeyLastUsed where

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

-- | Contains the response to a successful 'GetAccessKeyLastUsed' request. It is also returned as a member of the 'AccessKeyMetaData' structure returned by the 'ListAccessKeys' action.
--
--
--
-- /See:/ 'getAccessKeyLastUsedResponse' smart constructor.
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
  { _gaklursUserName          :: !(Maybe Text)
  , _gaklursAccessKeyLastUsed :: !(Maybe AccessKeyLastUsed)
  , _gaklursResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccessKeyLastUsedResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaklursUserName' - The name of the AWS IAM user that owns this access key.
--
-- * 'gaklursAccessKeyLastUsed' - Contains information about the last time the access key was used.
--
-- * 'gaklursResponseStatus' - -- | The response status code.
getAccessKeyLastUsedResponse
    :: Int -- ^ 'gaklursResponseStatus'
    -> GetAccessKeyLastUsedResponse
getAccessKeyLastUsedResponse pResponseStatus_ =
  GetAccessKeyLastUsedResponse'
    { _gaklursUserName = Nothing
    , _gaklursAccessKeyLastUsed = Nothing
    , _gaklursResponseStatus = pResponseStatus_
    }


-- | The name of the AWS IAM user that owns this access key.
gaklursUserName :: Lens' GetAccessKeyLastUsedResponse (Maybe Text)
gaklursUserName = lens _gaklursUserName (\ s a -> s{_gaklursUserName = a})

-- | Contains information about the last time the access key was used.
gaklursAccessKeyLastUsed :: Lens' GetAccessKeyLastUsedResponse (Maybe AccessKeyLastUsed)
gaklursAccessKeyLastUsed = lens _gaklursAccessKeyLastUsed (\ s a -> s{_gaklursAccessKeyLastUsed = a})

-- | -- | The response status code.
gaklursResponseStatus :: Lens' GetAccessKeyLastUsedResponse Int
gaklursResponseStatus = lens _gaklursResponseStatus (\ s a -> s{_gaklursResponseStatus = a})

instance NFData GetAccessKeyLastUsedResponse where
