{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the access policy, containing a list of permissions granted via
-- the @AddPermission@ API, associated with the specified bucket.
--
-- You need permission for the @lambda:GetPolicy action.@
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetPolicy.html>
module Network.AWS.Lambda.GetPolicy
    (
    -- * Request
      GetPolicy
    -- ** Request constructor
    , getPolicy
    -- ** Request lenses
    , gpFunctionName

    -- * Response
    , GetPolicyResponse
    -- ** Response constructor
    , getPolicyResponse
    -- ** Response lenses
    , gprsPolicy
    , gprsStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpFunctionName'
newtype GetPolicy = GetPolicy'
    { _gpFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicy' smart constructor.
getPolicy :: Text -> GetPolicy
getPolicy pFunctionName_ =
    GetPolicy'
    { _gpFunctionName = pFunctionName_
    }

-- | Function name whose access policy you want to retrieve.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
gpFunctionName :: Lens' GetPolicy Text
gpFunctionName = lens _gpFunctionName (\ s a -> s{_gpFunctionName = a});

instance AWSRequest GetPolicy where
        type Sv GetPolicy = Lambda
        type Rs GetPolicy = GetPolicyResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance ToHeaders GetPolicy where
        toHeaders = const mempty

instance ToPath GetPolicy where
        toPath GetPolicy'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _gpFunctionName,
               "/versions/HEAD/policy"]

instance ToQuery GetPolicy where
        toQuery = const mempty

-- | /See:/ 'getPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprsPolicy'
--
-- * 'gprsStatus'
data GetPolicyResponse = GetPolicyResponse'
    { _gprsPolicy :: !(Maybe Text)
    , _gprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicyResponse' smart constructor.
getPolicyResponse :: Int -> GetPolicyResponse
getPolicyResponse pStatus_ =
    GetPolicyResponse'
    { _gprsPolicy = Nothing
    , _gprsStatus = pStatus_
    }

-- | The access policy associated with the specified function. The response
-- returns the same as a string using \"\\\" as an escape character in the
-- JSON.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a});

-- | FIXME: Undocumented member.
gprsStatus :: Lens' GetPolicyResponse Int
gprsStatus = lens _gprsStatus (\ s a -> s{_gprsStatus = a});
