{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Lambda.GetPolicy
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

-- | Returns the access policy, containing a list of permissions granted via
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
    , gprPolicy
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Lambda.Types

-- | /See:/ 'getPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpFunctionName'
newtype GetPolicy = GetPolicy'{_gpFunctionName :: Text} deriving (Eq, Read, Show)

-- | 'GetPolicy' smart constructor.
getPolicy :: Text -> GetPolicy
getPolicy pFunctionName = GetPolicy'{_gpFunctionName = pFunctionName};

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
              (\ s h x -> GetPolicyResponse' <$> (x .?> "Policy"))

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
-- * 'gprPolicy'
newtype GetPolicyResponse = GetPolicyResponse'{_gprPolicy :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetPolicyResponse' smart constructor.
getPolicyResponse :: GetPolicyResponse
getPolicyResponse = GetPolicyResponse'{_gprPolicy = Nothing};

-- | The access policy associated with the specified function. The response
-- returns the same as a string using \"\\\" as an escape character in the
-- JSON.
gprPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprPolicy = lens _gprPolicy (\ s a -> s{_gprPolicy = a});
