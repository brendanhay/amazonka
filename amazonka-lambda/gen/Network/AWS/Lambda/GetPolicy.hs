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
-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access policy, containing a list of permissions granted via
-- the 'AddPermission' API, associated with the specified bucket.
--
-- You need permission for the 'lambda:GetPolicy action.'
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_GetPolicy.html AWS API Reference> for GetPolicy.
module Network.AWS.Lambda.GetPolicy
    (
    -- * Creating a Request
      getPolicy
    , GetPolicy
    -- * Request Lenses
    , gpFunctionName

    -- * Destructuring the Response
    , getPolicyResponse
    , GetPolicyResponse
    -- * Response Lenses
    , gprsPolicy
    , gprsStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
    { _gpFunctionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpFunctionName'
getPolicy
    :: Text -- ^ 'gpFunctionName'
    -> GetPolicy
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
        type Rs GetPolicy = GetPolicyResponse
        request = get lambda
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
              ["/2015-03-31/functions/", toBS _gpFunctionName,
               "/versions/HEAD/policy"]

instance ToQuery GetPolicy where
        toQuery = const mempty

-- | /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
    { _gprsPolicy :: !(Maybe Text)
    , _gprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicy'
--
-- * 'gprsStatus'
getPolicyResponse
    :: Int -- ^ 'gprsStatus'
    -> GetPolicyResponse
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

-- | The response status code.
gprsStatus :: Lens' GetPolicyResponse Int
gprsStatus = lens _gprsStatus (\ s a -> s{_gprsStatus = a});
