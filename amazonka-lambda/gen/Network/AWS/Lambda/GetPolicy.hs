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
-- Returns the resource policy, containing a list of permissions that apply
-- to a specific to an ARN that you specify via the 'Qualifier' paramter.
--
-- For informration about adding permissions, see AddPermission.
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
    , gpQualifier
    , gpFunctionName

    -- * Destructuring the Response
    , getPolicyResponse
    , GetPolicyResponse
    -- * Response Lenses
    , gprsPolicy
    , gprsResponseStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
data GetPolicy = GetPolicy'
    { _gpQualifier    :: !(Maybe Text)
    , _gpFunctionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpQualifier'
--
-- * 'gpFunctionName'
getPolicy
    :: Text -- ^ 'gpFunctionName'
    -> GetPolicy
getPolicy pFunctionName_ =
    GetPolicy'
    { _gpQualifier = Nothing
    , _gpFunctionName = pFunctionName_
    }

-- | You can specify this optional query parameter to specify function
-- version or alias name in which case this API will return all permissions
-- associated with the specific ARN. If you don\'t provide this parameter,
-- the API will return permissions that apply to the unqualified function
-- ARN.
gpQualifier :: Lens' GetPolicy (Maybe Text)
gpQualifier = lens _gpQualifier (\ s a -> s{_gpQualifier = a});

-- | Function name whose resource policy you want to retrieve.
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
               "/policy"]

instance ToQuery GetPolicy where
        toQuery = const mempty

-- | /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
    { _gprsPolicy         :: !(Maybe Text)
    , _gprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicy'
--
-- * 'gprsResponseStatus'
getPolicyResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPolicyResponse
getPolicyResponse pResponseStatus_ =
    GetPolicyResponse'
    { _gprsPolicy = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }

-- | The resource policy associated with the specified function. The response
-- returns the same as a string using \"\\\" as an escape character in the
-- JSON.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a});

-- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a});
