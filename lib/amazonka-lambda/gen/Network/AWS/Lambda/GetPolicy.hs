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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource policy associated with the specified Lambda function.
--
--
-- If you are using the versioning feature, you can get the resource policy associated with the specific Lambda function version or alias by specifying the version or alias name using the @Qualifier@ parameter. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- You need permission for the @lambda:GetPolicy action.@
--
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
    , gprsRevisionId
    , gprsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'getPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { _gpQualifier    :: !(Maybe Text)
  , _gpFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpQualifier' - You can specify this optional query parameter to specify a function version or an alias name in which case this API will return all permissions associated with the specific qualified ARN. If you don't provide this parameter, the API will return permissions that apply to the unqualified function ARN.
--
-- * 'gpFunctionName' - Function name whose resource policy you want to retrieve. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
getPolicy
    :: Text -- ^ 'gpFunctionName'
    -> GetPolicy
getPolicy pFunctionName_ =
  GetPolicy' {_gpQualifier = Nothing, _gpFunctionName = pFunctionName_}


-- | You can specify this optional query parameter to specify a function version or an alias name in which case this API will return all permissions associated with the specific qualified ARN. If you don't provide this parameter, the API will return permissions that apply to the unqualified function ARN.
gpQualifier :: Lens' GetPolicy (Maybe Text)
gpQualifier = lens _gpQualifier (\ s a -> s{_gpQualifier = a})

-- | Function name whose resource policy you want to retrieve. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
gpFunctionName :: Lens' GetPolicy Text
gpFunctionName = lens _gpFunctionName (\ s a -> s{_gpFunctionName = a})

instance AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 GetPolicyResponse' <$>
                   (x .?> "Policy") <*> (x .?> "RevisionId") <*>
                     (pure (fromEnum s)))

instance Hashable GetPolicy where

instance NFData GetPolicy where

instance ToHeaders GetPolicy where
        toHeaders = const mempty

instance ToPath GetPolicy where
        toPath GetPolicy'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gpFunctionName,
               "/policy"]

instance ToQuery GetPolicy where
        toQuery GetPolicy'{..}
          = mconcat ["Qualifier" =: _gpQualifier]

-- |
--
--
--
-- /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { _gprsPolicy         :: !(Maybe Text)
  , _gprsRevisionId     :: !(Maybe Text)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicy' - The resource policy associated with the specified function. The response returns the same as a string using a backslash ("\") as an escape character in the JSON.
--
-- * 'gprsRevisionId' - Represents the latest updated revision of the function or alias.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPolicyResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPolicyResponse
getPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { _gprsPolicy = Nothing
    , _gprsRevisionId = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }


-- | The resource policy associated with the specified function. The response returns the same as a string using a backslash ("\") as an escape character in the JSON.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a})

-- | Represents the latest updated revision of the function or alias.
gprsRevisionId :: Lens' GetPolicyResponse (Maybe Text)
gprsRevisionId = lens _gprsRevisionId (\ s a -> s{_gprsRevisionId = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPolicyResponse where
