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
-- Module      : Network.AWS.SecretsManager.PutResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the contents of the specified resource-based permission policy to a secret. A resource-based policy is optional. Alternatively, you can use IAM identity-based policies that specify the secret's Amazon Resource Name (ARN) in the policy statement's @Resources@ element. You can also use a combination of both identity-based and resource-based policies. The affected users and roles receive the permissions that are permitted by all of the relevant policies. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-based-policies.html Using Resource-Based Policies for AWS Secrets Manager> . For the complete description of the AWS policy syntax and grammar, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference> in the /IAM User Guide/ .
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:PutResourcePolicy
--
--
--
-- __Related operations__
--
--     * To retrieve the resource policy that's attached to a secret, use 'GetResourcePolicy' .
--
--     * To delete the resource-based policy that's attached to a secret, use 'DeleteResourcePolicy' .
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
--
--
--
module Network.AWS.SecretsManager.PutResourcePolicy
    (
    -- * Creating a Request
      putResourcePolicy
    , PutResourcePolicy
    -- * Request Lenses
    , prpSecretId
    , prpResourcePolicy

    -- * Destructuring the Response
    , putResourcePolicyResponse
    , PutResourcePolicyResponse
    -- * Response Lenses
    , prprsARN
    , prprsName
    , prprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'putResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { _prpSecretId       :: !Text
  , _prpResourcePolicy :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpSecretId' - Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
--
-- * 'prpResourcePolicy' - A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
putResourcePolicy
    :: Text -- ^ 'prpSecretId'
    -> Text -- ^ 'prpResourcePolicy'
    -> PutResourcePolicy
putResourcePolicy pSecretId_ pResourcePolicy_ =
  PutResourcePolicy'
    {_prpSecretId = pSecretId_, _prpResourcePolicy = pResourcePolicy_}


-- | Specifies the secret that you want to attach the resource-based policy to. You can specify either the ARN or the friendly name of the secret.
prpSecretId :: Lens' PutResourcePolicy Text
prpSecretId = lens _prpSecretId (\ s a -> s{_prpSecretId = a})

-- | A JSON-formatted string that's constructed according to the grammar and syntax for an AWS resource-based policy. The policy in the string identifies who can access or manage this secret and its versions. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
prpResourcePolicy :: Lens' PutResourcePolicy Text
prpResourcePolicy = lens _prpResourcePolicy (\ s a -> s{_prpResourcePolicy = a})

instance AWSRequest PutResourcePolicy where
        type Rs PutResourcePolicy = PutResourcePolicyResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 PutResourcePolicyResponse' <$>
                   (x .?> "ARN") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable PutResourcePolicy where

instance NFData PutResourcePolicy where

instance ToHeaders PutResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.PutResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutResourcePolicy where
        toJSON PutResourcePolicy'{..}
          = object
              (catMaybes
                 [Just ("SecretId" .= _prpSecretId),
                  Just ("ResourcePolicy" .= _prpResourcePolicy)])

instance ToPath PutResourcePolicy where
        toPath = const "/"

instance ToQuery PutResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'putResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { _prprsARN            :: !(Maybe Text)
  , _prprsName           :: !(Maybe Text)
  , _prprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prprsARN' - The ARN of the secret that the resource-based policy was retrieved for.
--
-- * 'prprsName' - The friendly name of the secret that the resource-based policy was retrieved for.
--
-- * 'prprsResponseStatus' - -- | The response status code.
putResourcePolicyResponse
    :: Int -- ^ 'prprsResponseStatus'
    -> PutResourcePolicyResponse
putResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { _prprsARN = Nothing
    , _prprsName = Nothing
    , _prprsResponseStatus = pResponseStatus_
    }


-- | The ARN of the secret that the resource-based policy was retrieved for.
prprsARN :: Lens' PutResourcePolicyResponse (Maybe Text)
prprsARN = lens _prprsARN (\ s a -> s{_prprsARN = a})

-- | The friendly name of the secret that the resource-based policy was retrieved for.
prprsName :: Lens' PutResourcePolicyResponse (Maybe Text)
prprsName = lens _prprsName (\ s a -> s{_prprsName = a})

-- | -- | The response status code.
prprsResponseStatus :: Lens' PutResourcePolicyResponse Int
prprsResponseStatus = lens _prprsResponseStatus (\ s a -> s{_prprsResponseStatus = a})

instance NFData PutResourcePolicyResponse where
