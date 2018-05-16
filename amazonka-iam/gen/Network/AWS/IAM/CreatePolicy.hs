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
-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your AWS account.
--
--
-- This operation creates a policy version with a version identifier of @v1@ and sets v1 as the policy's default version. For more information about policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- For more information about managed policies in general, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.CreatePolicy
    (
    -- * Creating a Request
      createPolicy
    , CreatePolicy
    -- * Request Lenses
    , cpPath
    , cpDescription
    , cpPolicyName
    , cpPolicyDocument

    -- * Destructuring the Response
    , createPolicyResponse
    , CreatePolicyResponse
    -- * Response Lenses
    , cprsPolicy
    , cprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { _cpPath           :: !(Maybe Text)
  , _cpDescription    :: !(Maybe Text)
  , _cpPolicyName     :: !Text
  , _cpPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPath' - The path for the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'cpDescription' - A friendly description of the policy. Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables." The policy description is immutable. After a value is assigned, it cannot be changed.
--
-- * 'cpPolicyName' - The friendly name of the policy. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'cpPolicyDocument' - The JSON policy document that you want to use as the content for the new policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
createPolicy
    :: Text -- ^ 'cpPolicyName'
    -> Text -- ^ 'cpPolicyDocument'
    -> CreatePolicy
createPolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { _cpPath = Nothing
    , _cpDescription = Nothing
    , _cpPolicyName = pPolicyName_
    , _cpPolicyDocument = pPolicyDocument_
    }


-- | The path for the policy. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
cpPath :: Lens' CreatePolicy (Maybe Text)
cpPath = lens _cpPath (\ s a -> s{_cpPath = a})

-- | A friendly description of the policy. Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables." The policy description is immutable. After a value is assigned, it cannot be changed.
cpDescription :: Lens' CreatePolicy (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a})

-- | The friendly name of the policy. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
cpPolicyName :: Lens' CreatePolicy Text
cpPolicyName = lens _cpPolicyName (\ s a -> s{_cpPolicyName = a})

-- | The JSON policy document that you want to use as the content for the new policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
cpPolicyDocument :: Lens' CreatePolicy Text
cpPolicyDocument = lens _cpPolicyDocument (\ s a -> s{_cpPolicyDocument = a})

instance AWSRequest CreatePolicy where
        type Rs CreatePolicy = CreatePolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreatePolicyResult"
              (\ s h x ->
                 CreatePolicyResponse' <$>
                   (x .@? "Policy") <*> (pure (fromEnum s)))

instance Hashable CreatePolicy where

instance NFData CreatePolicy where

instance ToHeaders CreatePolicy where
        toHeaders = const mempty

instance ToPath CreatePolicy where
        toPath = const "/"

instance ToQuery CreatePolicy where
        toQuery CreatePolicy'{..}
          = mconcat
              ["Action" =: ("CreatePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cpPath, "Description" =: _cpDescription,
               "PolicyName" =: _cpPolicyName,
               "PolicyDocument" =: _cpPolicyDocument]

-- | Contains the response to a successful 'CreatePolicy' request.
--
--
--
-- /See:/ 'createPolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { _cprsPolicy         :: !(Maybe Policy)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPolicy' - A structure containing details about the new policy.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPolicyResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePolicyResponse
createPolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    {_cprsPolicy = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | A structure containing details about the new policy.
cprsPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprsPolicy = lens _cprsPolicy (\ s a -> s{_cprsPolicy = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePolicyResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePolicyResponse where
