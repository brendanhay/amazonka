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
-- Module      : Network.AWS.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified managed policy. To update a managed policy, you create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must delete an existing version using 'DeletePolicyVersion' before you create a new version.
--
--
-- Optionally, you can set the new version as the policy's default version. The default version is the version that is in effect for the IAM users, groups, and roles to which the policy is attached.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.CreatePolicyVersion
    (
    -- * Creating a Request
      createPolicyVersion
    , CreatePolicyVersion
    -- * Request Lenses
    , cpvSetAsDefault
    , cpvPolicyARN
    , cpvPolicyDocument

    -- * Destructuring the Response
    , createPolicyVersionResponse
    , CreatePolicyVersionResponse
    -- * Response Lenses
    , cpvrsPolicyVersion
    , cpvrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { _cpvSetAsDefault   :: !(Maybe Bool)
  , _cpvPolicyARN      :: !Text
  , _cpvPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvSetAsDefault' - Specifies whether to set this version as the policy's default version. When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- * 'cpvPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'cpvPolicyDocument' - The JSON policy document that you want to use as the content for this new version of the policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
createPolicyVersion
    :: Text -- ^ 'cpvPolicyARN'
    -> Text -- ^ 'cpvPolicyDocument'
    -> CreatePolicyVersion
createPolicyVersion pPolicyARN_ pPolicyDocument_ =
  CreatePolicyVersion'
    { _cpvSetAsDefault = Nothing
    , _cpvPolicyARN = pPolicyARN_
    , _cpvPolicyDocument = pPolicyDocument_
    }


-- | Specifies whether to set this version as the policy's default version. When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
cpvSetAsDefault :: Lens' CreatePolicyVersion (Maybe Bool)
cpvSetAsDefault = lens _cpvSetAsDefault (\ s a -> s{_cpvSetAsDefault = a})

-- | The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
cpvPolicyARN :: Lens' CreatePolicyVersion Text
cpvPolicyARN = lens _cpvPolicyARN (\ s a -> s{_cpvPolicyARN = a})

-- | The JSON policy document that you want to use as the content for this new version of the policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
cpvPolicyDocument :: Lens' CreatePolicyVersion Text
cpvPolicyDocument = lens _cpvPolicyDocument (\ s a -> s{_cpvPolicyDocument = a})

instance AWSRequest CreatePolicyVersion where
        type Rs CreatePolicyVersion =
             CreatePolicyVersionResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreatePolicyVersionResult"
              (\ s h x ->
                 CreatePolicyVersionResponse' <$>
                   (x .@? "PolicyVersion") <*> (pure (fromEnum s)))

instance Hashable CreatePolicyVersion where

instance NFData CreatePolicyVersion where

instance ToHeaders CreatePolicyVersion where
        toHeaders = const mempty

instance ToPath CreatePolicyVersion where
        toPath = const "/"

instance ToQuery CreatePolicyVersion where
        toQuery CreatePolicyVersion'{..}
          = mconcat
              ["Action" =: ("CreatePolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "SetAsDefault" =: _cpvSetAsDefault,
               "PolicyArn" =: _cpvPolicyARN,
               "PolicyDocument" =: _cpvPolicyDocument]

-- | Contains the response to a successful 'CreatePolicyVersion' request.
--
--
--
-- /See:/ 'createPolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { _cpvrsPolicyVersion  :: !(Maybe PolicyVersion)
  , _cpvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvrsPolicyVersion' - A structure containing details about the new policy version.
--
-- * 'cpvrsResponseStatus' - -- | The response status code.
createPolicyVersionResponse
    :: Int -- ^ 'cpvrsResponseStatus'
    -> CreatePolicyVersionResponse
createPolicyVersionResponse pResponseStatus_ =
  CreatePolicyVersionResponse'
    {_cpvrsPolicyVersion = Nothing, _cpvrsResponseStatus = pResponseStatus_}


-- | A structure containing details about the new policy version.
cpvrsPolicyVersion :: Lens' CreatePolicyVersionResponse (Maybe PolicyVersion)
cpvrsPolicyVersion = lens _cpvrsPolicyVersion (\ s a -> s{_cpvrsPolicyVersion = a})

-- | -- | The response status code.
cpvrsResponseStatus :: Lens' CreatePolicyVersionResponse Int
cpvrsResponseStatus = lens _cpvrsResponseStatus (\ s a -> s{_cpvrsResponseStatus = a})

instance NFData CreatePolicyVersionResponse where
