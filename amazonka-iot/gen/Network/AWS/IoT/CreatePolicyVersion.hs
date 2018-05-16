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
-- Module      : Network.AWS.IoT.CreatePolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use 'DeletePolicyVersion' to delete an existing version before you create a new one.
--
--
-- Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
--
module Network.AWS.IoT.CreatePolicyVersion
    (
    -- * Creating a Request
      createPolicyVersion
    , CreatePolicyVersion
    -- * Request Lenses
    , cpvSetAsDefault
    , cpvPolicyName
    , cpvPolicyDocument

    -- * Destructuring the Response
    , createPolicyVersionResponse
    , CreatePolicyVersionResponse
    -- * Response Lenses
    , cpvrsPolicyDocument
    , cpvrsPolicyVersionId
    , cpvrsPolicyARN
    , cpvrsIsDefaultVersion
    , cpvrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreatePolicyVersion operation.
--
--
--
-- /See:/ 'createPolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { _cpvSetAsDefault   :: !(Maybe Bool)
  , _cpvPolicyName     :: !Text
  , _cpvPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvSetAsDefault' - Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
--
-- * 'cpvPolicyName' - The policy name.
--
-- * 'cpvPolicyDocument' - The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
createPolicyVersion
    :: Text -- ^ 'cpvPolicyName'
    -> Text -- ^ 'cpvPolicyDocument'
    -> CreatePolicyVersion
createPolicyVersion pPolicyName_ pPolicyDocument_ =
  CreatePolicyVersion'
    { _cpvSetAsDefault = Nothing
    , _cpvPolicyName = pPolicyName_
    , _cpvPolicyDocument = pPolicyDocument_
    }


-- | Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
cpvSetAsDefault :: Lens' CreatePolicyVersion (Maybe Bool)
cpvSetAsDefault = lens _cpvSetAsDefault (\ s a -> s{_cpvSetAsDefault = a})

-- | The policy name.
cpvPolicyName :: Lens' CreatePolicyVersion Text
cpvPolicyName = lens _cpvPolicyName (\ s a -> s{_cpvPolicyName = a})

-- | The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
cpvPolicyDocument :: Lens' CreatePolicyVersion Text
cpvPolicyDocument = lens _cpvPolicyDocument (\ s a -> s{_cpvPolicyDocument = a})

instance AWSRequest CreatePolicyVersion where
        type Rs CreatePolicyVersion =
             CreatePolicyVersionResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreatePolicyVersionResponse' <$>
                   (x .?> "policyDocument") <*>
                     (x .?> "policyVersionId")
                     <*> (x .?> "policyArn")
                     <*> (x .?> "isDefaultVersion")
                     <*> (pure (fromEnum s)))

instance Hashable CreatePolicyVersion where

instance NFData CreatePolicyVersion where

instance ToHeaders CreatePolicyVersion where
        toHeaders = const mempty

instance ToJSON CreatePolicyVersion where
        toJSON CreatePolicyVersion'{..}
          = object
              (catMaybes
                 [Just ("policyDocument" .= _cpvPolicyDocument)])

instance ToPath CreatePolicyVersion where
        toPath CreatePolicyVersion'{..}
          = mconcat
              ["/policies/", toBS _cpvPolicyName, "/version"]

instance ToQuery CreatePolicyVersion where
        toQuery CreatePolicyVersion'{..}
          = mconcat ["setAsDefault" =: _cpvSetAsDefault]

-- | The output of the CreatePolicyVersion operation.
--
--
--
-- /See:/ 'createPolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { _cpvrsPolicyDocument   :: !(Maybe Text)
  , _cpvrsPolicyVersionId  :: !(Maybe Text)
  , _cpvrsPolicyARN        :: !(Maybe Text)
  , _cpvrsIsDefaultVersion :: !(Maybe Bool)
  , _cpvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePolicyVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvrsPolicyDocument' - The JSON document that describes the policy.
--
-- * 'cpvrsPolicyVersionId' - The policy version ID.
--
-- * 'cpvrsPolicyARN' - The policy ARN.
--
-- * 'cpvrsIsDefaultVersion' - Specifies whether the policy version is the default.
--
-- * 'cpvrsResponseStatus' - -- | The response status code.
createPolicyVersionResponse
    :: Int -- ^ 'cpvrsResponseStatus'
    -> CreatePolicyVersionResponse
createPolicyVersionResponse pResponseStatus_ =
  CreatePolicyVersionResponse'
    { _cpvrsPolicyDocument = Nothing
    , _cpvrsPolicyVersionId = Nothing
    , _cpvrsPolicyARN = Nothing
    , _cpvrsIsDefaultVersion = Nothing
    , _cpvrsResponseStatus = pResponseStatus_
    }


-- | The JSON document that describes the policy.
cpvrsPolicyDocument :: Lens' CreatePolicyVersionResponse (Maybe Text)
cpvrsPolicyDocument = lens _cpvrsPolicyDocument (\ s a -> s{_cpvrsPolicyDocument = a})

-- | The policy version ID.
cpvrsPolicyVersionId :: Lens' CreatePolicyVersionResponse (Maybe Text)
cpvrsPolicyVersionId = lens _cpvrsPolicyVersionId (\ s a -> s{_cpvrsPolicyVersionId = a})

-- | The policy ARN.
cpvrsPolicyARN :: Lens' CreatePolicyVersionResponse (Maybe Text)
cpvrsPolicyARN = lens _cpvrsPolicyARN (\ s a -> s{_cpvrsPolicyARN = a})

-- | Specifies whether the policy version is the default.
cpvrsIsDefaultVersion :: Lens' CreatePolicyVersionResponse (Maybe Bool)
cpvrsIsDefaultVersion = lens _cpvrsIsDefaultVersion (\ s a -> s{_cpvrsIsDefaultVersion = a})

-- | -- | The response status code.
cpvrsResponseStatus :: Lens' CreatePolicyVersionResponse Int
cpvrsResponseStatus = lens _cpvrsResponseStatus (\ s a -> s{_cpvrsResponseStatus = a})

instance NFData CreatePolicyVersionResponse where
