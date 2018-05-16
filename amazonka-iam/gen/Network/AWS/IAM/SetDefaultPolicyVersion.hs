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
-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version.
--
--
-- This operation affects all users, groups, and roles that the policy is attached to. To list the users, groups, and roles that the policy is attached to, use the 'ListEntitiesForPolicy' API.
--
-- For information about managed policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.SetDefaultPolicyVersion
    (
    -- * Creating a Request
      setDefaultPolicyVersion
    , SetDefaultPolicyVersion
    -- * Request Lenses
    , sdpvPolicyARN
    , sdpvVersionId

    -- * Destructuring the Response
    , setDefaultPolicyVersionResponse
    , SetDefaultPolicyVersionResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { _sdpvPolicyARN :: !Text
  , _sdpvVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultPolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpvPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'sdpvVersionId' - The version of the policy to set as the default (operative) version. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
setDefaultPolicyVersion
    :: Text -- ^ 'sdpvPolicyARN'
    -> Text -- ^ 'sdpvVersionId'
    -> SetDefaultPolicyVersion
setDefaultPolicyVersion pPolicyARN_ pVersionId_ =
  SetDefaultPolicyVersion'
    {_sdpvPolicyARN = pPolicyARN_, _sdpvVersionId = pVersionId_}


-- | The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
sdpvPolicyARN :: Lens' SetDefaultPolicyVersion Text
sdpvPolicyARN = lens _sdpvPolicyARN (\ s a -> s{_sdpvPolicyARN = a})

-- | The version of the policy to set as the default (operative) version. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
sdpvVersionId :: Lens' SetDefaultPolicyVersion Text
sdpvVersionId = lens _sdpvVersionId (\ s a -> s{_sdpvVersionId = a})

instance AWSRequest SetDefaultPolicyVersion where
        type Rs SetDefaultPolicyVersion =
             SetDefaultPolicyVersionResponse
        request = postQuery iam
        response
          = receiveNull SetDefaultPolicyVersionResponse'

instance Hashable SetDefaultPolicyVersion where

instance NFData SetDefaultPolicyVersion where

instance ToHeaders SetDefaultPolicyVersion where
        toHeaders = const mempty

instance ToPath SetDefaultPolicyVersion where
        toPath = const "/"

instance ToQuery SetDefaultPolicyVersion where
        toQuery SetDefaultPolicyVersion'{..}
          = mconcat
              ["Action" =:
                 ("SetDefaultPolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _sdpvPolicyARN,
               "VersionId" =: _sdpvVersionId]

-- | /See:/ 'setDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultPolicyVersionResponse' with the minimum fields required to make a request.
--
setDefaultPolicyVersionResponse
    :: SetDefaultPolicyVersionResponse
setDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'


instance NFData SetDefaultPolicyVersionResponse where
