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
-- Module      : Network.AWS.IAM.DeletePolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified managed policy.
--
--
-- You cannot delete the default version from a policy using this API. To delete the default version from a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use 'ListPolicyVersions' .
--
-- For information about versions for managed policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DeletePolicyVersion
    (
    -- * Creating a Request
      deletePolicyVersion
    , DeletePolicyVersion
    -- * Request Lenses
    , dpvPolicyARN
    , dpvVersionId

    -- * Destructuring the Response
    , deletePolicyVersionResponse
    , DeletePolicyVersionResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { _dpvPolicyARN :: !Text
  , _dpvVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'dpvVersionId' - The policy version to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
deletePolicyVersion
    :: Text -- ^ 'dpvPolicyARN'
    -> Text -- ^ 'dpvVersionId'
    -> DeletePolicyVersion
deletePolicyVersion pPolicyARN_ pVersionId_ =
  DeletePolicyVersion'
    {_dpvPolicyARN = pPolicyARN_, _dpvVersionId = pVersionId_}


-- | The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
dpvPolicyARN :: Lens' DeletePolicyVersion Text
dpvPolicyARN = lens _dpvPolicyARN (\ s a -> s{_dpvPolicyARN = a})

-- | The policy version to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
dpvVersionId :: Lens' DeletePolicyVersion Text
dpvVersionId = lens _dpvVersionId (\ s a -> s{_dpvVersionId = a})

instance AWSRequest DeletePolicyVersion where
        type Rs DeletePolicyVersion =
             DeletePolicyVersionResponse
        request = postQuery iam
        response = receiveNull DeletePolicyVersionResponse'

instance Hashable DeletePolicyVersion where

instance NFData DeletePolicyVersion where

instance ToHeaders DeletePolicyVersion where
        toHeaders = const mempty

instance ToPath DeletePolicyVersion where
        toPath = const "/"

instance ToQuery DeletePolicyVersion where
        toQuery DeletePolicyVersion'{..}
          = mconcat
              ["Action" =: ("DeletePolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _dpvPolicyARN,
               "VersionId" =: _dpvVersionId]

-- | /See:/ 'deletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse =
  DeletePolicyVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicyVersionResponse' with the minimum fields required to make a request.
--
deletePolicyVersionResponse
    :: DeletePolicyVersionResponse
deletePolicyVersionResponse = DeletePolicyVersionResponse'


instance NFData DeletePolicyVersionResponse where
