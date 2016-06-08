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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified managed policy.
--
-- You cannot delete the default version of a policy using this API. To delete the default version of a policy, use < DeletePolicy>. To find out which version of a policy is marked as the default version, use < ListPolicyVersions>.
--
-- For information about versions for managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/.
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

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
    { _dpvPolicyARN :: !Text
    , _dpvVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvPolicyARN'
--
-- * 'dpvVersionId'
deletePolicyVersion
    :: Text -- ^ 'dpvPolicyARN'
    -> Text -- ^ 'dpvVersionId'
    -> DeletePolicyVersion
deletePolicyVersion pPolicyARN_ pVersionId_ =
    DeletePolicyVersion'
    { _dpvPolicyARN = pPolicyARN_
    , _dpvVersionId = pVersionId_
    }

-- | Undocumented member.
dpvPolicyARN :: Lens' DeletePolicyVersion Text
dpvPolicyARN = lens _dpvPolicyARN (\ s a -> s{_dpvPolicyARN = a});

-- | The policy version to delete.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/.
dpvVersionId :: Lens' DeletePolicyVersion Text
dpvVersionId = lens _dpvVersionId (\ s a -> s{_dpvVersionId = a});

instance AWSRequest DeletePolicyVersion where
        type Rs DeletePolicyVersion =
             DeletePolicyVersionResponse
        request = postQuery iam
        response = receiveNull DeletePolicyVersionResponse'

instance Hashable DeletePolicyVersion

instance NFData DeletePolicyVersion

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePolicyVersionResponse' with the minimum fields required to make a request.
--
deletePolicyVersionResponse
    :: DeletePolicyVersionResponse
deletePolicyVersionResponse = DeletePolicyVersionResponse'

instance NFData DeletePolicyVersionResponse
