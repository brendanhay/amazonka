{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- group.
--
-- A group can also have managed policies attached to it. To detach a
-- managed policy from a group, use DetachGroupPolicy. For more information
-- about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
module Network.AWS.IAM.DeleteGroupPolicy
    (
    -- * Request
      DeleteGroupPolicy
    -- ** Request constructor
    , deleteGroupPolicy
    -- ** Request lenses
    , dGroupName
    , dPolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    -- ** Response constructor
    , deleteGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dGroupName'
--
-- * 'dPolicyName'
data DeleteGroupPolicy = DeleteGroupPolicy'
    { _dGroupName  :: !Text
    , _dPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteGroupPolicy' smart constructor.
deleteGroupPolicy :: Text -> Text -> DeleteGroupPolicy
deleteGroupPolicy pGroupName_ pPolicyName_ =
    DeleteGroupPolicy'
    { _dGroupName = pGroupName_
    , _dPolicyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the group that the policy
-- is embedded in.
dGroupName :: Lens' DeleteGroupPolicy Text
dGroupName = lens _dGroupName (\ s a -> s{_dGroupName = a});

-- | The name identifying the policy document to delete.
dPolicyName :: Lens' DeleteGroupPolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a});

instance AWSRequest DeleteGroupPolicy where
        type Sv DeleteGroupPolicy = IAM
        type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
        request = postQuery
        response = receiveNull DeleteGroupPolicyResponse'

instance ToHeaders DeleteGroupPolicy where
        toHeaders = const mempty

instance ToPath DeleteGroupPolicy where
        toPath = const "/"

instance ToQuery DeleteGroupPolicy where
        toQuery DeleteGroupPolicy'{..}
          = mconcat
              ["Action" =: ("DeleteGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _dGroupName,
               "PolicyName" =: _dPolicyName]

-- | /See:/ 'deleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse =
    DeleteGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteGroupPolicyResponse' smart constructor.
deleteGroupPolicyResponse :: DeleteGroupPolicyResponse
deleteGroupPolicyResponse = DeleteGroupPolicyResponse'
