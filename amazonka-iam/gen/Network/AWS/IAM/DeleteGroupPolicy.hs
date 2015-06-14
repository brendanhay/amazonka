{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified inline policy that is embedded in the specified
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
    , delGroupName
    , delPolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    -- ** Response constructor
    , deleteGroupPolicyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'deleteGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delGroupName'
--
-- * 'delPolicyName'
data DeleteGroupPolicy = DeleteGroupPolicy'{_delGroupName :: Text, _delPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteGroupPolicy' smart constructor.
deleteGroupPolicy :: Text -> Text -> DeleteGroupPolicy
deleteGroupPolicy pGroupName pPolicyName = DeleteGroupPolicy'{_delGroupName = pGroupName, _delPolicyName = pPolicyName};

-- | The name (friendly name, not ARN) identifying the group that the policy
-- is embedded in.
delGroupName :: Lens' DeleteGroupPolicy Text
delGroupName = lens _delGroupName (\ s a -> s{_delGroupName = a});

-- | The name identifying the policy document to delete.
delPolicyName :: Lens' DeleteGroupPolicy Text
delPolicyName = lens _delPolicyName (\ s a -> s{_delPolicyName = a});

instance AWSRequest DeleteGroupPolicy where
        type Sv DeleteGroupPolicy = IAM
        type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
        request = post
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
               "GroupName" =: _delGroupName,
               "PolicyName" =: _delPolicyName]

-- | /See:/ 'deleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteGroupPolicyResponse' smart constructor.
deleteGroupPolicyResponse :: DeleteGroupPolicyResponse
deleteGroupPolicyResponse = DeleteGroupPolicyResponse';
