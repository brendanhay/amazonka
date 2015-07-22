{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an
-- inline policy, use the DeleteRolePolicy API. For information about
-- policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachRolePolicy.html>
module Network.AWS.IAM.DetachRolePolicy
    (
    -- * Request
      DetachRolePolicy
    -- ** Request constructor
    , detachRolePolicy
    -- ** Request lenses
    , drprqRoleName
    , drprqPolicyARN

    -- * Response
    , DetachRolePolicyResponse
    -- ** Response constructor
    , detachRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drprqRoleName'
--
-- * 'drprqPolicyARN'
data DetachRolePolicy = DetachRolePolicy'
    { _drprqRoleName  :: !Text
    , _drprqPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachRolePolicy' smart constructor.
detachRolePolicy :: Text -> Text -> DetachRolePolicy
detachRolePolicy pRoleName pPolicyARN =
    DetachRolePolicy'
    { _drprqRoleName = pRoleName
    , _drprqPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the role to detach the policy from.
drprqRoleName :: Lens' DetachRolePolicy Text
drprqRoleName = lens _drprqRoleName (\ s a -> s{_drprqRoleName = a});

-- | FIXME: Undocumented member.
drprqPolicyARN :: Lens' DetachRolePolicy Text
drprqPolicyARN = lens _drprqPolicyARN (\ s a -> s{_drprqPolicyARN = a});

instance AWSRequest DetachRolePolicy where
        type Sv DetachRolePolicy = IAM
        type Rs DetachRolePolicy = DetachRolePolicyResponse
        request = post
        response = receiveNull DetachRolePolicyResponse'

instance ToHeaders DetachRolePolicy where
        toHeaders = const mempty

instance ToPath DetachRolePolicy where
        toPath = const "/"

instance ToQuery DetachRolePolicy where
        toQuery DetachRolePolicy'{..}
          = mconcat
              ["Action" =: ("DetachRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _drprqRoleName,
               "PolicyArn" =: _drprqPolicyARN]

-- | /See:/ 'detachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse =
    DetachRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachRolePolicyResponse' smart constructor.
detachRolePolicyResponse :: DetachRolePolicyResponse
detachRolePolicyResponse = DetachRolePolicyResponse'
