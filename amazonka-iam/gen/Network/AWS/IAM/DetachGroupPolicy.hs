{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DetachGroupPolicy
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

-- | Removes the specified managed policy from the specified group.
--
-- A group can also have inline policies embedded with it. To delete an
-- inline policy, use the DeleteGroupPolicy API. For information about
-- policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachGroupPolicy.html>
module Network.AWS.IAM.DetachGroupPolicy
    (
    -- * Request
      DetachGroupPolicy
    -- ** Request constructor
    , detachGroupPolicy
    -- ** Request lenses
    , dgpGroupName
    , dgpPolicyARN

    -- * Response
    , DetachGroupPolicyResponse
    -- ** Response constructor
    , detachGroupPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgpGroupName'
--
-- * 'dgpPolicyARN'
data DetachGroupPolicy = DetachGroupPolicy'{_dgpGroupName :: Text, _dgpPolicyARN :: Text} deriving (Eq, Read, Show)

-- | 'DetachGroupPolicy' smart constructor.
detachGroupPolicy :: Text -> Text -> DetachGroupPolicy
detachGroupPolicy pGroupName pPolicyARN = DetachGroupPolicy'{_dgpGroupName = pGroupName, _dgpPolicyARN = pPolicyARN};

-- | The name (friendly name, not ARN) of the group to detach the policy
-- from.
dgpGroupName :: Lens' DetachGroupPolicy Text
dgpGroupName = lens _dgpGroupName (\ s a -> s{_dgpGroupName = a});

-- | FIXME: Undocumented member.
dgpPolicyARN :: Lens' DetachGroupPolicy Text
dgpPolicyARN = lens _dgpPolicyARN (\ s a -> s{_dgpPolicyARN = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DetachGroupPolicy where
        type Sv DetachGroupPolicy = IAM
        type Rs DetachGroupPolicy = DetachGroupPolicyResponse
        request = post
        response = receiveNull DetachGroupPolicyResponse'

instance ToHeaders DetachGroupPolicy where
        toHeaders = const mempty

instance ToPath DetachGroupPolicy where
        toPath = const "/"

instance ToQuery DetachGroupPolicy where
        toQuery DetachGroupPolicy'{..}
          = mconcat
              ["Action" =: ("DetachGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _dgpGroupName,
               "PolicyArn" =: _dgpPolicyARN]

-- | /See:/ 'detachGroupPolicyResponse' smart constructor.
data DetachGroupPolicyResponse = DetachGroupPolicyResponse' deriving (Eq, Read, Show)

-- | 'DetachGroupPolicyResponse' smart constructor.
detachGroupPolicyResponse :: DetachGroupPolicyResponse
detachGroupPolicyResponse = DetachGroupPolicyResponse';
