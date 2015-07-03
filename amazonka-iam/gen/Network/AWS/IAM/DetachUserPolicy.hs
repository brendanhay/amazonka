{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.DetachUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the specified managed policy from the specified user.
--
-- A user can also have inline policies embedded with it. To delete an
-- inline policy, use the DeleteUserPolicy API. For information about
-- policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachUserPolicy.html>
module Network.AWS.IAM.DetachUserPolicy
    (
    -- * Request
      DetachUserPolicy
    -- ** Request constructor
    , detachUserPolicy
    -- ** Request lenses
    , detUserName
    , detPolicyARN

    -- * Response
    , DetachUserPolicyResponse
    -- ** Response constructor
    , detachUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detUserName'
--
-- * 'detPolicyARN'
data DetachUserPolicy = DetachUserPolicy'
    { _detUserName  :: !Text
    , _detPolicyARN :: !Text
    } deriving (Eq,Read,Show)

-- | 'DetachUserPolicy' smart constructor.
detachUserPolicy :: Text -> Text -> DetachUserPolicy
detachUserPolicy pUserName pPolicyARN =
    DetachUserPolicy'
    { _detUserName = pUserName
    , _detPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the user to detach the policy from.
detUserName :: Lens' DetachUserPolicy Text
detUserName = lens _detUserName (\ s a -> s{_detUserName = a});

-- | FIXME: Undocumented member.
detPolicyARN :: Lens' DetachUserPolicy Text
detPolicyARN = lens _detPolicyARN (\ s a -> s{_detPolicyARN = a});

instance AWSRequest DetachUserPolicy where
        type Sv DetachUserPolicy = IAM
        type Rs DetachUserPolicy = DetachUserPolicyResponse
        request = post
        response = receiveNull DetachUserPolicyResponse'

instance ToHeaders DetachUserPolicy where
        toHeaders = const mempty

instance ToPath DetachUserPolicy where
        toPath = const "/"

instance ToQuery DetachUserPolicy where
        toQuery DetachUserPolicy'{..}
          = mconcat
              ["Action" =: ("DetachUserPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _detUserName,
               "PolicyArn" =: _detPolicyARN]

-- | /See:/ 'detachUserPolicyResponse' smart constructor.
data DetachUserPolicyResponse =
    DetachUserPolicyResponse'
    deriving (Eq,Read,Show)

-- | 'DetachUserPolicyResponse' smart constructor.
detachUserPolicyResponse :: DetachUserPolicyResponse
detachUserPolicyResponse = DetachUserPolicyResponse'
