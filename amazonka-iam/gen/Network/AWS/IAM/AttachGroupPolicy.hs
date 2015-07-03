{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.AttachGroupPolicy
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

-- | Attaches the specified managed policy to the specified group.
--
-- You use this API to attach a managed policy to a group. To embed an
-- inline policy in a group, use PutGroupPolicy.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachGroupPolicy.html>
module Network.AWS.IAM.AttachGroupPolicy
    (
    -- * Request
      AttachGroupPolicy
    -- ** Request constructor
    , attachGroupPolicy
    -- ** Request lenses
    , agpGroupName
    , agpPolicyARN

    -- * Response
    , AttachGroupPolicyResponse
    -- ** Response constructor
    , attachGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agpGroupName'
--
-- * 'agpPolicyARN'
data AttachGroupPolicy = AttachGroupPolicy'
    { _agpGroupName :: !Text
    , _agpPolicyARN :: !Text
    } deriving (Eq,Read,Show)

-- | 'AttachGroupPolicy' smart constructor.
attachGroupPolicy :: Text -> Text -> AttachGroupPolicy
attachGroupPolicy pGroupName pPolicyARN =
    AttachGroupPolicy'
    { _agpGroupName = pGroupName
    , _agpPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
agpGroupName :: Lens' AttachGroupPolicy Text
agpGroupName = lens _agpGroupName (\ s a -> s{_agpGroupName = a});

-- | FIXME: Undocumented member.
agpPolicyARN :: Lens' AttachGroupPolicy Text
agpPolicyARN = lens _agpPolicyARN (\ s a -> s{_agpPolicyARN = a});

instance AWSRequest AttachGroupPolicy where
        type Sv AttachGroupPolicy = IAM
        type Rs AttachGroupPolicy = AttachGroupPolicyResponse
        request = post
        response = receiveNull AttachGroupPolicyResponse'

instance ToHeaders AttachGroupPolicy where
        toHeaders = const mempty

instance ToPath AttachGroupPolicy where
        toPath = const "/"

instance ToQuery AttachGroupPolicy where
        toQuery AttachGroupPolicy'{..}
          = mconcat
              ["Action" =: ("AttachGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _agpGroupName,
               "PolicyArn" =: _agpPolicyARN]

-- | /See:/ 'attachGroupPolicyResponse' smart constructor.
data AttachGroupPolicyResponse =
    AttachGroupPolicyResponse'
    deriving (Eq,Read,Show)

-- | 'AttachGroupPolicyResponse' smart constructor.
attachGroupPolicyResponse :: AttachGroupPolicyResponse
attachGroupPolicyResponse = AttachGroupPolicyResponse'
