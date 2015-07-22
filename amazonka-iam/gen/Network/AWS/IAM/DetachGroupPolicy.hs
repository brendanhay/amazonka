{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified group.
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
    , dgprqGroupName
    , dgprqPolicyARN

    -- * Response
    , DetachGroupPolicyResponse
    -- ** Response constructor
    , detachGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgprqGroupName'
--
-- * 'dgprqPolicyARN'
data DetachGroupPolicy = DetachGroupPolicy'
    { _dgprqGroupName :: !Text
    , _dgprqPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachGroupPolicy' smart constructor.
detachGroupPolicy :: Text -> Text -> DetachGroupPolicy
detachGroupPolicy pGroupName pPolicyARN =
    DetachGroupPolicy'
    { _dgprqGroupName = pGroupName
    , _dgprqPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the group to detach the policy
-- from.
dgprqGroupName :: Lens' DetachGroupPolicy Text
dgprqGroupName = lens _dgprqGroupName (\ s a -> s{_dgprqGroupName = a});

-- | FIXME: Undocumented member.
dgprqPolicyARN :: Lens' DetachGroupPolicy Text
dgprqPolicyARN = lens _dgprqPolicyARN (\ s a -> s{_dgprqPolicyARN = a});

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
               "GroupName" =: _dgprqGroupName,
               "PolicyArn" =: _dgprqPolicyARN]

-- | /See:/ 'detachGroupPolicyResponse' smart constructor.
data DetachGroupPolicyResponse =
    DetachGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachGroupPolicyResponse' smart constructor.
detachGroupPolicyResponse :: DetachGroupPolicyResponse
detachGroupPolicyResponse = DetachGroupPolicyResponse'
