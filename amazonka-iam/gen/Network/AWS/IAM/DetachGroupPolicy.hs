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
-- Module      : Network.AWS.IAM.DetachGroupPolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified group.
--
-- A group can also have inline policies embedded with it. To delete an inline policy, use the < DeleteGroupPolicy> API. For information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/.
module Network.AWS.IAM.DetachGroupPolicy
    (
    -- * Creating a Request
      detachGroupPolicy
    , DetachGroupPolicy
    -- * Request Lenses
    , dgpGroupName
    , dgpPolicyARN

    -- * Destructuring the Response
    , detachGroupPolicyResponse
    , DetachGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachGroupPolicy' smart constructor.
data DetachGroupPolicy = DetachGroupPolicy'
    { _dgpGroupName :: !Text
    , _dgpPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgpGroupName'
--
-- * 'dgpPolicyARN'
detachGroupPolicy
    :: Text -- ^ 'dgpGroupName'
    -> Text -- ^ 'dgpPolicyARN'
    -> DetachGroupPolicy
detachGroupPolicy pGroupName_ pPolicyARN_ =
    DetachGroupPolicy'
    { _dgpGroupName = pGroupName_
    , _dgpPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the group to detach the policy from.
dgpGroupName :: Lens' DetachGroupPolicy Text
dgpGroupName = lens _dgpGroupName (\ s a -> s{_dgpGroupName = a});

-- | Undocumented member.
dgpPolicyARN :: Lens' DetachGroupPolicy Text
dgpPolicyARN = lens _dgpPolicyARN (\ s a -> s{_dgpPolicyARN = a});

instance AWSRequest DetachGroupPolicy where
        type Rs DetachGroupPolicy = DetachGroupPolicyResponse
        request = postQuery iam
        response = receiveNull DetachGroupPolicyResponse'

instance Hashable DetachGroupPolicy

instance NFData DetachGroupPolicy

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
data DetachGroupPolicyResponse =
    DetachGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachGroupPolicyResponse' with the minimum fields required to make a request.
--
detachGroupPolicyResponse
    :: DetachGroupPolicyResponse
detachGroupPolicyResponse = DetachGroupPolicyResponse'

instance NFData DetachGroupPolicyResponse
