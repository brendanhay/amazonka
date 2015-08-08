{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified group.
--
-- You use this API to attach a managed policy to a group. To embed an
-- inline policy in a group, use PutGroupPolicy.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachGroupPolicy.html AWS API Reference> for AttachGroupPolicy.
module Network.AWS.IAM.AttachGroupPolicy
    (
    -- * Creating a Request
      AttachGroupPolicy
    , attachGroupPolicy
    -- * Request Lenses
    , agpGroupName
    , agpPolicyARN

    -- * Destructuring the Response
    , AttachGroupPolicyResponse
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachGroupPolicy' smart constructor.
attachGroupPolicy :: Text -> Text -> AttachGroupPolicy
attachGroupPolicy pGroupName_ pPolicyARN_ =
    AttachGroupPolicy'
    { _agpGroupName = pGroupName_
    , _agpPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
agpGroupName :: Lens' AttachGroupPolicy Text
agpGroupName = lens _agpGroupName (\ s a -> s{_agpGroupName = a});

-- | Undocumented member.
agpPolicyARN :: Lens' AttachGroupPolicy Text
agpPolicyARN = lens _agpPolicyARN (\ s a -> s{_agpPolicyARN = a});

instance AWSRequest AttachGroupPolicy where
        type Sv AttachGroupPolicy = IAM
        type Rs AttachGroupPolicy = AttachGroupPolicyResponse
        request = postQuery
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachGroupPolicyResponse' smart constructor.
attachGroupPolicyResponse :: AttachGroupPolicyResponse
attachGroupPolicyResponse = AttachGroupPolicyResponse'
