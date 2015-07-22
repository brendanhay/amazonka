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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachGroupPolicy.html>
module Network.AWS.IAM.AttachGroupPolicy
    (
    -- * Request
      AttachGroupPolicy
    -- ** Request constructor
    , attachGroupPolicy
    -- ** Request lenses
    , agprqGroupName
    , agprqPolicyARN

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
-- * 'agprqGroupName'
--
-- * 'agprqPolicyARN'
data AttachGroupPolicy = AttachGroupPolicy'
    { _agprqGroupName :: !Text
    , _agprqPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachGroupPolicy' smart constructor.
attachGroupPolicy :: Text -> Text -> AttachGroupPolicy
attachGroupPolicy pGroupName_ pPolicyARN_ =
    AttachGroupPolicy'
    { _agprqGroupName = pGroupName_
    , _agprqPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
agprqGroupName :: Lens' AttachGroupPolicy Text
agprqGroupName = lens _agprqGroupName (\ s a -> s{_agprqGroupName = a});

-- | FIXME: Undocumented member.
agprqPolicyARN :: Lens' AttachGroupPolicy Text
agprqPolicyARN = lens _agprqPolicyARN (\ s a -> s{_agprqPolicyARN = a});

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
               "GroupName" =: _agprqGroupName,
               "PolicyArn" =: _agprqPolicyARN]

-- | /See:/ 'attachGroupPolicyResponse' smart constructor.
data AttachGroupPolicyResponse =
    AttachGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachGroupPolicyResponse' smart constructor.
attachGroupPolicyResponse :: AttachGroupPolicyResponse
attachGroupPolicyResponse = AttachGroupPolicyResponse'
