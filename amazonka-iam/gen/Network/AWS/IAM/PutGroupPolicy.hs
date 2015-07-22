{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds (or updates) an inline policy document that is embedded in the
-- specified group.
--
-- A user can also have managed policies attached to it. To attach a
-- managed policy to a group, use AttachGroupPolicy. To create a new
-- managed policy, use CreatePolicy. For information about policies, refer
-- to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you
-- can embed in a group, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutGroupPolicy@. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html>
module Network.AWS.IAM.PutGroupPolicy
    (
    -- * Request
      PutGroupPolicy
    -- ** Request constructor
    , putGroupPolicy
    -- ** Request lenses
    , pgprqGroupName
    , pgprqPolicyName
    , pgprqPolicyDocument

    -- * Response
    , PutGroupPolicyResponse
    -- ** Response constructor
    , putGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putGroupPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgprqGroupName'
--
-- * 'pgprqPolicyName'
--
-- * 'pgprqPolicyDocument'
data PutGroupPolicy = PutGroupPolicy'
    { _pgprqGroupName      :: !Text
    , _pgprqPolicyName     :: !Text
    , _pgprqPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutGroupPolicy' smart constructor.
putGroupPolicy :: Text -> Text -> Text -> PutGroupPolicy
putGroupPolicy pGroupName pPolicyName pPolicyDocument =
    PutGroupPolicy'
    { _pgprqGroupName = pGroupName
    , _pgprqPolicyName = pPolicyName
    , _pgprqPolicyDocument = pPolicyDocument
    }

-- | The name of the group to associate the policy with.
pgprqGroupName :: Lens' PutGroupPolicy Text
pgprqGroupName = lens _pgprqGroupName (\ s a -> s{_pgprqGroupName = a});

-- | The name of the policy document.
pgprqPolicyName :: Lens' PutGroupPolicy Text
pgprqPolicyName = lens _pgprqPolicyName (\ s a -> s{_pgprqPolicyName = a});

-- | The policy document.
pgprqPolicyDocument :: Lens' PutGroupPolicy Text
pgprqPolicyDocument = lens _pgprqPolicyDocument (\ s a -> s{_pgprqPolicyDocument = a});

instance AWSRequest PutGroupPolicy where
        type Sv PutGroupPolicy = IAM
        type Rs PutGroupPolicy = PutGroupPolicyResponse
        request = post
        response = receiveNull PutGroupPolicyResponse'

instance ToHeaders PutGroupPolicy where
        toHeaders = const mempty

instance ToPath PutGroupPolicy where
        toPath = const "/"

instance ToQuery PutGroupPolicy where
        toQuery PutGroupPolicy'{..}
          = mconcat
              ["Action" =: ("PutGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _pgprqGroupName,
               "PolicyName" =: _pgprqPolicyName,
               "PolicyDocument" =: _pgprqPolicyDocument]

-- | /See:/ 'putGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse =
    PutGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutGroupPolicyResponse' smart constructor.
putGroupPolicyResponse :: PutGroupPolicyResponse
putGroupPolicyResponse = PutGroupPolicyResponse'
