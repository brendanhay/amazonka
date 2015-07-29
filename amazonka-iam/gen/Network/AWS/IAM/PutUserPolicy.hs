{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds (or updates) an inline policy document that is embedded in the
-- specified user.
--
-- A user can also have a managed policy attached to it. To attach a
-- managed policy to a user, use AttachUserPolicy. To create a new managed
-- policy, use CreatePolicy. For information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you
-- can embed in a user, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutUserPolicy@. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutUserPolicy.html>
module Network.AWS.IAM.PutUserPolicy
    (
    -- * Request
      PutUserPolicy
    -- ** Request constructor
    , putUserPolicy
    -- ** Request lenses
    , pupUserName
    , pupPolicyName
    , pupPolicyDocument

    -- * Response
    , PutUserPolicyResponse
    -- ** Response constructor
    , putUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pupUserName'
--
-- * 'pupPolicyName'
--
-- * 'pupPolicyDocument'
data PutUserPolicy = PutUserPolicy'
    { _pupUserName       :: !Text
    , _pupPolicyName     :: !Text
    , _pupPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutUserPolicy' smart constructor.
putUserPolicy :: Text -> Text -> Text -> PutUserPolicy
putUserPolicy pUserName_ pPolicyName_ pPolicyDocument_ =
    PutUserPolicy'
    { _pupUserName = pUserName_
    , _pupPolicyName = pPolicyName_
    , _pupPolicyDocument = pPolicyDocument_
    }

-- | The name of the user to associate the policy with.
pupUserName :: Lens' PutUserPolicy Text
pupUserName = lens _pupUserName (\ s a -> s{_pupUserName = a});

-- | The name of the policy document.
pupPolicyName :: Lens' PutUserPolicy Text
pupPolicyName = lens _pupPolicyName (\ s a -> s{_pupPolicyName = a});

-- | The policy document.
pupPolicyDocument :: Lens' PutUserPolicy Text
pupPolicyDocument = lens _pupPolicyDocument (\ s a -> s{_pupPolicyDocument = a});

instance AWSRequest PutUserPolicy where
        type Sv PutUserPolicy = IAM
        type Rs PutUserPolicy = PutUserPolicyResponse
        request = postQuery
        response = receiveNull PutUserPolicyResponse'

instance ToHeaders PutUserPolicy where
        toHeaders = const mempty

instance ToPath PutUserPolicy where
        toPath = const mempty

instance ToQuery PutUserPolicy where
        toQuery PutUserPolicy'{..}
          = mconcat
              ["Action" =: ("PutUserPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _pupUserName,
               "PolicyName" =: _pupPolicyName,
               "PolicyDocument" =: _pupPolicyDocument]

-- | /See:/ 'putUserPolicyResponse' smart constructor.
data PutUserPolicyResponse =
    PutUserPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutUserPolicyResponse' smart constructor.
putUserPolicyResponse :: PutUserPolicyResponse
putUserPolicyResponse = PutUserPolicyResponse'
