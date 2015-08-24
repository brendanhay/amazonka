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
-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- GET when calling 'PutUserPolicy'. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutUserPolicy.html AWS API Reference> for PutUserPolicy.
module Network.AWS.IAM.PutUserPolicy
    (
    -- * Creating a Request
      putUserPolicy
    , PutUserPolicy
    -- * Request Lenses
    , pupUserName
    , pupPolicyName
    , pupPolicyDocument

    -- * Destructuring the Response
    , putUserPolicyResponse
    , PutUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putUserPolicy' smart constructor.
data PutUserPolicy = PutUserPolicy'
    { _pupUserName       :: !Text
    , _pupPolicyName     :: !Text
    , _pupPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pupUserName'
--
-- * 'pupPolicyName'
--
-- * 'pupPolicyDocument'
putUserPolicy
    :: Text -- ^ 'pupUserName'
    -> Text -- ^ 'pupPolicyName'
    -> Text -- ^ 'pupPolicyDocument'
    -> PutUserPolicy
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
        type Rs PutUserPolicy = PutUserPolicyResponse
        request = postQuery iAM
        response = receiveNull PutUserPolicyResponse'

instance ToHeaders PutUserPolicy where
        toHeaders = const mempty

instance ToPath PutUserPolicy where
        toPath = const "/"

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

-- | Creates a value of 'PutUserPolicyResponse' with the minimum fields required to make a request.
--
putUserPolicyResponse
    :: PutUserPolicyResponse
putUserPolicyResponse = PutUserPolicyResponse'
