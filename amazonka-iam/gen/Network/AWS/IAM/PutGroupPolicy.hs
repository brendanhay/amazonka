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
-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- in the /IAM User Guide/.
--
-- For information about limits on the number of inline policies that you
-- can embed in a group, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /IAM User Guide/.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling 'PutGroupPolicy'. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html AWS API Reference> for PutGroupPolicy.
module Network.AWS.IAM.PutGroupPolicy
    (
    -- * Creating a Request
      putGroupPolicy
    , PutGroupPolicy
    -- * Request Lenses
    , pgpGroupName
    , pgpPolicyName
    , pgpPolicyDocument

    -- * Destructuring the Response
    , putGroupPolicyResponse
    , PutGroupPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putGroupPolicy' smart constructor.
data PutGroupPolicy = PutGroupPolicy'
    { _pgpGroupName      :: !Text
    , _pgpPolicyName     :: !Text
    , _pgpPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgpGroupName'
--
-- * 'pgpPolicyName'
--
-- * 'pgpPolicyDocument'
putGroupPolicy
    :: Text -- ^ 'pgpGroupName'
    -> Text -- ^ 'pgpPolicyName'
    -> Text -- ^ 'pgpPolicyDocument'
    -> PutGroupPolicy
putGroupPolicy pGroupName_ pPolicyName_ pPolicyDocument_ =
    PutGroupPolicy'
    { _pgpGroupName = pGroupName_
    , _pgpPolicyName = pPolicyName_
    , _pgpPolicyDocument = pPolicyDocument_
    }

-- | The name of the group to associate the policy with.
pgpGroupName :: Lens' PutGroupPolicy Text
pgpGroupName = lens _pgpGroupName (\ s a -> s{_pgpGroupName = a});

-- | The name of the policy document.
pgpPolicyName :: Lens' PutGroupPolicy Text
pgpPolicyName = lens _pgpPolicyName (\ s a -> s{_pgpPolicyName = a});

-- | The policy document.
pgpPolicyDocument :: Lens' PutGroupPolicy Text
pgpPolicyDocument = lens _pgpPolicyDocument (\ s a -> s{_pgpPolicyDocument = a});

instance AWSRequest PutGroupPolicy where
        type Rs PutGroupPolicy = PutGroupPolicyResponse
        request = postQuery iAM
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
               "GroupName" =: _pgpGroupName,
               "PolicyName" =: _pgpPolicyName,
               "PolicyDocument" =: _pgpPolicyDocument]

-- | /See:/ 'putGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse =
    PutGroupPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutGroupPolicyResponse' with the minimum fields required to make a request.
--
putGroupPolicyResponse
    :: PutGroupPolicyResponse
putGroupPolicyResponse = PutGroupPolicyResponse'
