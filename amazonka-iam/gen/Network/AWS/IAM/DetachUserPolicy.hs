{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified user.
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
    , dUserName
    , dPolicyARN

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
-- * 'dUserName'
--
-- * 'dPolicyARN'
data DetachUserPolicy = DetachUserPolicy'
    { _dUserName  :: !Text
    , _dPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachUserPolicy' smart constructor.
detachUserPolicy :: Text -> Text -> DetachUserPolicy
detachUserPolicy pUserName_ pPolicyARN_ =
    DetachUserPolicy'
    { _dUserName = pUserName_
    , _dPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the user to detach the policy from.
dUserName :: Lens' DetachUserPolicy Text
dUserName = lens _dUserName (\ s a -> s{_dUserName = a});

-- | FIXME: Undocumented member.
dPolicyARN :: Lens' DetachUserPolicy Text
dPolicyARN = lens _dPolicyARN (\ s a -> s{_dPolicyARN = a});

instance AWSRequest DetachUserPolicy where
        type Sv DetachUserPolicy = IAM
        type Rs DetachUserPolicy = DetachUserPolicyResponse
        request = postQuery
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
               "UserName" =: _dUserName, "PolicyArn" =: _dPolicyARN]

-- | /See:/ 'detachUserPolicyResponse' smart constructor.
data DetachUserPolicyResponse =
    DetachUserPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachUserPolicyResponse' smart constructor.
detachUserPolicyResponse :: DetachUserPolicyResponse
detachUserPolicyResponse = DetachUserPolicyResponse'
