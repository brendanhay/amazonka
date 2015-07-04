{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.IAM.AttachUserPolicy
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

-- | Attaches the specified managed policy to the specified user.
--
-- You use this API to attach a managed policy to a user. To embed an
-- inline policy in a user, use PutUserPolicy.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachUserPolicy.html>
module Network.AWS.IAM.AttachUserPolicy
    (
    -- * Request
      AttachUserPolicy
    -- ** Request constructor
    , attachUserPolicy
    -- ** Request lenses
    , aupUserName
    , aupPolicyARN

    -- * Response
    , AttachUserPolicyResponse
    -- ** Response constructor
    , attachUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aupUserName'
--
-- * 'aupPolicyARN'
data AttachUserPolicy = AttachUserPolicy'
    { _aupUserName  :: !Text
    , _aupPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachUserPolicy' smart constructor.
attachUserPolicy :: Text -> Text -> AttachUserPolicy
attachUserPolicy pUserName pPolicyARN =
    AttachUserPolicy'
    { _aupUserName = pUserName
    , _aupPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the user to attach the policy to.
aupUserName :: Lens' AttachUserPolicy Text
aupUserName = lens _aupUserName (\ s a -> s{_aupUserName = a});

-- | FIXME: Undocumented member.
aupPolicyARN :: Lens' AttachUserPolicy Text
aupPolicyARN = lens _aupPolicyARN (\ s a -> s{_aupPolicyARN = a});

instance AWSRequest AttachUserPolicy where
        type Sv AttachUserPolicy = IAM
        type Rs AttachUserPolicy = AttachUserPolicyResponse
        request = post
        response = receiveNull AttachUserPolicyResponse'

instance ToHeaders AttachUserPolicy where
        toHeaders = const mempty

instance ToPath AttachUserPolicy where
        toPath = const "/"

instance ToQuery AttachUserPolicy where
        toQuery AttachUserPolicy'{..}
          = mconcat
              ["Action" =: ("AttachUserPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _aupUserName,
               "PolicyArn" =: _aupPolicyARN]

-- | /See:/ 'attachUserPolicyResponse' smart constructor.
data AttachUserPolicyResponse =
    AttachUserPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachUserPolicyResponse' smart constructor.
attachUserPolicyResponse :: AttachUserPolicyResponse
attachUserPolicyResponse = AttachUserPolicyResponse'
