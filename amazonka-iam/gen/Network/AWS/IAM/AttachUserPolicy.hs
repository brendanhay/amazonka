{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified user.
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
    , auprqUserName
    , auprqPolicyARN

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
-- * 'auprqUserName'
--
-- * 'auprqPolicyARN'
data AttachUserPolicy = AttachUserPolicy'
    { _auprqUserName  :: !Text
    , _auprqPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachUserPolicy' smart constructor.
attachUserPolicy :: Text -> Text -> AttachUserPolicy
attachUserPolicy pUserName pPolicyARN =
    AttachUserPolicy'
    { _auprqUserName = pUserName
    , _auprqPolicyARN = pPolicyARN
    }

-- | The name (friendly name, not ARN) of the user to attach the policy to.
auprqUserName :: Lens' AttachUserPolicy Text
auprqUserName = lens _auprqUserName (\ s a -> s{_auprqUserName = a});

-- | FIXME: Undocumented member.
auprqPolicyARN :: Lens' AttachUserPolicy Text
auprqPolicyARN = lens _auprqPolicyARN (\ s a -> s{_auprqPolicyARN = a});

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
               "UserName" =: _auprqUserName,
               "PolicyArn" =: _auprqPolicyARN]

-- | /See:/ 'attachUserPolicyResponse' smart constructor.
data AttachUserPolicyResponse =
    AttachUserPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachUserPolicyResponse' smart constructor.
attachUserPolicyResponse :: AttachUserPolicyResponse
attachUserPolicyResponse = AttachUserPolicyResponse'
