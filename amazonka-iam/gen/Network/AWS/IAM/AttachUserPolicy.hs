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
-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachUserPolicy.html AWS API Reference> for AttachUserPolicy.
module Network.AWS.IAM.AttachUserPolicy
    (
    -- * Creating a Request
      attachUserPolicy
    , AttachUserPolicy
    -- * Request Lenses
    , aupUserName
    , aupPolicyARN

    -- * Destructuring the Response
    , attachUserPolicyResponse
    , AttachUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachUserPolicy' smart constructor.
data AttachUserPolicy = AttachUserPolicy'
    { _aupUserName  :: !Text
    , _aupPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aupUserName'
--
-- * 'aupPolicyARN'
attachUserPolicy
    :: Text -- ^ 'aupUserName'
    -> Text -- ^ 'aupPolicyARN'
    -> AttachUserPolicy
attachUserPolicy pUserName_ pPolicyARN_ =
    AttachUserPolicy'
    { _aupUserName = pUserName_
    , _aupPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the user to attach the policy to.
aupUserName :: Lens' AttachUserPolicy Text
aupUserName = lens _aupUserName (\ s a -> s{_aupUserName = a});

-- | Undocumented member.
aupPolicyARN :: Lens' AttachUserPolicy Text
aupPolicyARN = lens _aupPolicyARN (\ s a -> s{_aupPolicyARN = a});

instance AWSRequest AttachUserPolicy where
        type Rs AttachUserPolicy = AttachUserPolicyResponse
        request = postQuery iAM
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

-- | Creates a value of 'AttachUserPolicyResponse' with the minimum fields required to make a request.
--
attachUserPolicyResponse
    :: AttachUserPolicyResponse
attachUserPolicyResponse = AttachUserPolicyResponse'
