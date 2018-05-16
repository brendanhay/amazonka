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
-- Module      : Network.AWS.IAM.DetachUserPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified user.
--
--
-- A user can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteUserPolicy' API. For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DetachUserPolicy
    (
    -- * Creating a Request
      detachUserPolicy
    , DetachUserPolicy
    -- * Request Lenses
    , dUserName
    , dPolicyARN

    -- * Destructuring the Response
    , detachUserPolicyResponse
    , DetachUserPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachUserPolicy' smart constructor.
data DetachUserPolicy = DetachUserPolicy'
  { _dUserName  :: !Text
  , _dPolicyARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserName' - The name (friendly name, not ARN) of the IAM user to detach the policy from. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to detach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
detachUserPolicy
    :: Text -- ^ 'dUserName'
    -> Text -- ^ 'dPolicyARN'
    -> DetachUserPolicy
detachUserPolicy pUserName_ pPolicyARN_ =
  DetachUserPolicy' {_dUserName = pUserName_, _dPolicyARN = pPolicyARN_}


-- | The name (friendly name, not ARN) of the IAM user to detach the policy from. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dUserName :: Lens' DetachUserPolicy Text
dUserName = lens _dUserName (\ s a -> s{_dUserName = a})

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
dPolicyARN :: Lens' DetachUserPolicy Text
dPolicyARN = lens _dPolicyARN (\ s a -> s{_dPolicyARN = a})

instance AWSRequest DetachUserPolicy where
        type Rs DetachUserPolicy = DetachUserPolicyResponse
        request = postQuery iam
        response = receiveNull DetachUserPolicyResponse'

instance Hashable DetachUserPolicy where

instance NFData DetachUserPolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachUserPolicyResponse' with the minimum fields required to make a request.
--
detachUserPolicyResponse
    :: DetachUserPolicyResponse
detachUserPolicyResponse = DetachUserPolicyResponse'


instance NFData DetachUserPolicyResponse where
