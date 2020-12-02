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
-- Module      : Network.AWS.IAM.AttachGroupPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM group.
--
--
-- You use this API to attach a managed policy to a group. To embed an inline policy in a group, use 'PutGroupPolicy' .
--
-- For more information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.AttachGroupPolicy
    (
    -- * Creating a Request
      attachGroupPolicy
    , AttachGroupPolicy
    -- * Request Lenses
    , agpGroupName
    , agpPolicyARN

    -- * Destructuring the Response
    , attachGroupPolicyResponse
    , AttachGroupPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachGroupPolicy' smart constructor.
data AttachGroupPolicy = AttachGroupPolicy'
  { _agpGroupName :: !Text
  , _agpPolicyARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agpGroupName' - The name (friendly name, not ARN) of the group to attach the policy to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'agpPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to attach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
attachGroupPolicy
    :: Text -- ^ 'agpGroupName'
    -> Text -- ^ 'agpPolicyARN'
    -> AttachGroupPolicy
attachGroupPolicy pGroupName_ pPolicyARN_ =
  AttachGroupPolicy' {_agpGroupName = pGroupName_, _agpPolicyARN = pPolicyARN_}


-- | The name (friendly name, not ARN) of the group to attach the policy to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
agpGroupName :: Lens' AttachGroupPolicy Text
agpGroupName = lens _agpGroupName (\ s a -> s{_agpGroupName = a})

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
agpPolicyARN :: Lens' AttachGroupPolicy Text
agpPolicyARN = lens _agpPolicyARN (\ s a -> s{_agpPolicyARN = a})

instance AWSRequest AttachGroupPolicy where
        type Rs AttachGroupPolicy = AttachGroupPolicyResponse
        request = postQuery iam
        response = receiveNull AttachGroupPolicyResponse'

instance Hashable AttachGroupPolicy where

instance NFData AttachGroupPolicy where

instance ToHeaders AttachGroupPolicy where
        toHeaders = const mempty

instance ToPath AttachGroupPolicy where
        toPath = const "/"

instance ToQuery AttachGroupPolicy where
        toQuery AttachGroupPolicy'{..}
          = mconcat
              ["Action" =: ("AttachGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _agpGroupName,
               "PolicyArn" =: _agpPolicyARN]

-- | /See:/ 'attachGroupPolicyResponse' smart constructor.
data AttachGroupPolicyResponse =
  AttachGroupPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachGroupPolicyResponse' with the minimum fields required to make a request.
--
attachGroupPolicyResponse
    :: AttachGroupPolicyResponse
attachGroupPolicyResponse = AttachGroupPolicyResponse'


instance NFData AttachGroupPolicyResponse where
