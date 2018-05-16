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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM group.
--
--
-- A user can also have managed policies attached to it. To attach a managed policy to a group, use 'AttachGroupPolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- For information about limits on the number of inline policies that you can embed in a group, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
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

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putGroupPolicy' smart constructor.
data PutGroupPolicy = PutGroupPolicy'
  { _pgpGroupName      :: !Text
  , _pgpPolicyName     :: !Text
  , _pgpPolicyDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgpGroupName' - The name of the group to associate the policy with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'pgpPolicyName' - The name of the policy document. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'pgpPolicyDocument' - The policy document. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
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


-- | The name of the group to associate the policy with. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
pgpGroupName :: Lens' PutGroupPolicy Text
pgpGroupName = lens _pgpGroupName (\ s a -> s{_pgpGroupName = a})

-- | The name of the policy document. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
pgpPolicyName :: Lens' PutGroupPolicy Text
pgpPolicyName = lens _pgpPolicyName (\ s a -> s{_pgpPolicyName = a})

-- | The policy document. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
pgpPolicyDocument :: Lens' PutGroupPolicy Text
pgpPolicyDocument = lens _pgpPolicyDocument (\ s a -> s{_pgpPolicyDocument = a})

instance AWSRequest PutGroupPolicy where
        type Rs PutGroupPolicy = PutGroupPolicyResponse
        request = postQuery iam
        response = receiveNull PutGroupPolicyResponse'

instance Hashable PutGroupPolicy where

instance NFData PutGroupPolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutGroupPolicyResponse' with the minimum fields required to make a request.
--
putGroupPolicyResponse
    :: PutGroupPolicyResponse
putGroupPolicyResponse = PutGroupPolicyResponse'


instance NFData PutGroupPolicyResponse where
