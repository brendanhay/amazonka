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
-- Module      : Network.AWS.IAM.GetContextKeysForPrincipalPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in all the IAM policies that are attached to the specified IAM entity. The entity can be an IAM user, group, or role. If you specify a user, then the request also includes all of the policies attached to groups that the user is a member of.
--
--
-- You can optionally include a list of one or more additional policies, specified as strings. If you want to include /only/ a list of policies by string, use 'GetContextKeysForCustomPolicy' instead.
--
-- __Note:__ This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use 'GetContextKeysForCustomPolicy' instead.
--
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. Context keys can be evaluated by testing against a value in an IAM policy. Use 'GetContextKeysForPrincipalPolicy' to understand what key names and values you must supply when you call 'SimulatePrincipalPolicy' .
--
module Network.AWS.IAM.GetContextKeysForPrincipalPolicy
    (
    -- * Creating a Request
      getContextKeysForPrincipalPolicy
    , GetContextKeysForPrincipalPolicy
    -- * Request Lenses
    , gckfppPolicyInputList
    , gckfppPolicySourceARN

    -- * Destructuring the Response
    , getContextKeysForPolicyResponse
    , GetContextKeysForPolicyResponse
    -- * Response Lenses
    , gckfpContextKeyNames
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContextKeysForPrincipalPolicy' smart constructor.
data GetContextKeysForPrincipalPolicy = GetContextKeysForPrincipalPolicy'
  { _gckfppPolicyInputList :: !(Maybe [Text])
  , _gckfppPolicySourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContextKeysForPrincipalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gckfppPolicyInputList' - An optional list of additional policies for which you want the list of context keys that are referenced. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
--
-- * 'gckfppPolicySourceARN' - The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
getContextKeysForPrincipalPolicy
    :: Text -- ^ 'gckfppPolicySourceARN'
    -> GetContextKeysForPrincipalPolicy
getContextKeysForPrincipalPolicy pPolicySourceARN_ =
  GetContextKeysForPrincipalPolicy'
    { _gckfppPolicyInputList = Nothing
    , _gckfppPolicySourceARN = pPolicySourceARN_
    }


-- | An optional list of additional policies for which you want the list of context keys that are referenced. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
gckfppPolicyInputList :: Lens' GetContextKeysForPrincipalPolicy [Text]
gckfppPolicyInputList = lens _gckfppPolicyInputList (\ s a -> s{_gckfppPolicyInputList = a}) . _Default . _Coerce

-- | The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
gckfppPolicySourceARN :: Lens' GetContextKeysForPrincipalPolicy Text
gckfppPolicySourceARN = lens _gckfppPolicySourceARN (\ s a -> s{_gckfppPolicySourceARN = a})

instance AWSRequest GetContextKeysForPrincipalPolicy
         where
        type Rs GetContextKeysForPrincipalPolicy =
             GetContextKeysForPolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "GetContextKeysForPrincipalPolicyResult"
              (\ s h x -> parseXML x)

instance Hashable GetContextKeysForPrincipalPolicy
         where

instance NFData GetContextKeysForPrincipalPolicy
         where

instance ToHeaders GetContextKeysForPrincipalPolicy
         where
        toHeaders = const mempty

instance ToPath GetContextKeysForPrincipalPolicy
         where
        toPath = const "/"

instance ToQuery GetContextKeysForPrincipalPolicy
         where
        toQuery GetContextKeysForPrincipalPolicy'{..}
          = mconcat
              ["Action" =:
                 ("GetContextKeysForPrincipalPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyInputList" =:
                 toQuery
                   (toQueryList "member" <$> _gckfppPolicyInputList),
               "PolicySourceArn" =: _gckfppPolicySourceARN]
