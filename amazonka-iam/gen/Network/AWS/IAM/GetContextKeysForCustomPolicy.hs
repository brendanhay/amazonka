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
-- Module      : Network.AWS.IAM.GetContextKeysForCustomPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in the input policies. The policies are supplied as a list of one or more strings. To get the context keys from policies associated with an IAM user, group, or role, use 'GetContextKeysForPrincipalPolicy' .
--
--
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. Context keys can be evaluated by testing against a value specified in an IAM policy. Use @GetContextKeysForCustomPolicy@ to understand what key names and values you must supply when you call 'SimulateCustomPolicy' . Note that all parameters are shown in unencoded form here for clarity but must be URL encoded to be included as a part of a real HTML request.
--
module Network.AWS.IAM.GetContextKeysForCustomPolicy
    (
    -- * Creating a Request
      getContextKeysForCustomPolicy
    , GetContextKeysForCustomPolicy
    -- * Request Lenses
    , gckfcpPolicyInputList

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

-- | /See:/ 'getContextKeysForCustomPolicy' smart constructor.
newtype GetContextKeysForCustomPolicy = GetContextKeysForCustomPolicy'
  { _gckfcpPolicyInputList :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContextKeysForCustomPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gckfcpPolicyInputList' - A list of policies for which you want the list of context keys referenced in those policies. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
getContextKeysForCustomPolicy
    :: GetContextKeysForCustomPolicy
getContextKeysForCustomPolicy =
  GetContextKeysForCustomPolicy' {_gckfcpPolicyInputList = mempty}


-- | A list of policies for which you want the list of context keys referenced in those policies. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
gckfcpPolicyInputList :: Lens' GetContextKeysForCustomPolicy [Text]
gckfcpPolicyInputList = lens _gckfcpPolicyInputList (\ s a -> s{_gckfcpPolicyInputList = a}) . _Coerce

instance AWSRequest GetContextKeysForCustomPolicy
         where
        type Rs GetContextKeysForCustomPolicy =
             GetContextKeysForPolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "GetContextKeysForCustomPolicyResult"
              (\ s h x -> parseXML x)

instance Hashable GetContextKeysForCustomPolicy where

instance NFData GetContextKeysForCustomPolicy where

instance ToHeaders GetContextKeysForCustomPolicy
         where
        toHeaders = const mempty

instance ToPath GetContextKeysForCustomPolicy where
        toPath = const "/"

instance ToQuery GetContextKeysForCustomPolicy where
        toQuery GetContextKeysForCustomPolicy'{..}
          = mconcat
              ["Action" =:
                 ("GetContextKeysForCustomPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyInputList" =:
                 toQueryList "member" _gckfcpPolicyInputList]
