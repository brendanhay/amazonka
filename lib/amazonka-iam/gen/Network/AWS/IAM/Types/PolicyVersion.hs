{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a version of a managed policy.
--
--
-- This data type is used as a response element in the 'CreatePolicyVersion' , 'GetPolicyVersion' , 'ListPolicyVersions' , and 'GetAccountAuthorizationDetails' operations.
--
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- /See:/ 'policyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { _pvVersionId :: !(Maybe Text),
    _pvCreateDate :: !(Maybe ISO8601),
    _pvDocument :: !(Maybe Text),
    _pvIsDefaultVersion :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvVersionId' - The identifier for the policy version. Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
--
-- * 'pvCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
--
-- * 'pvDocument' - The policy document. The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.  The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
--
-- * 'pvIsDefaultVersion' - Specifies whether the policy version is set as the policy's default version.
policyVersion ::
  PolicyVersion
policyVersion =
  PolicyVersion'
    { _pvVersionId = Nothing,
      _pvCreateDate = Nothing,
      _pvDocument = Nothing,
      _pvIsDefaultVersion = Nothing
    }

-- | The identifier for the policy version. Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\s a -> s {_pvVersionId = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\s a -> s {_pvCreateDate = a}) . mapping _Time

-- | The policy document. The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.  The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
pvDocument :: Lens' PolicyVersion (Maybe Text)
pvDocument = lens _pvDocument (\s a -> s {_pvDocument = a})

-- | Specifies whether the policy version is set as the policy's default version.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion = lens _pvIsDefaultVersion (\s a -> s {_pvIsDefaultVersion = a})

instance FromXML PolicyVersion where
  parseXML x =
    PolicyVersion'
      <$> (x .@? "VersionId")
      <*> (x .@? "CreateDate")
      <*> (x .@? "Document")
      <*> (x .@? "IsDefaultVersion")

instance Hashable PolicyVersion

instance NFData PolicyVersion
