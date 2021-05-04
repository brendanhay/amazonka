{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a version of a managed policy.
--
-- This data type is used as a response element in the CreatePolicyVersion,
-- GetPolicyVersion, ListPolicyVersions, and GetAccountAuthorizationDetails
-- operations.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy version was created.
    createDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The identifier for the policy version.
    --
    -- Policy version identifiers always begin with @v@ (always lowercase).
    -- When a policy is created, the first policy version is @v1@.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The policy document.
    --
    -- The policy document is returned in the response to the GetPolicyVersion
    -- and GetAccountAuthorizationDetails operations. It is not returned in the
    -- response to the CreatePolicyVersion or ListPolicyVersions operations.
    --
    -- The policy document returned in this structure is URL-encoded compliant
    -- with <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
    -- decoding method to convert the policy back to plain JSON text. For
    -- example, if you use Java, you can use the @decode@ method of the
    -- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
    -- SDKs provide similar functionality.
    document :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the policy version is set as the policy\'s default
    -- version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'policyVersion_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy version was created.
--
-- 'versionId', 'policyVersion_versionId' - The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase).
-- When a policy is created, the first policy version is @v1@.
--
-- 'document', 'policyVersion_document' - The policy document.
--
-- The policy document is returned in the response to the GetPolicyVersion
-- and GetAccountAuthorizationDetails operations. It is not returned in the
-- response to the CreatePolicyVersion or ListPolicyVersions operations.
--
-- The policy document returned in this structure is URL-encoded compliant
-- with <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
--
-- 'isDefaultVersion', 'policyVersion_isDefaultVersion' - Specifies whether the policy version is set as the policy\'s default
-- version.
newPolicyVersion ::
  PolicyVersion
newPolicyVersion =
  PolicyVersion'
    { createDate = Prelude.Nothing,
      versionId = Prelude.Nothing,
      document = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing
    }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy version was created.
policyVersion_createDate :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.UTCTime)
policyVersion_createDate = Lens.lens (\PolicyVersion' {createDate} -> createDate) (\s@PolicyVersion' {} a -> s {createDate = a} :: PolicyVersion) Prelude.. Lens.mapping Prelude._Time

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase).
-- When a policy is created, the first policy version is @v1@.
policyVersion_versionId :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Text)
policyVersion_versionId = Lens.lens (\PolicyVersion' {versionId} -> versionId) (\s@PolicyVersion' {} a -> s {versionId = a} :: PolicyVersion)

-- | The policy document.
--
-- The policy document is returned in the response to the GetPolicyVersion
-- and GetAccountAuthorizationDetails operations. It is not returned in the
-- response to the CreatePolicyVersion or ListPolicyVersions operations.
--
-- The policy document returned in this structure is URL-encoded compliant
-- with <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
policyVersion_document :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Text)
policyVersion_document = Lens.lens (\PolicyVersion' {document} -> document) (\s@PolicyVersion' {} a -> s {document = a} :: PolicyVersion)

-- | Specifies whether the policy version is set as the policy\'s default
-- version.
policyVersion_isDefaultVersion :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Bool)
policyVersion_isDefaultVersion = Lens.lens (\PolicyVersion' {isDefaultVersion} -> isDefaultVersion) (\s@PolicyVersion' {} a -> s {isDefaultVersion = a} :: PolicyVersion)

instance Prelude.FromXML PolicyVersion where
  parseXML x =
    PolicyVersion'
      Prelude.<$> (x Prelude..@? "CreateDate")
      Prelude.<*> (x Prelude..@? "VersionId")
      Prelude.<*> (x Prelude..@? "Document")
      Prelude.<*> (x Prelude..@? "IsDefaultVersion")

instance Prelude.Hashable PolicyVersion

instance Prelude.NFData PolicyVersion
