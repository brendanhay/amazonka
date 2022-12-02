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
-- Module      : Amazonka.IAM.Types.PolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | Specifies whether the policy version is set as the policy\'s default
    -- version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy version was created.
    createDate :: Prelude.Maybe Data.ISO8601,
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
    -- | The identifier for the policy version.
    --
    -- Policy version identifiers always begin with @v@ (always lowercase).
    -- When a policy is created, the first policy version is @v1@.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'policyVersion_isDefaultVersion' - Specifies whether the policy version is set as the policy\'s default
-- version.
--
-- 'createDate', 'policyVersion_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy version was created.
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
-- 'versionId', 'policyVersion_versionId' - The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase).
-- When a policy is created, the first policy version is @v1@.
newPolicyVersion ::
  PolicyVersion
newPolicyVersion =
  PolicyVersion'
    { isDefaultVersion = Prelude.Nothing,
      createDate = Prelude.Nothing,
      document = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Specifies whether the policy version is set as the policy\'s default
-- version.
policyVersion_isDefaultVersion :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Bool)
policyVersion_isDefaultVersion = Lens.lens (\PolicyVersion' {isDefaultVersion} -> isDefaultVersion) (\s@PolicyVersion' {} a -> s {isDefaultVersion = a} :: PolicyVersion)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy version was created.
policyVersion_createDate :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.UTCTime)
policyVersion_createDate = Lens.lens (\PolicyVersion' {createDate} -> createDate) (\s@PolicyVersion' {} a -> s {createDate = a} :: PolicyVersion) Prelude.. Lens.mapping Data._Time

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

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase).
-- When a policy is created, the first policy version is @v1@.
policyVersion_versionId :: Lens.Lens' PolicyVersion (Prelude.Maybe Prelude.Text)
policyVersion_versionId = Lens.lens (\PolicyVersion' {versionId} -> versionId) (\s@PolicyVersion' {} a -> s {versionId = a} :: PolicyVersion)

instance Data.FromXML PolicyVersion where
  parseXML x =
    PolicyVersion'
      Prelude.<$> (x Data..@? "IsDefaultVersion")
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> (x Data..@? "Document")
      Prelude.<*> (x Data..@? "VersionId")

instance Prelude.Hashable PolicyVersion where
  hashWithSalt _salt PolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` isDefaultVersion
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData PolicyVersion where
  rnf PolicyVersion' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf versionId
