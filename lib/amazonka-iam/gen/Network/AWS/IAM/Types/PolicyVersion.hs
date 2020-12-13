{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyVersion
  ( PolicyVersion (..),

    -- * Smart constructor
    mkPolicyVersion,

    -- * Lenses
    pvVersionId,
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a version of a managed policy.
--
-- This data type is used as a response element in the 'CreatePolicyVersion' , 'GetPolicyVersion' , 'ListPolicyVersions' , and 'GetAccountAuthorizationDetails' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The identifier for the policy version.
    --
    -- Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
    versionId :: Lude.Maybe Lude.Text,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
    createDate :: Lude.Maybe Lude.DateTime,
    -- | The policy document.
    --
    -- The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.
    -- The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
    document :: Lude.Maybe Lude.Text,
    -- | Specifies whether the policy version is set as the policy's default version.
    isDefaultVersion :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
-- * 'document' - The policy document.
--
-- The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.
-- The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
-- * 'isDefaultVersion' - Specifies whether the policy version is set as the policy's default version.
mkPolicyVersion ::
  PolicyVersion
mkPolicyVersion =
  PolicyVersion'
    { versionId = Lude.Nothing,
      createDate = Lude.Nothing,
      document = Lude.Nothing,
      isDefaultVersion = Lude.Nothing
    }

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVersionId :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Text)
pvVersionId = Lens.lens (versionId :: PolicyVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: PolicyVersion)
{-# DEPRECATED pvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCreateDate :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.DateTime)
pvCreateDate = Lens.lens (createDate :: PolicyVersion -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: PolicyVersion)
{-# DEPRECATED pvCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The policy document.
--
-- The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.
-- The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvDocument :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Text)
pvDocument = Lens.lens (document :: PolicyVersion -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: PolicyVersion)
{-# DEPRECATED pvDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | Specifies whether the policy version is set as the policy's default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvIsDefaultVersion :: Lens.Lens' PolicyVersion (Lude.Maybe Lude.Bool)
pvIsDefaultVersion = Lens.lens (isDefaultVersion :: PolicyVersion -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: PolicyVersion)
{-# DEPRECATED pvIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

instance Lude.FromXML PolicyVersion where
  parseXML x =
    PolicyVersion'
      Lude.<$> (x Lude..@? "VersionId")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "Document")
      Lude.<*> (x Lude..@? "IsDefaultVersion")
