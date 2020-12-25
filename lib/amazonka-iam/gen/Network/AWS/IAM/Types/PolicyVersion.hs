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
    pvCreateDate,
    pvDocument,
    pvIsDefaultVersion,
    pvVersionId,
  )
where

import qualified Network.AWS.IAM.Types.Document as Types
import qualified Network.AWS.IAM.Types.PolicyVersionIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a version of a managed policy.
--
-- This data type is used as a response element in the 'CreatePolicyVersion' , 'GetPolicyVersion' , 'ListPolicyVersions' , and 'GetAccountAuthorizationDetails' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The policy document.
    --
    -- The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.
    -- The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
    document :: Core.Maybe Types.Document,
    -- | Specifies whether the policy version is set as the policy's default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The identifier for the policy version.
    --
    -- Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
    versionId :: Core.Maybe Types.PolicyVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PolicyVersion' value with any optional fields omitted.
mkPolicyVersion ::
  PolicyVersion
mkPolicyVersion =
  PolicyVersion'
    { createDate = Core.Nothing,
      document = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy version was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCreateDate :: Lens.Lens' PolicyVersion (Core.Maybe Core.UTCTime)
pvCreateDate = Lens.field @"createDate"
{-# DEPRECATED pvCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The policy document.
--
-- The policy document is returned in the response to the 'GetPolicyVersion' and 'GetAccountAuthorizationDetails' operations. It is not returned in the response to the 'CreatePolicyVersion' or 'ListPolicyVersions' operations.
-- The policy document returned in this structure is URL-encoded compliant with <https://tools.ietf.org/html/rfc3986 RFC 3986> . You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the @decode@ method of the @java.net.URLDecoder@ utility class in the Java SDK. Other languages and SDKs provide similar functionality.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvDocument :: Lens.Lens' PolicyVersion (Core.Maybe Types.Document)
pvDocument = Lens.field @"document"
{-# DEPRECATED pvDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | Specifies whether the policy version is set as the policy's default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvIsDefaultVersion :: Lens.Lens' PolicyVersion (Core.Maybe Core.Bool)
pvIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED pvIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with @v@ (always lowercase). When a policy is created, the first policy version is @v1@ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVersionId :: Lens.Lens' PolicyVersion (Core.Maybe Types.PolicyVersionIdType)
pvVersionId = Lens.field @"versionId"
{-# DEPRECATED pvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromXML PolicyVersion where
  parseXML x =
    PolicyVersion'
      Core.<$> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Document")
      Core.<*> (x Core..@? "IsDefaultVersion")
      Core.<*> (x Core..@? "VersionId")
