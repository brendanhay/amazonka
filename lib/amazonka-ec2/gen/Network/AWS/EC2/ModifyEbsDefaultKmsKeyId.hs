{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the default customer master key (CMK) for EBS encryption by default for your account in this Region.
--
-- AWS creates a unique AWS managed CMK in each Region for use with encryption by default. If you change the default CMK to a symmetric customer managed CMK, it is used instead of the AWS managed CMK. To reset the default CMK to the AWS managed CMK for EBS, use 'ResetEbsDefaultKmsKeyId' . Amazon EBS does not support asymmetric CMKs.
-- If you delete or disable the customer managed CMK that you specified for use with encryption by default, your instances will fail to launch.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
    (
    -- * Creating a request
      ModifyEbsDefaultKmsKeyId (..)
    , mkModifyEbsDefaultKmsKeyId
    -- ** Request lenses
    , medkkiKmsKeyId
    , medkkiDryRun

    -- * Destructuring the response
    , ModifyEbsDefaultKmsKeyIdResponse (..)
    , mkModifyEbsDefaultKmsKeyIdResponse
    -- ** Response lenses
    , medkkirrsKmsKeyId
    , medkkirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyEbsDefaultKmsKeyId' smart constructor.
data ModifyEbsDefaultKmsKeyId = ModifyEbsDefaultKmsKeyId'
  { kmsKeyId :: Types.KmsKeyId
    -- ^ The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- Amazon EBS does not support asymmetric CMKs.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEbsDefaultKmsKeyId' value with any optional fields omitted.
mkModifyEbsDefaultKmsKeyId
    :: Types.KmsKeyId -- ^ 'kmsKeyId'
    -> ModifyEbsDefaultKmsKeyId
mkModifyEbsDefaultKmsKeyId kmsKeyId
  = ModifyEbsDefaultKmsKeyId'{kmsKeyId, dryRun = Core.Nothing}

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkiKmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyId Types.KmsKeyId
medkkiKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE medkkiKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkiDryRun :: Lens.Lens' ModifyEbsDefaultKmsKeyId (Core.Maybe Core.Bool)
medkkiDryRun = Lens.field @"dryRun"
{-# INLINEABLE medkkiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyEbsDefaultKmsKeyId where
        toQuery ModifyEbsDefaultKmsKeyId{..}
          = Core.toQueryPair "Action"
              ("ModifyEbsDefaultKmsKeyId" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "KmsKeyId" kmsKeyId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyEbsDefaultKmsKeyId where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyEbsDefaultKmsKeyId where
        type Rs ModifyEbsDefaultKmsKeyId = ModifyEbsDefaultKmsKeyIdResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyEbsDefaultKmsKeyIdResponse' Core.<$>
                   (x Core..@? "kmsKeyId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyEbsDefaultKmsKeyIdResponse' smart constructor.
data ModifyEbsDefaultKmsKeyIdResponse = ModifyEbsDefaultKmsKeyIdResponse'
  { kmsKeyId :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the default CMK for encryption by default.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEbsDefaultKmsKeyIdResponse' value with any optional fields omitted.
mkModifyEbsDefaultKmsKeyIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyEbsDefaultKmsKeyIdResponse
mkModifyEbsDefaultKmsKeyIdResponse responseStatus
  = ModifyEbsDefaultKmsKeyIdResponse'{kmsKeyId = Core.Nothing,
                                      responseStatus}

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkirrsKmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse (Core.Maybe Core.Text)
medkkirrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE medkkirrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkirrsResponseStatus :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse Core.Int
medkkirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE medkkirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
