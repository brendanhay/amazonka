{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateWebACLMigrationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation WAFV2 template for the specified web ACL in the specified Amazon S3 bucket. Then, in CloudFormation, you create a stack from the template, to create the web ACL and its resources in AWS WAFV2. Use this to migrate your AWS WAF Classic web ACL to the latest version of AWS WAF.
--
-- This is part of a larger migration procedure for web ACLs from AWS WAF Classic to the latest version of AWS WAF. For the full procedure, including caveats and manual steps to complete the migration and switch over to the new web ACL, see <https://docs.aws.amazon.com/waf/latest/developerguide/waf-migrating-from-classic.html Migrating your AWS WAF Classic resources to AWS WAF> in the <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide> . 
module Network.AWS.WAFRegional.CreateWebACLMigrationStack
    (
    -- * Creating a request
      CreateWebACLMigrationStack (..)
    , mkCreateWebACLMigrationStack
    -- ** Request lenses
    , cwaclmsWebACLId
    , cwaclmsS3BucketName
    , cwaclmsIgnoreUnsupportedType

    -- * Destructuring the response
    , CreateWebACLMigrationStackResponse (..)
    , mkCreateWebACLMigrationStackResponse
    -- ** Response lenses
    , cwaclmsrrsS3ObjectUrl
    , cwaclmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkCreateWebACLMigrationStack' smart constructor.
data CreateWebACLMigrationStack = CreateWebACLMigrationStack'
  { webACLId :: Types.ResourceId
    -- ^ The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
  , s3BucketName :: Types.S3BucketName
    -- ^ The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration: 
--
--
--     * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .
--
--
--     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2. 
--
--
--     * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section. 
--
--
  , ignoreUnsupportedType :: Core.Bool
    -- ^ Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWebACLMigrationStack' value with any optional fields omitted.
mkCreateWebACLMigrationStack
    :: Types.ResourceId -- ^ 'webACLId'
    -> Types.S3BucketName -- ^ 's3BucketName'
    -> Core.Bool -- ^ 'ignoreUnsupportedType'
    -> CreateWebACLMigrationStack
mkCreateWebACLMigrationStack webACLId s3BucketName
  ignoreUnsupportedType
  = CreateWebACLMigrationStack'{webACLId, s3BucketName,
                                ignoreUnsupportedType}

-- | The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclmsWebACLId :: Lens.Lens' CreateWebACLMigrationStack Types.ResourceId
cwaclmsWebACLId = Lens.field @"webACLId"
{-# INLINEABLE cwaclmsWebACLId #-}
{-# DEPRECATED webACLId "Use generic-lens or generic-optics with 'webACLId' instead"  #-}

-- | The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration: 
--
--
--     * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .
--
--
--     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2. 
--
--
--     * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section. 
--
--
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclmsS3BucketName :: Lens.Lens' CreateWebACLMigrationStack Types.S3BucketName
cwaclmsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE cwaclmsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception. 
--
-- /Note:/ Consider using 'ignoreUnsupportedType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclmsIgnoreUnsupportedType :: Lens.Lens' CreateWebACLMigrationStack Core.Bool
cwaclmsIgnoreUnsupportedType = Lens.field @"ignoreUnsupportedType"
{-# INLINEABLE cwaclmsIgnoreUnsupportedType #-}
{-# DEPRECATED ignoreUnsupportedType "Use generic-lens or generic-optics with 'ignoreUnsupportedType' instead"  #-}

instance Core.ToQuery CreateWebACLMigrationStack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateWebACLMigrationStack where
        toHeaders CreateWebACLMigrationStack{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSWAF_Regional_20161128.CreateWebACLMigrationStack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateWebACLMigrationStack where
        toJSON CreateWebACLMigrationStack{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WebACLId" Core..= webACLId),
                  Core.Just ("S3BucketName" Core..= s3BucketName),
                  Core.Just ("IgnoreUnsupportedType" Core..= ignoreUnsupportedType)])

instance Core.AWSRequest CreateWebACLMigrationStack where
        type Rs CreateWebACLMigrationStack =
             CreateWebACLMigrationStackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateWebACLMigrationStackResponse' Core.<$>
                   (x Core..: "S3ObjectUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateWebACLMigrationStackResponse' smart constructor.
data CreateWebACLMigrationStackResponse = CreateWebACLMigrationStackResponse'
  { s3ObjectUrl :: Types.S3ObjectUrl
    -- ^ The URL of the template created in Amazon S3. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWebACLMigrationStackResponse' value with any optional fields omitted.
mkCreateWebACLMigrationStackResponse
    :: Types.S3ObjectUrl -- ^ 's3ObjectUrl'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateWebACLMigrationStackResponse
mkCreateWebACLMigrationStackResponse s3ObjectUrl responseStatus
  = CreateWebACLMigrationStackResponse'{s3ObjectUrl, responseStatus}

-- | The URL of the template created in Amazon S3. 
--
-- /Note:/ Consider using 's3ObjectUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclmsrrsS3ObjectUrl :: Lens.Lens' CreateWebACLMigrationStackResponse Types.S3ObjectUrl
cwaclmsrrsS3ObjectUrl = Lens.field @"s3ObjectUrl"
{-# INLINEABLE cwaclmsrrsS3ObjectUrl #-}
{-# DEPRECATED s3ObjectUrl "Use generic-lens or generic-optics with 's3ObjectUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaclmsrrsResponseStatus :: Lens.Lens' CreateWebACLMigrationStackResponse Core.Int
cwaclmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cwaclmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
