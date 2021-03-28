{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GenerateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an AWS CloudFormation template based on the current launch configuration and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateTemplate
    (
    -- * Creating a request
      GenerateTemplate (..)
    , mkGenerateTemplate
    -- ** Request lenses
    , gtAppId
    , gtTemplateFormat

    -- * Destructuring the response
    , GenerateTemplateResponse (..)
    , mkGenerateTemplateResponse
    -- ** Response lenses
    , gtrrsS3Location
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGenerateTemplate' smart constructor.
data GenerateTemplate = GenerateTemplate'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application associated with the AWS CloudFormation template.
  , templateFormat :: Core.Maybe Types.OutputFormat
    -- ^ The format for generating the AWS CloudFormation template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateTemplate' value with any optional fields omitted.
mkGenerateTemplate
    :: GenerateTemplate
mkGenerateTemplate
  = GenerateTemplate'{appId = Core.Nothing,
                      templateFormat = Core.Nothing}

-- | The ID of the application associated with the AWS CloudFormation template.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtAppId :: Lens.Lens' GenerateTemplate (Core.Maybe Types.AppId)
gtAppId = Lens.field @"appId"
{-# INLINEABLE gtAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The format for generating the AWS CloudFormation template.
--
-- /Note:/ Consider using 'templateFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateFormat :: Lens.Lens' GenerateTemplate (Core.Maybe Types.OutputFormat)
gtTemplateFormat = Lens.field @"templateFormat"
{-# INLINEABLE gtTemplateFormat #-}
{-# DEPRECATED templateFormat "Use generic-lens or generic-optics with 'templateFormat' instead"  #-}

instance Core.ToQuery GenerateTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateTemplate where
        toHeaders GenerateTemplate{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.GenerateTemplate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GenerateTemplate where
        toJSON GenerateTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("appId" Core..=) Core.<$> appId,
                  ("templateFormat" Core..=) Core.<$> templateFormat])

instance Core.AWSRequest GenerateTemplate where
        type Rs GenerateTemplate = GenerateTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GenerateTemplateResponse' Core.<$>
                   (x Core..:? "s3Location") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGenerateTemplateResponse' smart constructor.
data GenerateTemplateResponse = GenerateTemplateResponse'
  { s3Location :: Core.Maybe Types.S3Location
    -- ^ The location of the Amazon S3 object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateTemplateResponse' value with any optional fields omitted.
mkGenerateTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GenerateTemplateResponse
mkGenerateTemplateResponse responseStatus
  = GenerateTemplateResponse'{s3Location = Core.Nothing,
                              responseStatus}

-- | The location of the Amazon S3 object.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsS3Location :: Lens.Lens' GenerateTemplateResponse (Core.Maybe Types.S3Location)
gtrrsS3Location = Lens.field @"s3Location"
{-# INLINEABLE gtrrsS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GenerateTemplateResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
