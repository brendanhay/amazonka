{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociation
  ( LambdaFunctionAssociation (..),

    -- * Smart constructor
    mkLambdaFunctionAssociation,

    -- * Lenses
    lfaLambdaFunctionARN,
    lfaEventType,
    lfaIncludeBody,
  )
where

import qualified Network.AWS.CloudFront.Types.EventType as Types
import qualified Network.AWS.CloudFront.Types.LambdaFunctionARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains a Lambda function association.
--
-- /See:/ 'mkLambdaFunctionAssociation' smart constructor.
data LambdaFunctionAssociation = LambdaFunctionAssociation'
  { -- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
    lambdaFunctionARN :: Types.LambdaFunctionARN,
    -- | Specifies the event type that triggers a Lambda function invocation. You can specify the following values:
    --
    --
    --     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.
    --
    --
    --     * @origin-request@ : The function executes only when CloudFront sends a request to your origin. When the requested object is in the edge cache, the function doesn't execute.
    --
    --
    --     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute.
    --
    --
    --     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache.
    -- If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
    eventType :: Types.EventType,
    -- | A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
    includeBody :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionAssociation' value with any optional fields omitted.
mkLambdaFunctionAssociation ::
  -- | 'lambdaFunctionARN'
  Types.LambdaFunctionARN ->
  -- | 'eventType'
  Types.EventType ->
  LambdaFunctionAssociation
mkLambdaFunctionAssociation lambdaFunctionARN eventType =
  LambdaFunctionAssociation'
    { lambdaFunctionARN,
      eventType,
      includeBody = Core.Nothing
    }

-- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaLambdaFunctionARN :: Lens.Lens' LambdaFunctionAssociation Types.LambdaFunctionARN
lfaLambdaFunctionARN = Lens.field @"lambdaFunctionARN"
{-# DEPRECATED lfaLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

-- | Specifies the event type that triggers a Lambda function invocation. You can specify the following values:
--
--
--     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.
--
--
--     * @origin-request@ : The function executes only when CloudFront sends a request to your origin. When the requested object is in the edge cache, the function doesn't execute.
--
--
--     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute.
--
--
--     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache.
-- If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
--
--
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaEventType :: Lens.Lens' LambdaFunctionAssociation Types.EventType
lfaEventType = Lens.field @"eventType"
{-# DEPRECATED lfaEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
--
-- /Note:/ Consider using 'includeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaIncludeBody :: Lens.Lens' LambdaFunctionAssociation (Core.Maybe Core.Bool)
lfaIncludeBody = Lens.field @"includeBody"
{-# DEPRECATED lfaIncludeBody "Use generic-lens or generic-optics with 'includeBody' instead." #-}

instance Core.ToXML LambdaFunctionAssociation where
  toXML LambdaFunctionAssociation {..} =
    Core.toXMLNode "LambdaFunctionARN" lambdaFunctionARN
      Core.<> Core.toXMLNode "EventType" eventType
      Core.<> Core.toXMLNode "IncludeBody" Core.<$> includeBody

instance Core.FromXML LambdaFunctionAssociation where
  parseXML x =
    LambdaFunctionAssociation'
      Core.<$> (x Core..@ "LambdaFunctionARN")
      Core.<*> (x Core..@ "EventType")
      Core.<*> (x Core..@? "IncludeBody")
