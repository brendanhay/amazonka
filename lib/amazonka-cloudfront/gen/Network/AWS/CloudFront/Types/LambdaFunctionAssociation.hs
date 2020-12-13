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
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,
  )
where

import Network.AWS.CloudFront.Types.EventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains a Lambda function association.
--
-- /See:/ 'mkLambdaFunctionAssociation' smart constructor.
data LambdaFunctionAssociation = LambdaFunctionAssociation'
  { -- | A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
    includeBody :: Lude.Maybe Lude.Bool,
    -- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
    lambdaFunctionARN :: Lude.Text,
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
    eventType :: EventType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionAssociation' with the minimum fields required to make a request.
--
-- * 'includeBody' - A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
-- * 'lambdaFunctionARN' - The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
-- * 'eventType' - Specifies the event type that triggers a Lambda function invocation. You can specify the following values:
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
mkLambdaFunctionAssociation ::
  -- | 'lambdaFunctionARN'
  Lude.Text ->
  -- | 'eventType'
  EventType ->
  LambdaFunctionAssociation
mkLambdaFunctionAssociation pLambdaFunctionARN_ pEventType_ =
  LambdaFunctionAssociation'
    { includeBody = Lude.Nothing,
      lambdaFunctionARN = pLambdaFunctionARN_,
      eventType = pEventType_
    }

-- | A flag that allows a Lambda function to have read access to the body content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/lambda-include-body-access.html Accessing the Request Body by Choosing the Include Body Option> in the Amazon CloudFront Developer Guide.
--
-- /Note:/ Consider using 'includeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaIncludeBody :: Lens.Lens' LambdaFunctionAssociation (Lude.Maybe Lude.Bool)
lfaIncludeBody = Lens.lens (includeBody :: LambdaFunctionAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {includeBody = a} :: LambdaFunctionAssociation)
{-# DEPRECATED lfaIncludeBody "Use generic-lens or generic-optics with 'includeBody' instead." #-}

-- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaLambdaFunctionARN :: Lens.Lens' LambdaFunctionAssociation Lude.Text
lfaLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: LambdaFunctionAssociation -> Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: LambdaFunctionAssociation)
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
lfaEventType :: Lens.Lens' LambdaFunctionAssociation EventType
lfaEventType = Lens.lens (eventType :: LambdaFunctionAssociation -> EventType) (\s a -> s {eventType = a} :: LambdaFunctionAssociation)
{-# DEPRECATED lfaEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

instance Lude.FromXML LambdaFunctionAssociation where
  parseXML x =
    LambdaFunctionAssociation'
      Lude.<$> (x Lude..@? "IncludeBody")
      Lude.<*> (x Lude..@ "LambdaFunctionARN")
      Lude.<*> (x Lude..@ "EventType")

instance Lude.ToXML LambdaFunctionAssociation where
  toXML LambdaFunctionAssociation' {..} =
    Lude.mconcat
      [ "IncludeBody" Lude.@= includeBody,
        "LambdaFunctionARN" Lude.@= lambdaFunctionARN,
        "EventType" Lude.@= eventType
      ]
