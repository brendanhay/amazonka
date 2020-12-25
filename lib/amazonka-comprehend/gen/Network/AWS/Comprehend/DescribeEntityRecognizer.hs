{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an entity recognizer including status, S3 buckets containing training data, recognizer metadata, metrics, and so on.
module Network.AWS.Comprehend.DescribeEntityRecognizer
  ( -- * Creating a request
    DescribeEntityRecognizer (..),
    mkDescribeEntityRecognizer,

    -- ** Request lenses
    dEntityRecognizerArn,

    -- * Destructuring the response
    DescribeEntityRecognizerResponse (..),
    mkDescribeEntityRecognizerResponse,

    -- ** Response lenses
    drsEntityRecognizerProperties,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEntityRecognizer' smart constructor.
newtype DescribeEntityRecognizer = DescribeEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Types.EntityRecognizerArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEntityRecognizer' value with any optional fields omitted.
mkDescribeEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Types.EntityRecognizerArn ->
  DescribeEntityRecognizer
mkDescribeEntityRecognizer entityRecognizerArn =
  DescribeEntityRecognizer' {entityRecognizerArn}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEntityRecognizerArn :: Lens.Lens' DescribeEntityRecognizer Types.EntityRecognizerArn
dEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# DEPRECATED dEntityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead." #-}

instance Core.FromJSON DescribeEntityRecognizer where
  toJSON DescribeEntityRecognizer {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EntityRecognizerArn" Core..= entityRecognizerArn)]
      )

instance Core.AWSRequest DescribeEntityRecognizer where
  type Rs DescribeEntityRecognizer = DescribeEntityRecognizerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DescribeEntityRecognizer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntityRecognizerResponse'
            Core.<$> (x Core..:? "EntityRecognizerProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEntityRecognizerResponse' smart constructor.
data DescribeEntityRecognizerResponse = DescribeEntityRecognizerResponse'
  { -- | Describes information associated with an entity recognizer.
    entityRecognizerProperties :: Core.Maybe Types.EntityRecognizerProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEntityRecognizerResponse' value with any optional fields omitted.
mkDescribeEntityRecognizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEntityRecognizerResponse
mkDescribeEntityRecognizerResponse responseStatus =
  DescribeEntityRecognizerResponse'
    { entityRecognizerProperties =
        Core.Nothing,
      responseStatus
    }

-- | Describes information associated with an entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEntityRecognizerProperties :: Lens.Lens' DescribeEntityRecognizerResponse (Core.Maybe Types.EntityRecognizerProperties)
drsEntityRecognizerProperties = Lens.field @"entityRecognizerProperties"
{-# DEPRECATED drsEntityRecognizerProperties "Use generic-lens or generic-optics with 'entityRecognizerProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEntityRecognizerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
