{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DeleteEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entity recognizer.
--
-- Only those recognizers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
-- This is an asynchronous action that puts the recognizer into a DELETING state, and it is then removed by a background job. Once removed, the recognizer disappears from your account and is no longer available for use.
module Network.AWS.Comprehend.DeleteEntityRecognizer
  ( -- * Creating a request
    DeleteEntityRecognizer (..),
    mkDeleteEntityRecognizer,

    -- ** Request lenses
    derEntityRecognizerArn,

    -- * Destructuring the response
    DeleteEntityRecognizerResponse (..),
    mkDeleteEntityRecognizerResponse,

    -- ** Response lenses
    derrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEntityRecognizer' smart constructor.
newtype DeleteEntityRecognizer = DeleteEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Types.EntityRecognizerArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEntityRecognizer' value with any optional fields omitted.
mkDeleteEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Types.EntityRecognizerArn ->
  DeleteEntityRecognizer
mkDeleteEntityRecognizer entityRecognizerArn =
  DeleteEntityRecognizer' {entityRecognizerArn}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEntityRecognizerArn :: Lens.Lens' DeleteEntityRecognizer Types.EntityRecognizerArn
derEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# DEPRECATED derEntityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead." #-}

instance Core.FromJSON DeleteEntityRecognizer where
  toJSON DeleteEntityRecognizer {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EntityRecognizerArn" Core..= entityRecognizerArn)]
      )

instance Core.AWSRequest DeleteEntityRecognizer where
  type Rs DeleteEntityRecognizer = DeleteEntityRecognizerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DeleteEntityRecognizer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEntityRecognizerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEntityRecognizerResponse' smart constructor.
newtype DeleteEntityRecognizerResponse = DeleteEntityRecognizerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEntityRecognizerResponse' value with any optional fields omitted.
mkDeleteEntityRecognizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEntityRecognizerResponse
mkDeleteEntityRecognizerResponse responseStatus =
  DeleteEntityRecognizerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrrsResponseStatus :: Lens.Lens' DeleteEntityRecognizerResponse Core.Int
derrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
