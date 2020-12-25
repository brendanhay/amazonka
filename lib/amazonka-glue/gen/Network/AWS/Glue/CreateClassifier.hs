{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a classifier in the user's account. This can be a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field of the request is present.
module Network.AWS.Glue.CreateClassifier
  ( -- * Creating a request
    CreateClassifier (..),
    mkCreateClassifier,

    -- ** Request lenses
    ccCsvClassifier,
    ccGrokClassifier,
    ccJsonClassifier,
    ccXMLClassifier,

    -- * Destructuring the response
    CreateClassifierResponse (..),
    mkCreateClassifierResponse,

    -- ** Response lenses
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateClassifier' smart constructor.
data CreateClassifier = CreateClassifier'
  { -- | A @CsvClassifier@ object specifying the classifier to create.
    csvClassifier :: Core.Maybe Types.CreateCsvClassifierRequest,
    -- | A @GrokClassifier@ object specifying the classifier to create.
    grokClassifier :: Core.Maybe Types.CreateGrokClassifierRequest,
    -- | A @JsonClassifier@ object specifying the classifier to create.
    jsonClassifier :: Core.Maybe Types.CreateJsonClassifierRequest,
    -- | An @XMLClassifier@ object specifying the classifier to create.
    xMLClassifier :: Core.Maybe Types.CreateXMLClassifierRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClassifier' value with any optional fields omitted.
mkCreateClassifier ::
  CreateClassifier
mkCreateClassifier =
  CreateClassifier'
    { csvClassifier = Core.Nothing,
      grokClassifier = Core.Nothing,
      jsonClassifier = Core.Nothing,
      xMLClassifier = Core.Nothing
    }

-- | A @CsvClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCsvClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateCsvClassifierRequest)
ccCsvClassifier = Lens.field @"csvClassifier"
{-# DEPRECATED ccCsvClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A @GrokClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccGrokClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateGrokClassifierRequest)
ccGrokClassifier = Lens.field @"grokClassifier"
{-# DEPRECATED ccGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | A @JsonClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccJsonClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateJsonClassifierRequest)
ccJsonClassifier = Lens.field @"jsonClassifier"
{-# DEPRECATED ccJsonClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

-- | An @XMLClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'xMLClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccXMLClassifier :: Lens.Lens' CreateClassifier (Core.Maybe Types.CreateXMLClassifierRequest)
ccXMLClassifier = Lens.field @"xMLClassifier"
{-# DEPRECATED ccXMLClassifier "Use generic-lens or generic-optics with 'xMLClassifier' instead." #-}

instance Core.FromJSON CreateClassifier where
  toJSON CreateClassifier {..} =
    Core.object
      ( Core.catMaybes
          [ ("CsvClassifier" Core..=) Core.<$> csvClassifier,
            ("GrokClassifier" Core..=) Core.<$> grokClassifier,
            ("JsonClassifier" Core..=) Core.<$> jsonClassifier,
            ("XMLClassifier" Core..=) Core.<$> xMLClassifier
          ]
      )

instance Core.AWSRequest CreateClassifier where
  type Rs CreateClassifier = CreateClassifierResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateClassifier")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateClassifierResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClassifierResponse' smart constructor.
newtype CreateClassifierResponse = CreateClassifierResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClassifierResponse' value with any optional fields omitted.
mkCreateClassifierResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClassifierResponse
mkCreateClassifierResponse responseStatus =
  CreateClassifierResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClassifierResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
