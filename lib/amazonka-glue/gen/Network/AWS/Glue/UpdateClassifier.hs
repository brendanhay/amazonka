{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing classifier (a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field is present).
module Network.AWS.Glue.UpdateClassifier
  ( -- * Creating a request
    UpdateClassifier (..),
    mkUpdateClassifier,

    -- ** Request lenses
    ucCsvClassifier,
    ucGrokClassifier,
    ucJsonClassifier,
    ucXMLClassifier,

    -- * Destructuring the response
    UpdateClassifierResponse (..),
    mkUpdateClassifierResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateClassifier' smart constructor.
data UpdateClassifier = UpdateClassifier'
  { -- | A @CsvClassifier@ object with updated fields.
    csvClassifier :: Core.Maybe Types.UpdateCsvClassifierRequest,
    -- | A @GrokClassifier@ object with updated fields.
    grokClassifier :: Core.Maybe Types.UpdateGrokClassifierRequest,
    -- | A @JsonClassifier@ object with updated fields.
    jsonClassifier :: Core.Maybe Types.UpdateJsonClassifierRequest,
    -- | An @XMLClassifier@ object with updated fields.
    xMLClassifier :: Core.Maybe Types.UpdateXMLClassifierRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClassifier' value with any optional fields omitted.
mkUpdateClassifier ::
  UpdateClassifier
mkUpdateClassifier =
  UpdateClassifier'
    { csvClassifier = Core.Nothing,
      grokClassifier = Core.Nothing,
      jsonClassifier = Core.Nothing,
      xMLClassifier = Core.Nothing
    }

-- | A @CsvClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCsvClassifier :: Lens.Lens' UpdateClassifier (Core.Maybe Types.UpdateCsvClassifierRequest)
ucCsvClassifier = Lens.field @"csvClassifier"
{-# DEPRECATED ucCsvClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A @GrokClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucGrokClassifier :: Lens.Lens' UpdateClassifier (Core.Maybe Types.UpdateGrokClassifierRequest)
ucGrokClassifier = Lens.field @"grokClassifier"
{-# DEPRECATED ucGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | A @JsonClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucJsonClassifier :: Lens.Lens' UpdateClassifier (Core.Maybe Types.UpdateJsonClassifierRequest)
ucJsonClassifier = Lens.field @"jsonClassifier"
{-# DEPRECATED ucJsonClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

-- | An @XMLClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'xMLClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucXMLClassifier :: Lens.Lens' UpdateClassifier (Core.Maybe Types.UpdateXMLClassifierRequest)
ucXMLClassifier = Lens.field @"xMLClassifier"
{-# DEPRECATED ucXMLClassifier "Use generic-lens or generic-optics with 'xMLClassifier' instead." #-}

instance Core.FromJSON UpdateClassifier where
  toJSON UpdateClassifier {..} =
    Core.object
      ( Core.catMaybes
          [ ("CsvClassifier" Core..=) Core.<$> csvClassifier,
            ("GrokClassifier" Core..=) Core.<$> grokClassifier,
            ("JsonClassifier" Core..=) Core.<$> jsonClassifier,
            ("XMLClassifier" Core..=) Core.<$> xMLClassifier
          ]
      )

instance Core.AWSRequest UpdateClassifier where
  type Rs UpdateClassifier = UpdateClassifierResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateClassifier")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClassifierResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateClassifierResponse' smart constructor.
newtype UpdateClassifierResponse = UpdateClassifierResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateClassifierResponse' value with any optional fields omitted.
mkUpdateClassifierResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateClassifierResponse
mkUpdateClassifierResponse responseStatus =
  UpdateClassifierResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateClassifierResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
