{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectDominantLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <https://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages> .
module Network.AWS.Comprehend.DetectDominantLanguage
  ( -- * Creating a request
    DetectDominantLanguage (..),
    mkDetectDominantLanguage,

    -- ** Request lenses
    ddlText,

    -- * Destructuring the response
    DetectDominantLanguageResponse (..),
    mkDetectDominantLanguageResponse,

    -- ** Response lenses
    ddlrrsLanguages,
    ddlrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetectDominantLanguage' smart constructor.
newtype DetectDominantLanguage = DetectDominantLanguage'
  { -- | A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    text :: Types.CustomerInputString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetectDominantLanguage' value with any optional fields omitted.
mkDetectDominantLanguage ::
  -- | 'text'
  Types.CustomerInputString ->
  DetectDominantLanguage
mkDetectDominantLanguage text = DetectDominantLanguage' {text}

-- | A UTF-8 text string. Each string should contain at least 20 characters and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlText :: Lens.Lens' DetectDominantLanguage Types.CustomerInputString
ddlText = Lens.field @"text"
{-# DEPRECATED ddlText "Use generic-lens or generic-optics with 'text' instead." #-}

instance Core.FromJSON DetectDominantLanguage where
  toJSON DetectDominantLanguage {..} =
    Core.object (Core.catMaybes [Core.Just ("Text" Core..= text)])

instance Core.AWSRequest DetectDominantLanguage where
  type Rs DetectDominantLanguage = DetectDominantLanguageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DetectDominantLanguage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectDominantLanguageResponse'
            Core.<$> (x Core..:? "Languages") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetectDominantLanguageResponse' smart constructor.
data DetectDominantLanguageResponse = DetectDominantLanguageResponse'
  { -- | The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
    languages :: Core.Maybe [Types.DominantLanguage],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetectDominantLanguageResponse' value with any optional fields omitted.
mkDetectDominantLanguageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetectDominantLanguageResponse
mkDetectDominantLanguageResponse responseStatus =
  DetectDominantLanguageResponse'
    { languages = Core.Nothing,
      responseStatus
    }

-- | The languages that Amazon Comprehend detected in the input text. For each language, the response returns the RFC 5646 language code and the level of confidence that Amazon Comprehend has in the accuracy of its inference. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlrrsLanguages :: Lens.Lens' DetectDominantLanguageResponse (Core.Maybe [Types.DominantLanguage])
ddlrrsLanguages = Lens.field @"languages"
{-# DEPRECATED ddlrrsLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlrrsResponseStatus :: Lens.Lens' DetectDominantLanguageResponse Core.Int
ddlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
