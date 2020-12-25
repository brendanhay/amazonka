{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.PutLexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.
--
-- For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
module Network.AWS.Polly.PutLexicon
  ( -- * Creating a request
    PutLexicon (..),
    mkPutLexicon,

    -- ** Request lenses
    plName,
    plContent,

    -- * Destructuring the response
    PutLexiconResponse (..),
    mkPutLexiconResponse,

    -- ** Response lenses
    plrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { -- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
    name :: Types.LexiconName,
    -- | Content of the PLS lexicon as string data.
    content :: Types.Content
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLexicon' value with any optional fields omitted.
mkPutLexicon ::
  -- | 'name'
  Types.LexiconName ->
  -- | 'content'
  Types.Content ->
  PutLexicon
mkPutLexicon name content = PutLexicon' {name, content}

-- | Name of the lexicon. The name must follow the regular express format [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric string up to 20 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plName :: Lens.Lens' PutLexicon Types.LexiconName
plName = Lens.field @"name"
{-# DEPRECATED plName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Content of the PLS lexicon as string data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plContent :: Lens.Lens' PutLexicon Types.Content
plContent = Lens.field @"content"
{-# DEPRECATED plContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON PutLexicon where
  toJSON PutLexicon {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Content" Core..= content)])

instance Core.AWSRequest PutLexicon where
  type Rs PutLexicon = PutLexiconResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/v1/lexicons/" Core.<> (Core.toText name)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLexiconResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutLexiconResponse' smart constructor.
newtype PutLexiconResponse = PutLexiconResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutLexiconResponse' value with any optional fields omitted.
mkPutLexiconResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutLexiconResponse
mkPutLexiconResponse responseStatus =
  PutLexiconResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plrrsResponseStatus :: Lens.Lens' PutLexiconResponse Core.Int
plrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
