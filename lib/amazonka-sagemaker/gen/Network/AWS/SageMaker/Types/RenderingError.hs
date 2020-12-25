{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderingError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderingError
  ( RenderingError (..),

    -- * Smart constructor
    mkRenderingError,

    -- * Lenses
    reCode,
    reMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.String as Types

-- | A description of an error that occurred while rendering the template.
--
-- /See:/ 'mkRenderingError' smart constructor.
data RenderingError = RenderingError'
  { -- | A unique identifier for a specific class of errors.
    code :: Types.String,
    -- | A human-readable message describing the error.
    message :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenderingError' value with any optional fields omitted.
mkRenderingError ::
  -- | 'code'
  Types.String ->
  -- | 'message'
  Types.String ->
  RenderingError
mkRenderingError code message = RenderingError' {code, message}

-- | A unique identifier for a specific class of errors.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' RenderingError Types.String
reCode = Lens.field @"code"
{-# DEPRECATED reCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A human-readable message describing the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' RenderingError Types.String
reMessage = Lens.field @"message"
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON RenderingError where
  parseJSON =
    Core.withObject "RenderingError" Core.$
      \x ->
        RenderingError'
          Core.<$> (x Core..: "Code") Core.<*> (x Core..: "Message")
