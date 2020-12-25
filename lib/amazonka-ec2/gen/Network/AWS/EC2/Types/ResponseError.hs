{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResponseError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseError
  ( ResponseError (..),

    -- * Smart constructor
    mkResponseError,

    -- * Lenses
    reCode,
    reMessage,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateErrorCode as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the error that's returned when you cannot delete a launch template version.
--
-- /See:/ 'mkResponseError' smart constructor.
data ResponseError = ResponseError'
  { -- | The error code.
    code :: Core.Maybe Types.LaunchTemplateErrorCode,
    -- | The error message, if applicable.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResponseError' value with any optional fields omitted.
mkResponseError ::
  ResponseError
mkResponseError =
  ResponseError' {code = Core.Nothing, message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' ResponseError (Core.Maybe Types.LaunchTemplateErrorCode)
reCode = Lens.field @"code"
{-# DEPRECATED reCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' ResponseError (Core.Maybe Types.String)
reMessage = Lens.field @"message"
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML ResponseError where
  parseXML x =
    ResponseError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
