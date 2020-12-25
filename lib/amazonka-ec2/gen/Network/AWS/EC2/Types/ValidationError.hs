{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationError
  ( ValidationError (..),

    -- * Smart constructor
    mkValidationError,

    -- * Lenses
    veCode,
    veMessage,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The error code and error message that is returned for a parameter or parameter combination that is not valid when a new launch template or new version of a launch template is created.
--
-- /See:/ 'mkValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
    code :: Core.Maybe Types.String,
    -- | The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationError' value with any optional fields omitted.
mkValidationError ::
  ValidationError
mkValidationError =
  ValidationError' {code = Core.Nothing, message = Core.Nothing}

-- | The error code that indicates why the parameter or parameter combination is not valid. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veCode :: Lens.Lens' ValidationError (Core.Maybe Types.String)
veCode = Lens.field @"code"
{-# DEPRECATED veCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message that describes why the parameter or parameter combination is not valid. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veMessage :: Lens.Lens' ValidationError (Core.Maybe Types.String)
veMessage = Lens.field @"message"
{-# DEPRECATED veMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML ValidationError where
  parseXML x =
    ValidationError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
