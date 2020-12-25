{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LastError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LastError
  ( LastError (..),

    -- * Smart constructor
    mkLastError,

    -- * Lenses
    leCode,
    leMessage,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The last error that occurred for a VPC endpoint.
--
-- /See:/ 'mkLastError' smart constructor.
data LastError = LastError'
  { -- | The error code for the VPC endpoint error.
    code :: Core.Maybe Types.String,
    -- | The error message for the VPC endpoint error.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LastError' value with any optional fields omitted.
mkLastError ::
  LastError
mkLastError =
  LastError' {code = Core.Nothing, message = Core.Nothing}

-- | The error code for the VPC endpoint error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCode :: Lens.Lens' LastError (Core.Maybe Types.String)
leCode = Lens.field @"code"
{-# DEPRECATED leCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message for the VPC endpoint error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMessage :: Lens.Lens' LastError (Core.Maybe Types.String)
leMessage = Lens.field @"message"
{-# DEPRECATED leMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML LastError where
  parseXML x =
    LastError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
