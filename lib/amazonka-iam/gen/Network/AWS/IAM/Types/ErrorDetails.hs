{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edMessage,
    edCode,
  )
where

import qualified Network.AWS.IAM.Types.Code as Types
import qualified Network.AWS.IAM.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the reason that the operation failed.
--
-- This data type is used as a response element in the 'GetOrganizationsAccessReport' , 'GetServiceLastAccessedDetails' , and 'GetServiceLastAccessedDetailsWithEntities' operations.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | Detailed information about the reason that the operation failed.
    message :: Types.Message,
    -- | The error code associated with the operation failure.
    code :: Types.Code
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetails' value with any optional fields omitted.
mkErrorDetails ::
  -- | 'message'
  Types.Message ->
  -- | 'code'
  Types.Code ->
  ErrorDetails
mkErrorDetails message code = ErrorDetails' {message, code}

-- | Detailed information about the reason that the operation failed.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' ErrorDetails Types.Message
edMessage = Lens.field @"message"
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The error code associated with the operation failure.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCode :: Lens.Lens' ErrorDetails Types.Code
edCode = Lens.field @"code"
{-# DEPRECATED edCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Core.FromXML ErrorDetails where
  parseXML x =
    ErrorDetails'
      Core.<$> (x Core..@ "Message") Core.<*> (x Core..@ "Code")
