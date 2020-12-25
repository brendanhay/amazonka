{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryError
  ( QueryError (..),

    -- * Smart constructor
    mkQueryError,

    -- * Lenses
    qeErrorCode,
    qeMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.QueryErrorCode as Types
import qualified Network.AWS.ResourceGroups.Types.QueryErrorMessage as Types

-- | A two-part error structure that can occur in @ListGroupResources@ or @SearchResources@ operations on CloudFormation stack-based queries. The error occurs if the CloudFormation stack on which the query is based either does not exist, or has a status that renders the stack inactive. A @QueryError@ occurrence does not necessarily mean that AWS Resource Groups could not complete the operation, but the resulting group might have no member resources.
--
-- /See:/ 'mkQueryError' smart constructor.
data QueryError = QueryError'
  { -- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
    errorCode :: Core.Maybe Types.QueryErrorCode,
    -- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
    message :: Core.Maybe Types.QueryErrorMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryError' value with any optional fields omitted.
mkQueryError ::
  QueryError
mkQueryError =
  QueryError' {errorCode = Core.Nothing, message = Core.Nothing}

-- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeErrorCode :: Lens.Lens' QueryError (Core.Maybe Types.QueryErrorCode)
qeErrorCode = Lens.field @"errorCode"
{-# DEPRECATED qeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeMessage :: Lens.Lens' QueryError (Core.Maybe Types.QueryErrorMessage)
qeMessage = Lens.field @"message"
{-# DEPRECATED qeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON QueryError where
  parseJSON =
    Core.withObject "QueryError" Core.$
      \x ->
        QueryError'
          Core.<$> (x Core..:? "ErrorCode") Core.<*> (x Core..:? "Message")
