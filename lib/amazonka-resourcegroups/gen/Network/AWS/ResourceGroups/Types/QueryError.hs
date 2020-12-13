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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.QueryErrorCode

-- | A two-part error structure that can occur in @ListGroupResources@ or @SearchResources@ operations on CloudFormation stack-based queries. The error occurs if the CloudFormation stack on which the query is based either does not exist, or has a status that renders the stack inactive. A @QueryError@ occurrence does not necessarily mean that AWS Resource Groups could not complete the operation, but the resulting group might have no member resources.
--
-- /See:/ 'mkQueryError' smart constructor.
data QueryError = QueryError'
  { -- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
    errorCode :: Lude.Maybe QueryErrorCode,
    -- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryError' with the minimum fields required to make a request.
--
-- * 'errorCode' - Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
-- * 'message' - A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
mkQueryError ::
  QueryError
mkQueryError =
  QueryError' {errorCode = Lude.Nothing, message = Lude.Nothing}

-- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeErrorCode :: Lens.Lens' QueryError (Lude.Maybe QueryErrorCode)
qeErrorCode = Lens.lens (errorCode :: QueryError -> Lude.Maybe QueryErrorCode) (\s a -> s {errorCode = a} :: QueryError)
{-# DEPRECATED qeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeMessage :: Lens.Lens' QueryError (Lude.Maybe Lude.Text)
qeMessage = Lens.lens (message :: QueryError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: QueryError)
{-# DEPRECATED qeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON QueryError where
  parseJSON =
    Lude.withObject
      "QueryError"
      ( \x ->
          QueryError'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "Message")
      )
