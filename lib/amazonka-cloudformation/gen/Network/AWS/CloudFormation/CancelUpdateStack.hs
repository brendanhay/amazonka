{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.
module Network.AWS.CloudFormation.CancelUpdateStack
  ( -- * Creating a request
    CancelUpdateStack (..),
    mkCancelUpdateStack,

    -- ** Request lenses
    cusStackName,
    cusClientRequestToken,

    -- * Destructuring the response
    CancelUpdateStackResponse (..),
    mkCancelUpdateStackResponse,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'CancelUpdateStack' action.
--
-- /See:/ 'mkCancelUpdateStack' smart constructor.
data CancelUpdateStack = CancelUpdateStack'
  { -- | The name or the unique stack ID that is associated with the stack.
    stackName :: Types.StackName,
    -- | A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelUpdateStack' value with any optional fields omitted.
mkCancelUpdateStack ::
  -- | 'stackName'
  Types.StackName ->
  CancelUpdateStack
mkCancelUpdateStack stackName =
  CancelUpdateStack' {stackName, clientRequestToken = Core.Nothing}

-- | The name or the unique stack ID that is associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusStackName :: Lens.Lens' CancelUpdateStack Types.StackName
cusStackName = Lens.field @"stackName"
{-# DEPRECATED cusStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | A unique identifier for this @CancelUpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to cancel an update on a stack with the same name. You might retry @CancelUpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cusClientRequestToken :: Lens.Lens' CancelUpdateStack (Core.Maybe Types.ClientRequestToken)
cusClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cusClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Core.AWSRequest CancelUpdateStack where
  type Rs CancelUpdateStack = CancelUpdateStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CancelUpdateStack")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
            )
      }
  response = Response.receiveNull CancelUpdateStackResponse'

-- | /See:/ 'mkCancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse = CancelUpdateStackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelUpdateStackResponse' value with any optional fields omitted.
mkCancelUpdateStackResponse ::
  CancelUpdateStackResponse
mkCancelUpdateStackResponse = CancelUpdateStackResponse'
