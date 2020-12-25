{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the Lambda function from the drop-down options available in the relevant contact flow blocks.
module Network.AWS.Connect.DisassociateLambdaFunction
  ( -- * Creating a request
    DisassociateLambdaFunction (..),
    mkDisassociateLambdaFunction,

    -- ** Request lenses
    dlfInstanceId,
    dlfFunctionArn,

    -- * Destructuring the response
    DisassociateLambdaFunctionResponse (..),
    mkDisassociateLambdaFunctionResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateLambdaFunction' smart constructor.
data DisassociateLambdaFunction = DisassociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance..
    instanceId :: Types.InstanceId,
    -- | The Amazon Resource Name (ARN) of the Lambda function being disassociated.
    functionArn :: Types.FunctionArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLambdaFunction' value with any optional fields omitted.
mkDisassociateLambdaFunction ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'functionArn'
  Types.FunctionArn ->
  DisassociateLambdaFunction
mkDisassociateLambdaFunction instanceId functionArn =
  DisassociateLambdaFunction' {instanceId, functionArn}

-- | The identifier of the Amazon Connect instance..
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfInstanceId :: Lens.Lens' DisassociateLambdaFunction Types.InstanceId
dlfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dlfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Lambda function being disassociated.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlfFunctionArn :: Lens.Lens' DisassociateLambdaFunction Types.FunctionArn
dlfFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED dlfFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

instance Core.AWSRequest DisassociateLambdaFunction where
  type
    Rs DisassociateLambdaFunction =
      DisassociateLambdaFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/lambda-function")
            ),
        Core._rqQuery = Core.toQueryValue "functionArn" functionArn,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response = Response.receiveNull DisassociateLambdaFunctionResponse'

-- | /See:/ 'mkDisassociateLambdaFunctionResponse' smart constructor.
data DisassociateLambdaFunctionResponse = DisassociateLambdaFunctionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLambdaFunctionResponse' value with any optional fields omitted.
mkDisassociateLambdaFunctionResponse ::
  DisassociateLambdaFunctionResponse
mkDisassociateLambdaFunctionResponse =
  DisassociateLambdaFunctionResponse'
