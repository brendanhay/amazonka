{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Lambda function.
module Network.AWS.Connect.AssociateLambdaFunction
  ( -- * Creating a request
    AssociateLambdaFunction (..),
    mkAssociateLambdaFunction,

    -- ** Request lenses
    alfInstanceId,
    alfFunctionArn,

    -- * Destructuring the response
    AssociateLambdaFunctionResponse (..),
    mkAssociateLambdaFunctionResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateLambdaFunction' smart constructor.
data AssociateLambdaFunction = AssociateLambdaFunction'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
    functionArn :: Types.FunctionArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateLambdaFunction' value with any optional fields omitted.
mkAssociateLambdaFunction ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'functionArn'
  Types.FunctionArn ->
  AssociateLambdaFunction
mkAssociateLambdaFunction instanceId functionArn =
  AssociateLambdaFunction' {instanceId, functionArn}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfInstanceId :: Lens.Lens' AssociateLambdaFunction Types.InstanceId
alfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED alfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alfFunctionArn :: Lens.Lens' AssociateLambdaFunction Types.FunctionArn
alfFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED alfFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

instance Core.FromJSON AssociateLambdaFunction where
  toJSON AssociateLambdaFunction {..} =
    Core.object
      (Core.catMaybes [Core.Just ("FunctionArn" Core..= functionArn)])

instance Core.AWSRequest AssociateLambdaFunction where
  type Rs AssociateLambdaFunction = AssociateLambdaFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/lambda-function")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AssociateLambdaFunctionResponse'

-- | /See:/ 'mkAssociateLambdaFunctionResponse' smart constructor.
data AssociateLambdaFunctionResponse = AssociateLambdaFunctionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateLambdaFunctionResponse' value with any optional fields omitted.
mkAssociateLambdaFunctionResponse ::
  AssociateLambdaFunctionResponse
mkAssociateLambdaFunctionResponse =
  AssociateLambdaFunctionResponse'
