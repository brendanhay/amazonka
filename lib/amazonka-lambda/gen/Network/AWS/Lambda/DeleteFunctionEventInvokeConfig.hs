{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration for asynchronous invocation for a function, version, or alias.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
  ( -- * Creating a request
    DeleteFunctionEventInvokeConfig (..),
    mkDeleteFunctionEventInvokeConfig,

    -- ** Request lenses
    dfeicQualifier,
    dfeicFunctionName,

    -- * Destructuring the response
    DeleteFunctionEventInvokeConfigResponse (..),
    mkDeleteFunctionEventInvokeConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunctionEventInvokeConfig' smart constructor.
data DeleteFunctionEventInvokeConfig = DeleteFunctionEventInvokeConfig'
  { qualifier ::
      Lude.Maybe Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - A version number or alias name.
mkDeleteFunctionEventInvokeConfig ::
  -- | 'functionName'
  Lude.Text ->
  DeleteFunctionEventInvokeConfig
mkDeleteFunctionEventInvokeConfig pFunctionName_ =
  DeleteFunctionEventInvokeConfig'
    { qualifier = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeicQualifier :: Lens.Lens' DeleteFunctionEventInvokeConfig (Lude.Maybe Lude.Text)
dfeicQualifier = Lens.lens (qualifier :: DeleteFunctionEventInvokeConfig -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: DeleteFunctionEventInvokeConfig)
{-# DEPRECATED dfeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeicFunctionName :: Lens.Lens' DeleteFunctionEventInvokeConfig Lude.Text
dfeicFunctionName = Lens.lens (functionName :: DeleteFunctionEventInvokeConfig -> Lude.Text) (\s a -> s {functionName = a} :: DeleteFunctionEventInvokeConfig)
{-# DEPRECATED dfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest DeleteFunctionEventInvokeConfig where
  type
    Rs DeleteFunctionEventInvokeConfig =
      DeleteFunctionEventInvokeConfigResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteFunctionEventInvokeConfigResponse'

instance Lude.ToHeaders DeleteFunctionEventInvokeConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFunctionEventInvokeConfig where
  toPath DeleteFunctionEventInvokeConfig' {..} =
    Lude.mconcat
      [ "/2019-09-25/functions/",
        Lude.toBS functionName,
        "/event-invoke-config"
      ]

instance Lude.ToQuery DeleteFunctionEventInvokeConfig where
  toQuery DeleteFunctionEventInvokeConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkDeleteFunctionEventInvokeConfigResponse' smart constructor.
data DeleteFunctionEventInvokeConfigResponse = DeleteFunctionEventInvokeConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionEventInvokeConfigResponse' with the minimum fields required to make a request.
mkDeleteFunctionEventInvokeConfigResponse ::
  DeleteFunctionEventInvokeConfigResponse
mkDeleteFunctionEventInvokeConfigResponse =
  DeleteFunctionEventInvokeConfigResponse'
