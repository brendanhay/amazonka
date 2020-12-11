{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the code signing configuration from the function.
module Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
  ( -- * Creating a request
    DeleteFunctionCodeSigningConfig (..),
    mkDeleteFunctionCodeSigningConfig,

    -- ** Request lenses
    dfcscFunctionName,

    -- * Destructuring the response
    DeleteFunctionCodeSigningConfigResponse (..),
    mkDeleteFunctionCodeSigningConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunctionCodeSigningConfig' smart constructor.
newtype DeleteFunctionCodeSigningConfig = DeleteFunctionCodeSigningConfig'
  { functionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
mkDeleteFunctionCodeSigningConfig ::
  -- | 'functionName'
  Lude.Text ->
  DeleteFunctionCodeSigningConfig
mkDeleteFunctionCodeSigningConfig pFunctionName_ =
  DeleteFunctionCodeSigningConfig' {functionName = pFunctionName_}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcscFunctionName :: Lens.Lens' DeleteFunctionCodeSigningConfig Lude.Text
dfcscFunctionName = Lens.lens (functionName :: DeleteFunctionCodeSigningConfig -> Lude.Text) (\s a -> s {functionName = a} :: DeleteFunctionCodeSigningConfig)
{-# DEPRECATED dfcscFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest DeleteFunctionCodeSigningConfig where
  type
    Rs DeleteFunctionCodeSigningConfig =
      DeleteFunctionCodeSigningConfigResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteFunctionCodeSigningConfigResponse'

instance Lude.ToHeaders DeleteFunctionCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFunctionCodeSigningConfig where
  toPath DeleteFunctionCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-06-30/functions/",
        Lude.toBS functionName,
        "/code-signing-config"
      ]

instance Lude.ToQuery DeleteFunctionCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFunctionCodeSigningConfigResponse' smart constructor.
data DeleteFunctionCodeSigningConfigResponse = DeleteFunctionCodeSigningConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionCodeSigningConfigResponse' with the minimum fields required to make a request.
mkDeleteFunctionCodeSigningConfigResponse ::
  DeleteFunctionCodeSigningConfigResponse
mkDeleteFunctionCodeSigningConfigResponse =
  DeleteFunctionCodeSigningConfigResponse'
