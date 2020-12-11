{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the provisioned concurrency configuration for a function.
module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
  ( -- * Creating a request
    DeleteProvisionedConcurrencyConfig (..),
    mkDeleteProvisionedConcurrencyConfig,

    -- ** Request lenses
    dpccFunctionName,
    dpccQualifier,

    -- * Destructuring the response
    DeleteProvisionedConcurrencyConfigResponse (..),
    mkDeleteProvisionedConcurrencyConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProvisionedConcurrencyConfig' smart constructor.
data DeleteProvisionedConcurrencyConfig = DeleteProvisionedConcurrencyConfig'
  { functionName ::
      Lude.Text,
    qualifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - The version number or alias name.
mkDeleteProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'qualifier'
  Lude.Text ->
  DeleteProvisionedConcurrencyConfig
mkDeleteProvisionedConcurrencyConfig pFunctionName_ pQualifier_ =
  DeleteProvisionedConcurrencyConfig'
    { functionName =
        pFunctionName_,
      qualifier = pQualifier_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpccFunctionName :: Lens.Lens' DeleteProvisionedConcurrencyConfig Lude.Text
dpccFunctionName = Lens.lens (functionName :: DeleteProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {functionName = a} :: DeleteProvisionedConcurrencyConfig)
{-# DEPRECATED dpccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpccQualifier :: Lens.Lens' DeleteProvisionedConcurrencyConfig Lude.Text
dpccQualifier = Lens.lens (qualifier :: DeleteProvisionedConcurrencyConfig -> Lude.Text) (\s a -> s {qualifier = a} :: DeleteProvisionedConcurrencyConfig)
{-# DEPRECATED dpccQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Lude.AWSRequest DeleteProvisionedConcurrencyConfig where
  type
    Rs DeleteProvisionedConcurrencyConfig =
      DeleteProvisionedConcurrencyConfigResponse
  request = Req.delete lambdaService
  response =
    Res.receiveNull DeleteProvisionedConcurrencyConfigResponse'

instance Lude.ToHeaders DeleteProvisionedConcurrencyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteProvisionedConcurrencyConfig where
  toPath DeleteProvisionedConcurrencyConfig' {..} =
    Lude.mconcat
      [ "/2019-09-30/functions/",
        Lude.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Lude.ToQuery DeleteProvisionedConcurrencyConfig where
  toQuery DeleteProvisionedConcurrencyConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkDeleteProvisionedConcurrencyConfigResponse' smart constructor.
data DeleteProvisionedConcurrencyConfigResponse = DeleteProvisionedConcurrencyConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
mkDeleteProvisionedConcurrencyConfigResponse ::
  DeleteProvisionedConcurrencyConfigResponse
mkDeleteProvisionedConcurrencyConfigResponse =
  DeleteProvisionedConcurrencyConfigResponse'
