{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function. To delete a specific function version, use the @Qualifier@ parameter. Otherwise, all versions and aliases are deleted.
--
-- To delete Lambda event source mappings that invoke a function, use 'DeleteEventSourceMapping' . For AWS services and resources that invoke your function directly, delete the trigger in the service where you originally configured it.
module Network.AWS.Lambda.DeleteFunction
  ( -- * Creating a request
    DeleteFunction (..),
    mkDeleteFunction,

    -- ** Request lenses
    dfQualifier,
    dfFunctionName,

    -- * Destructuring the response
    DeleteFunctionResponse (..),
    mkDeleteFunctionResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
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

-- | Creates a value of 'DeleteFunction' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function or version.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - Specify a version to delete. You can't delete a version that's referenced by an alias.
mkDeleteFunction ::
  -- | 'functionName'
  Lude.Text ->
  DeleteFunction
mkDeleteFunction pFunctionName_ =
  DeleteFunction'
    { qualifier = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version to delete. You can't delete a version that's referenced by an alias.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfQualifier :: Lens.Lens' DeleteFunction (Lude.Maybe Lude.Text)
dfQualifier = Lens.lens (qualifier :: DeleteFunction -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: DeleteFunction)
{-# DEPRECATED dfQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | The name of the Lambda function or version.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).
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
dfFunctionName :: Lens.Lens' DeleteFunction Lude.Text
dfFunctionName = Lens.lens (functionName :: DeleteFunction -> Lude.Text) (\s a -> s {functionName = a} :: DeleteFunction)
{-# DEPRECATED dfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteFunctionResponse'

instance Lude.ToHeaders DeleteFunction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Lude.mconcat ["/2015-03-31/functions/", Lude.toBS functionName]

instance Lude.ToQuery DeleteFunction where
  toQuery DeleteFunction' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkDeleteFunctionResponse' smart constructor.
data DeleteFunctionResponse = DeleteFunctionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFunctionResponse' with the minimum fields required to make a request.
mkDeleteFunctionResponse ::
  DeleteFunctionResponse
mkDeleteFunctionResponse = DeleteFunctionResponse'
