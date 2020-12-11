{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
module Network.AWS.Lambda.DeleteAlias
  ( -- * Creating a request
    DeleteAlias (..),
    mkDeleteAlias,

    -- ** Request lenses
    daFunctionName,
    daName,

    -- * Destructuring the response
    DeleteAliasResponse (..),
    mkDeleteAliasResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { functionName :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
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
-- * 'name' - The name of the alias.
mkDeleteAlias ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeleteAlias
mkDeleteAlias pFunctionName_ pName_ =
  DeleteAlias' {functionName = pFunctionName_, name = pName_}

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
daFunctionName :: Lens.Lens' DeleteAlias Lude.Text
daFunctionName = Lens.lens (functionName :: DeleteAlias -> Lude.Text) (\s a -> s {functionName = a} :: DeleteAlias)
{-# DEPRECATED daFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DeleteAlias Lude.Text
daName = Lens.lens (name :: DeleteAlias -> Lude.Text) (\s a -> s {name = a} :: DeleteAlias)
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteAliasResponse'

instance Lude.ToHeaders DeleteAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAlias where
  toPath DeleteAlias' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/aliases/",
        Lude.toBS name
      ]

instance Lude.ToQuery DeleteAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
mkDeleteAliasResponse ::
  DeleteAliasResponse
mkDeleteAliasResponse = DeleteAliasResponse'
