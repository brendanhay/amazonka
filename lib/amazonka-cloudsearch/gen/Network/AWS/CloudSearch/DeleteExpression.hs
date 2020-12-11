{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @'Expression' @ from the search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteExpression
  ( -- * Creating a request
    DeleteExpression (..),
    mkDeleteExpression,

    -- ** Request lenses
    delDomainName,
    delExpressionName,

    -- * Destructuring the response
    DeleteExpressionResponse (..),
    mkDeleteExpressionResponse,

    -- ** Response lenses
    delrsResponseStatus,
    delrsExpression,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteExpression' @ operation. Specifies the name of the domain you want to update and the name of the expression you want to delete.
--
-- /See:/ 'mkDeleteExpression' smart constructor.
data DeleteExpression = DeleteExpression'
  { domainName :: Lude.Text,
    expressionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteExpression' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
-- * 'expressionName' - The name of the @'Expression' @ to delete.
mkDeleteExpression ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'expressionName'
  Lude.Text ->
  DeleteExpression
mkDeleteExpression pDomainName_ pExpressionName_ =
  DeleteExpression'
    { domainName = pDomainName_,
      expressionName = pExpressionName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delDomainName :: Lens.Lens' DeleteExpression Lude.Text
delDomainName = Lens.lens (domainName :: DeleteExpression -> Lude.Text) (\s a -> s {domainName = a} :: DeleteExpression)
{-# DEPRECATED delDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the @'Expression' @ to delete.
--
-- /Note:/ Consider using 'expressionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delExpressionName :: Lens.Lens' DeleteExpression Lude.Text
delExpressionName = Lens.lens (expressionName :: DeleteExpression -> Lude.Text) (\s a -> s {expressionName = a} :: DeleteExpression)
{-# DEPRECATED delExpressionName "Use generic-lens or generic-optics with 'expressionName' instead." #-}

instance Lude.AWSRequest DeleteExpression where
  type Rs DeleteExpression = DeleteExpressionResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteExpressionResult"
      ( \s h x ->
          DeleteExpressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Expression")
      )

instance Lude.ToHeaders DeleteExpression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteExpression where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteExpression where
  toQuery DeleteExpression' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteExpression" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "ExpressionName" Lude.=: expressionName
      ]

-- | The result of a @'DeleteExpression' @ request. Specifies the expression being deleted.
--
-- /See:/ 'mkDeleteExpressionResponse' smart constructor.
data DeleteExpressionResponse = DeleteExpressionResponse'
  { responseStatus ::
      Lude.Int,
    expression :: ExpressionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteExpressionResponse' with the minimum fields required to make a request.
--
-- * 'expression' - The status of the expression being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteExpressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'expression'
  ExpressionStatus ->
  DeleteExpressionResponse
mkDeleteExpressionResponse pResponseStatus_ pExpression_ =
  DeleteExpressionResponse'
    { responseStatus = pResponseStatus_,
      expression = pExpression_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteExpressionResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteExpressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteExpressionResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The status of the expression being deleted.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsExpression :: Lens.Lens' DeleteExpressionResponse ExpressionStatus
delrsExpression = Lens.lens (expression :: DeleteExpressionResponse -> ExpressionStatus) (\s a -> s {expression = a} :: DeleteExpressionResponse)
{-# DEPRECATED delrsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}
