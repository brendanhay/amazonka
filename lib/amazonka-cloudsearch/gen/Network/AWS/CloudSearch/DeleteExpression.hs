{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    deExpressionName,
    deDomainName,

    -- * Destructuring the response
    DeleteExpressionResponse (..),
    mkDeleteExpressionResponse,

    -- ** Response lenses
    defrsExpression,
    defrsResponseStatus,
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
  { -- | The name of the @'Expression' @ to delete.
    expressionName :: Lude.Text,
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteExpression' with the minimum fields required to make a request.
--
-- * 'expressionName' - The name of the @'Expression' @ to delete.
-- * 'domainName' -
mkDeleteExpression ::
  -- | 'expressionName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  DeleteExpression
mkDeleteExpression pExpressionName_ pDomainName_ =
  DeleteExpression'
    { expressionName = pExpressionName_,
      domainName = pDomainName_
    }

-- | The name of the @'Expression' @ to delete.
--
-- /Note:/ Consider using 'expressionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExpressionName :: Lens.Lens' DeleteExpression Lude.Text
deExpressionName = Lens.lens (expressionName :: DeleteExpression -> Lude.Text) (\s a -> s {expressionName = a} :: DeleteExpression)
{-# DEPRECATED deExpressionName "Use generic-lens or generic-optics with 'expressionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDomainName :: Lens.Lens' DeleteExpression Lude.Text
deDomainName = Lens.lens (domainName :: DeleteExpression -> Lude.Text) (\s a -> s {domainName = a} :: DeleteExpression)
{-# DEPRECATED deDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteExpression where
  type Rs DeleteExpression = DeleteExpressionResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteExpressionResult"
      ( \s h x ->
          DeleteExpressionResponse'
            Lude.<$> (x Lude..@ "Expression") Lude.<*> (Lude.pure (Lude.fromEnum s))
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
        "ExpressionName" Lude.=: expressionName,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @'DeleteExpression' @ request. Specifies the expression being deleted.
--
-- /See:/ 'mkDeleteExpressionResponse' smart constructor.
data DeleteExpressionResponse = DeleteExpressionResponse'
  { -- | The status of the expression being deleted.
    expression :: ExpressionStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteExpressionResponse' with the minimum fields required to make a request.
--
-- * 'expression' - The status of the expression being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteExpressionResponse ::
  -- | 'expression'
  ExpressionStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteExpressionResponse
mkDeleteExpressionResponse pExpression_ pResponseStatus_ =
  DeleteExpressionResponse'
    { expression = pExpression_,
      responseStatus = pResponseStatus_
    }

-- | The status of the expression being deleted.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsExpression :: Lens.Lens' DeleteExpressionResponse ExpressionStatus
defrsExpression = Lens.lens (expression :: DeleteExpressionResponse -> ExpressionStatus) (\s a -> s {expression = a} :: DeleteExpressionResponse)
{-# DEPRECATED defrsExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsResponseStatus :: Lens.Lens' DeleteExpressionResponse Lude.Int
defrsResponseStatus = Lens.lens (responseStatus :: DeleteExpressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteExpressionResponse)
{-# DEPRECATED defrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
