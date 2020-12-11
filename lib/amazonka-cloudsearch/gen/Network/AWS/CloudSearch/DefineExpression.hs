{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @'Expression' @ for the search domain. Used to create new expressions and modify existing ones. If the expression exists, the new configuration replaces the old one. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineExpression
  ( -- * Creating a request
    DefineExpression (..),
    mkDefineExpression,

    -- ** Request lenses
    dDomainName,
    dExpression,

    -- * Destructuring the response
    DefineExpressionResponse (..),
    mkDefineExpressionResponse,

    -- ** Response lenses
    dersResponseStatus,
    dersExpression,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DefineExpression' @ operation. Specifies the name of the domain you want to update and the expression you want to configure.
--
-- /See:/ 'mkDefineExpression' smart constructor.
data DefineExpression = DefineExpression'
  { domainName :: Lude.Text,
    expression :: Expression
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineExpression' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
-- * 'expression' - Undocumented field.
mkDefineExpression ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'expression'
  Expression ->
  DefineExpression
mkDefineExpression pDomainName_ pExpression_ =
  DefineExpression'
    { domainName = pDomainName_,
      expression = pExpression_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DefineExpression Lude.Text
dDomainName = Lens.lens (domainName :: DefineExpression -> Lude.Text) (\s a -> s {domainName = a} :: DefineExpression)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpression :: Lens.Lens' DefineExpression Expression
dExpression = Lens.lens (expression :: DefineExpression -> Expression) (\s a -> s {expression = a} :: DefineExpression)
{-# DEPRECATED dExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Lude.AWSRequest DefineExpression where
  type Rs DefineExpression = DefineExpressionResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DefineExpressionResult"
      ( \s h x ->
          DefineExpressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Expression")
      )

instance Lude.ToHeaders DefineExpression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DefineExpression where
  toPath = Lude.const "/"

instance Lude.ToQuery DefineExpression where
  toQuery DefineExpression' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DefineExpression" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "Expression" Lude.=: expression
      ]

-- | The result of a @DefineExpression@ request. Contains the status of the newly-configured expression.
--
-- /See:/ 'mkDefineExpressionResponse' smart constructor.
data DefineExpressionResponse = DefineExpressionResponse'
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

-- | Creates a value of 'DefineExpressionResponse' with the minimum fields required to make a request.
--
-- * 'expression' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDefineExpressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'expression'
  ExpressionStatus ->
  DefineExpressionResponse
mkDefineExpressionResponse pResponseStatus_ pExpression_ =
  DefineExpressionResponse'
    { responseStatus = pResponseStatus_,
      expression = pExpression_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DefineExpressionResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DefineExpressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DefineExpressionResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersExpression :: Lens.Lens' DefineExpressionResponse ExpressionStatus
dersExpression = Lens.lens (expression :: DefineExpressionResponse -> ExpressionStatus) (\s a -> s {expression = a} :: DefineExpressionResponse)
{-# DEPRECATED dersExpression "Use generic-lens or generic-optics with 'expression' instead." #-}
