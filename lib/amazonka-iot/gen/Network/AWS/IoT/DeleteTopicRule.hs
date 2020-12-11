{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the rule.
module Network.AWS.IoT.DeleteTopicRule
  ( -- * Creating a request
    DeleteTopicRule (..),
    mkDeleteTopicRule,

    -- ** Request lenses
    dRuleName,

    -- * Destructuring the response
    DeleteTopicRuleResponse (..),
    mkDeleteTopicRuleResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteTopicRule operation.
--
-- /See:/ 'mkDeleteTopicRule' smart constructor.
newtype DeleteTopicRule = DeleteTopicRule' {ruleName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the rule.
mkDeleteTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  DeleteTopicRule
mkDeleteTopicRule pRuleName_ =
  DeleteTopicRule' {ruleName = pRuleName_}

-- | The name of the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleName :: Lens.Lens' DeleteTopicRule Lude.Text
dRuleName = Lens.lens (ruleName :: DeleteTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: DeleteTopicRule)
{-# DEPRECATED dRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest DeleteTopicRule where
  type Rs DeleteTopicRule = DeleteTopicRuleResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeleteTopicRuleResponse'

instance Lude.ToHeaders DeleteTopicRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTopicRule where
  toPath DeleteTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName]

instance Lude.ToQuery DeleteTopicRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTopicRuleResponse' smart constructor.
data DeleteTopicRuleResponse = DeleteTopicRuleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopicRuleResponse' with the minimum fields required to make a request.
mkDeleteTopicRuleResponse ::
  DeleteTopicRuleResponse
mkDeleteTopicRuleResponse = DeleteTopicRuleResponse'
