{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.EnableTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the rule.
module Network.AWS.IoT.EnableTopicRule
  ( -- * Creating a request
    EnableTopicRule (..),
    mkEnableTopicRule,

    -- ** Request lenses
    etrRuleName,

    -- * Destructuring the response
    EnableTopicRuleResponse (..),
    mkEnableTopicRuleResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the EnableTopicRuleRequest operation.
--
-- /See:/ 'mkEnableTopicRule' smart constructor.
newtype EnableTopicRule = EnableTopicRule' {ruleName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the topic rule to enable.
mkEnableTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  EnableTopicRule
mkEnableTopicRule pRuleName_ =
  EnableTopicRule' {ruleName = pRuleName_}

-- | The name of the topic rule to enable.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etrRuleName :: Lens.Lens' EnableTopicRule Lude.Text
etrRuleName = Lens.lens (ruleName :: EnableTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: EnableTopicRule)
{-# DEPRECATED etrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest EnableTopicRule where
  type Rs EnableTopicRule = EnableTopicRuleResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull EnableTopicRuleResponse'

instance Lude.ToHeaders EnableTopicRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON EnableTopicRule where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableTopicRule where
  toPath EnableTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName, "/enable"]

instance Lude.ToQuery EnableTopicRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableTopicRuleResponse' smart constructor.
data EnableTopicRuleResponse = EnableTopicRuleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableTopicRuleResponse' with the minimum fields required to make a request.
mkEnableTopicRuleResponse ::
  EnableTopicRuleResponse
mkEnableTopicRuleResponse = EnableTopicRuleResponse'
