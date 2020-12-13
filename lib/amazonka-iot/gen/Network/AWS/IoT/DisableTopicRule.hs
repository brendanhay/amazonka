{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DisableTopicRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the rule.
module Network.AWS.IoT.DisableTopicRule
  ( -- * Creating a request
    DisableTopicRule (..),
    mkDisableTopicRule,

    -- ** Request lenses
    dRuleName,

    -- * Destructuring the response
    DisableTopicRuleResponse (..),
    mkDisableTopicRuleResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DisableTopicRuleRequest operation.
--
-- /See:/ 'mkDisableTopicRule' smart constructor.
newtype DisableTopicRule = DisableTopicRule'
  { -- | The name of the rule to disable.
    ruleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableTopicRule' with the minimum fields required to make a request.
--
-- * 'ruleName' - The name of the rule to disable.
mkDisableTopicRule ::
  -- | 'ruleName'
  Lude.Text ->
  DisableTopicRule
mkDisableTopicRule pRuleName_ =
  DisableTopicRule' {ruleName = pRuleName_}

-- | The name of the rule to disable.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleName :: Lens.Lens' DisableTopicRule Lude.Text
dRuleName = Lens.lens (ruleName :: DisableTopicRule -> Lude.Text) (\s a -> s {ruleName = a} :: DisableTopicRule)
{-# DEPRECATED dRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Lude.AWSRequest DisableTopicRule where
  type Rs DisableTopicRule = DisableTopicRuleResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull DisableTopicRuleResponse'

instance Lude.ToHeaders DisableTopicRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DisableTopicRule where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisableTopicRule where
  toPath DisableTopicRule' {..} =
    Lude.mconcat ["/rules/", Lude.toBS ruleName, "/disable"]

instance Lude.ToQuery DisableTopicRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableTopicRuleResponse' smart constructor.
data DisableTopicRuleResponse = DisableTopicRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableTopicRuleResponse' with the minimum fields required to make a request.
mkDisableTopicRuleResponse ::
  DisableTopicRuleResponse
mkDisableTopicRuleResponse = DisableTopicRuleResponse'
