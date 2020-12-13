{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS Config rule and all of its evaluation results.
--
-- AWS Config sets the state of a rule to @DELETING@ until the deletion is complete. You cannot update a rule while it is in this state. If you make a @PutConfigRule@ or @DeleteConfigRule@ request for the rule, you will receive a @ResourceInUseException@ .
-- You can check the state of a rule by using the @DescribeConfigRules@ request.
module Network.AWS.Config.DeleteConfigRule
  ( -- * Creating a request
    DeleteConfigRule (..),
    mkDeleteConfigRule,

    -- ** Request lenses
    dcrConfigRuleName,

    -- * Destructuring the response
    DeleteConfigRuleResponse (..),
    mkDeleteConfigRuleResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteConfigRule' smart constructor.
newtype DeleteConfigRule = DeleteConfigRule'
  { -- | The name of the AWS Config rule that you want to delete.
    configRuleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigRule' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule that you want to delete.
mkDeleteConfigRule ::
  -- | 'configRuleName'
  Lude.Text ->
  DeleteConfigRule
mkDeleteConfigRule pConfigRuleName_ =
  DeleteConfigRule' {configRuleName = pConfigRuleName_}

-- | The name of the AWS Config rule that you want to delete.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigRuleName :: Lens.Lens' DeleteConfigRule Lude.Text
dcrConfigRuleName = Lens.lens (configRuleName :: DeleteConfigRule -> Lude.Text) (\s a -> s {configRuleName = a} :: DeleteConfigRule)
{-# DEPRECATED dcrConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.AWSRequest DeleteConfigRule where
  type Rs DeleteConfigRule = DeleteConfigRuleResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteConfigRuleResponse'

instance Lude.ToHeaders DeleteConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeleteConfigRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConfigRule where
  toJSON DeleteConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ConfigRuleName" Lude..= configRuleName)]
      )

instance Lude.ToPath DeleteConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConfigRuleResponse' smart constructor.
data DeleteConfigRuleResponse = DeleteConfigRuleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigRuleResponse' with the minimum fields required to make a request.
mkDeleteConfigRuleResponse ::
  DeleteConfigRuleResponse
mkDeleteConfigRuleResponse = DeleteConfigRuleResponse'
