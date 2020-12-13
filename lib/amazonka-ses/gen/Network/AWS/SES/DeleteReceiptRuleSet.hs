{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule set and all of the receipt rules it contains.
--
-- For information about managing receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptRuleSet
  ( -- * Creating a request
    DeleteReceiptRuleSet (..),
    mkDeleteReceiptRuleSet,

    -- ** Request lenses
    dRuleSetName,

    -- * Destructuring the response
    DeleteReceiptRuleSetResponse (..),
    mkDeleteReceiptRuleSetResponse,

    -- ** Response lenses
    drrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete a receipt rule set and all of the receipt rules it contains. You use receipt rule sets to receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptRuleSet' smart constructor.
newtype DeleteReceiptRuleSet = DeleteReceiptRuleSet'
  { -- | The name of the receipt rule set to delete.
    ruleSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReceiptRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - The name of the receipt rule set to delete.
mkDeleteReceiptRuleSet ::
  -- | 'ruleSetName'
  Lude.Text ->
  DeleteReceiptRuleSet
mkDeleteReceiptRuleSet pRuleSetName_ =
  DeleteReceiptRuleSet' {ruleSetName = pRuleSetName_}

-- | The name of the receipt rule set to delete.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleSetName :: Lens.Lens' DeleteReceiptRuleSet Lude.Text
dRuleSetName = Lens.lens (ruleSetName :: DeleteReceiptRuleSet -> Lude.Text) (\s a -> s {ruleSetName = a} :: DeleteReceiptRuleSet)
{-# DEPRECATED dRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

instance Lude.AWSRequest DeleteReceiptRuleSet where
  type Rs DeleteReceiptRuleSet = DeleteReceiptRuleSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteReceiptRuleSetResult"
      ( \s h x ->
          DeleteReceiptRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReceiptRuleSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteReceiptRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReceiptRuleSet where
  toQuery DeleteReceiptRuleSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteReceiptRuleSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "RuleSetName" Lude.=: ruleSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptRuleSetResponse' smart constructor.
newtype DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReceiptRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReceiptRuleSetResponse
mkDeleteReceiptRuleSetResponse pResponseStatus_ =
  DeleteReceiptRuleSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsrsResponseStatus :: Lens.Lens' DeleteReceiptRuleSetResponse Lude.Int
drrsrsResponseStatus = Lens.lens (responseStatus :: DeleteReceiptRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReceiptRuleSetResponse)
{-# DEPRECATED drrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
