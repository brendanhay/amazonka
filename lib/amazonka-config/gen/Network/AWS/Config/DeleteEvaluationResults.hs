{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteEvaluationResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the evaluation results for the specified AWS Config rule. You can specify one AWS Config rule per request. After you delete the evaluation results, you can call the 'StartConfigRulesEvaluation' API to start evaluating your AWS resources against the rule.
module Network.AWS.Config.DeleteEvaluationResults
  ( -- * Creating a request
    DeleteEvaluationResults (..),
    mkDeleteEvaluationResults,

    -- ** Request lenses
    derConfigRuleName,

    -- * Destructuring the response
    DeleteEvaluationResultsResponse (..),
    mkDeleteEvaluationResultsResponse,

    -- ** Response lenses
    derrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteEvaluationResults' smart constructor.
newtype DeleteEvaluationResults = DeleteEvaluationResults'
  { configRuleName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEvaluationResults' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule for which you want to delete the evaluation results.
mkDeleteEvaluationResults ::
  -- | 'configRuleName'
  Lude.Text ->
  DeleteEvaluationResults
mkDeleteEvaluationResults pConfigRuleName_ =
  DeleteEvaluationResults' {configRuleName = pConfigRuleName_}

-- | The name of the AWS Config rule for which you want to delete the evaluation results.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derConfigRuleName :: Lens.Lens' DeleteEvaluationResults Lude.Text
derConfigRuleName = Lens.lens (configRuleName :: DeleteEvaluationResults -> Lude.Text) (\s a -> s {configRuleName = a} :: DeleteEvaluationResults)
{-# DEPRECATED derConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.AWSRequest DeleteEvaluationResults where
  type Rs DeleteEvaluationResults = DeleteEvaluationResultsResponse
  request = Req.postJSON configService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteEvaluationResultsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEvaluationResults where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeleteEvaluationResults" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEvaluationResults where
  toJSON DeleteEvaluationResults' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ConfigRuleName" Lude..= configRuleName)]
      )

instance Lude.ToPath DeleteEvaluationResults where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEvaluationResults where
  toQuery = Lude.const Lude.mempty

-- | The output when you delete the evaluation results for the specified AWS Config rule.
--
-- /See:/ 'mkDeleteEvaluationResultsResponse' smart constructor.
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEvaluationResultsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteEvaluationResultsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEvaluationResultsResponse
mkDeleteEvaluationResultsResponse pResponseStatus_ =
  DeleteEvaluationResultsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEvaluationResultsResponse Lude.Int
derrsResponseStatus = Lens.lens (responseStatus :: DeleteEvaluationResultsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEvaluationResultsResponse)
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
