{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartConfigRulesEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand evaluation for the specified AWS Config rules against the last known configuration state of the resources. Use @StartConfigRulesEvaluation@ when you want to test that a rule you updated is working as expected. @StartConfigRulesEvaluation@ does not re-record the latest configuration state for your resources. It re-runs an evaluation against the last known state of your resources.
--
-- You can specify up to 25 AWS Config rules per request.
-- An existing @StartConfigRulesEvaluation@ call for the specified rules must complete before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a @ConfigRuleEvaluationStarted@ notification when the evaluation starts.
-- The @StartConfigRulesEvaluation@ API is useful if you want to run on-demand evaluations, such as the following example:
--
--     * You have a custom rule that evaluates your IAM resources every 24 hours.
--
--
--     * You update your Lambda function to add additional conditions to your rule.
--
--
--     * Instead of waiting for the next periodic evaluation, you call the @StartConfigRulesEvaluation@ API.
--
--
--     * AWS Config invokes your Lambda function and evaluates your IAM resources.
--
--
--     * Your custom rule will still run periodic evaluations every 24 hours.
module Network.AWS.Config.StartConfigRulesEvaluation
  ( -- * Creating a request
    StartConfigRulesEvaluation (..),
    mkStartConfigRulesEvaluation,

    -- ** Request lenses
    screConfigRuleNames,

    -- * Destructuring the response
    StartConfigRulesEvaluationResponse (..),
    mkStartConfigRulesEvaluationResponse,

    -- ** Response lenses
    scrersResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStartConfigRulesEvaluation' smart constructor.
newtype StartConfigRulesEvaluation = StartConfigRulesEvaluation'
  { configRuleNames ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartConfigRulesEvaluation' with the minimum fields required to make a request.
--
-- * 'configRuleNames' - The list of names of AWS Config rules that you want to run evaluations for.
mkStartConfigRulesEvaluation ::
  StartConfigRulesEvaluation
mkStartConfigRulesEvaluation =
  StartConfigRulesEvaluation' {configRuleNames = Lude.Nothing}

-- | The list of names of AWS Config rules that you want to run evaluations for.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
screConfigRuleNames :: Lens.Lens' StartConfigRulesEvaluation (Lude.Maybe (Lude.NonEmpty Lude.Text))
screConfigRuleNames = Lens.lens (configRuleNames :: StartConfigRulesEvaluation -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {configRuleNames = a} :: StartConfigRulesEvaluation)
{-# DEPRECATED screConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

instance Lude.AWSRequest StartConfigRulesEvaluation where
  type
    Rs StartConfigRulesEvaluation =
      StartConfigRulesEvaluationResponse
  request = Req.postJSON configService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartConfigRulesEvaluationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartConfigRulesEvaluation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.StartConfigRulesEvaluation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartConfigRulesEvaluation where
  toJSON StartConfigRulesEvaluation' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames]
      )

instance Lude.ToPath StartConfigRulesEvaluation where
  toPath = Lude.const "/"

instance Lude.ToQuery StartConfigRulesEvaluation where
  toQuery = Lude.const Lude.mempty

-- | The output when you start the evaluation for the specified AWS Config rule.
--
-- /See:/ 'mkStartConfigRulesEvaluationResponse' smart constructor.
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse'
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

-- | Creates a value of 'StartConfigRulesEvaluationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartConfigRulesEvaluationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartConfigRulesEvaluationResponse
mkStartConfigRulesEvaluationResponse pResponseStatus_ =
  StartConfigRulesEvaluationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrersResponseStatus :: Lens.Lens' StartConfigRulesEvaluationResponse Lude.Int
scrersResponseStatus = Lens.lens (responseStatus :: StartConfigRulesEvaluationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartConfigRulesEvaluationResponse)
{-# DEPRECATED scrersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
