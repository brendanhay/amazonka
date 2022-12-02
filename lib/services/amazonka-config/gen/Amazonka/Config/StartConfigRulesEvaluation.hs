{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.StartConfigRulesEvaluation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand evaluation for the specified Config rules against the
-- last known configuration state of the resources. Use
-- @StartConfigRulesEvaluation@ when you want to test that a rule you
-- updated is working as expected. @StartConfigRulesEvaluation@ does not
-- re-record the latest configuration state for your resources. It re-runs
-- an evaluation against the last known state of your resources.
--
-- You can specify up to 25 Config rules per request.
--
-- An existing @StartConfigRulesEvaluation@ call for the specified rules
-- must complete before you can call the API again. If you chose to have
-- Config stream to an Amazon SNS topic, you will receive a
-- @ConfigRuleEvaluationStarted@ notification when the evaluation starts.
--
-- You don\'t need to call the @StartConfigRulesEvaluation@ API to run an
-- evaluation for a new rule. When you create a rule, Config evaluates your
-- resources against the rule automatically.
--
-- The @StartConfigRulesEvaluation@ API is useful if you want to run
-- on-demand evaluations, such as the following example:
--
-- 1.  You have a custom rule that evaluates your IAM resources every 24
--     hours.
--
-- 2.  You update your Lambda function to add additional conditions to your
--     rule.
--
-- 3.  Instead of waiting for the next periodic evaluation, you call the
--     @StartConfigRulesEvaluation@ API.
--
-- 4.  Config invokes your Lambda function and evaluates your IAM
--     resources.
--
-- 5.  Your custom rule will still run periodic evaluations every 24 hours.
module Amazonka.Config.StartConfigRulesEvaluation
  ( -- * Creating a Request
    StartConfigRulesEvaluation (..),
    newStartConfigRulesEvaluation,

    -- * Request Lenses
    startConfigRulesEvaluation_configRuleNames,

    -- * Destructuring the Response
    StartConfigRulesEvaluationResponse (..),
    newStartConfigRulesEvaluationResponse,

    -- * Response Lenses
    startConfigRulesEvaluationResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newStartConfigRulesEvaluation' smart constructor.
data StartConfigRulesEvaluation = StartConfigRulesEvaluation'
  { -- | The list of names of Config rules that you want to run evaluations for.
    configRuleNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConfigRulesEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleNames', 'startConfigRulesEvaluation_configRuleNames' - The list of names of Config rules that you want to run evaluations for.
newStartConfigRulesEvaluation ::
  StartConfigRulesEvaluation
newStartConfigRulesEvaluation =
  StartConfigRulesEvaluation'
    { configRuleNames =
        Prelude.Nothing
    }

-- | The list of names of Config rules that you want to run evaluations for.
startConfigRulesEvaluation_configRuleNames :: Lens.Lens' StartConfigRulesEvaluation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
startConfigRulesEvaluation_configRuleNames = Lens.lens (\StartConfigRulesEvaluation' {configRuleNames} -> configRuleNames) (\s@StartConfigRulesEvaluation' {} a -> s {configRuleNames = a} :: StartConfigRulesEvaluation) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest StartConfigRulesEvaluation where
  type
    AWSResponse StartConfigRulesEvaluation =
      StartConfigRulesEvaluationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartConfigRulesEvaluationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartConfigRulesEvaluation where
  hashWithSalt _salt StartConfigRulesEvaluation' {..} =
    _salt `Prelude.hashWithSalt` configRuleNames

instance Prelude.NFData StartConfigRulesEvaluation where
  rnf StartConfigRulesEvaluation' {..} =
    Prelude.rnf configRuleNames

instance Data.ToHeaders StartConfigRulesEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.StartConfigRulesEvaluation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartConfigRulesEvaluation where
  toJSON StartConfigRulesEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigRuleNames" Data..=)
              Prelude.<$> configRuleNames
          ]
      )

instance Data.ToPath StartConfigRulesEvaluation where
  toPath = Prelude.const "/"

instance Data.ToQuery StartConfigRulesEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | The output when you start the evaluation for the specified Config rule.
--
-- /See:/ 'newStartConfigRulesEvaluationResponse' smart constructor.
data StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConfigRulesEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startConfigRulesEvaluationResponse_httpStatus' - The response's http status code.
newStartConfigRulesEvaluationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartConfigRulesEvaluationResponse
newStartConfigRulesEvaluationResponse pHttpStatus_ =
  StartConfigRulesEvaluationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startConfigRulesEvaluationResponse_httpStatus :: Lens.Lens' StartConfigRulesEvaluationResponse Prelude.Int
startConfigRulesEvaluationResponse_httpStatus = Lens.lens (\StartConfigRulesEvaluationResponse' {httpStatus} -> httpStatus) (\s@StartConfigRulesEvaluationResponse' {} a -> s {httpStatus = a} :: StartConfigRulesEvaluationResponse)

instance
  Prelude.NFData
    StartConfigRulesEvaluationResponse
  where
  rnf StartConfigRulesEvaluationResponse' {..} =
    Prelude.rnf httpStatus
