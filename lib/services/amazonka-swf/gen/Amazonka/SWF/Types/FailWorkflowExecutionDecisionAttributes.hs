{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Types.FailWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.FailWorkflowExecutionDecisionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @FailWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newFailWorkflowExecutionDecisionAttributes' smart constructor.
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes'
  { -- | Details of the failure.
    details :: Prelude.Maybe Prelude.Text,
    -- | A descriptive reason for the failure that may help in diagnostics.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailWorkflowExecutionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'failWorkflowExecutionDecisionAttributes_details' - Details of the failure.
--
-- 'reason', 'failWorkflowExecutionDecisionAttributes_reason' - A descriptive reason for the failure that may help in diagnostics.
newFailWorkflowExecutionDecisionAttributes ::
  FailWorkflowExecutionDecisionAttributes
newFailWorkflowExecutionDecisionAttributes =
  FailWorkflowExecutionDecisionAttributes'
    { details =
        Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | Details of the failure.
failWorkflowExecutionDecisionAttributes_details :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
failWorkflowExecutionDecisionAttributes_details = Lens.lens (\FailWorkflowExecutionDecisionAttributes' {details} -> details) (\s@FailWorkflowExecutionDecisionAttributes' {} a -> s {details = a} :: FailWorkflowExecutionDecisionAttributes)

-- | A descriptive reason for the failure that may help in diagnostics.
failWorkflowExecutionDecisionAttributes_reason :: Lens.Lens' FailWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
failWorkflowExecutionDecisionAttributes_reason = Lens.lens (\FailWorkflowExecutionDecisionAttributes' {reason} -> reason) (\s@FailWorkflowExecutionDecisionAttributes' {} a -> s {reason = a} :: FailWorkflowExecutionDecisionAttributes)

instance
  Prelude.Hashable
    FailWorkflowExecutionDecisionAttributes
  where
  hashWithSalt
    _salt
    FailWorkflowExecutionDecisionAttributes' {..} =
      _salt `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` reason

instance
  Prelude.NFData
    FailWorkflowExecutionDecisionAttributes
  where
  rnf FailWorkflowExecutionDecisionAttributes' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf reason

instance
  Data.ToJSON
    FailWorkflowExecutionDecisionAttributes
  where
  toJSON FailWorkflowExecutionDecisionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("details" Data..=) Prelude.<$> details,
            ("reason" Data..=) Prelude.<$> reason
          ]
      )
