{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.Types.CancelTimerDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelTimerDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of the @CancelTimer@ decision.
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
-- /See:/ 'newCancelTimerDecisionAttributes' smart constructor.
data CancelTimerDecisionAttributes = CancelTimerDecisionAttributes'
  { -- | The unique ID of the timer to cancel.
    timerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelTimerDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timerId', 'cancelTimerDecisionAttributes_timerId' - The unique ID of the timer to cancel.
newCancelTimerDecisionAttributes ::
  -- | 'timerId'
  Prelude.Text ->
  CancelTimerDecisionAttributes
newCancelTimerDecisionAttributes pTimerId_ =
  CancelTimerDecisionAttributes' {timerId = pTimerId_}

-- | The unique ID of the timer to cancel.
cancelTimerDecisionAttributes_timerId :: Lens.Lens' CancelTimerDecisionAttributes Prelude.Text
cancelTimerDecisionAttributes_timerId = Lens.lens (\CancelTimerDecisionAttributes' {timerId} -> timerId) (\s@CancelTimerDecisionAttributes' {} a -> s {timerId = a} :: CancelTimerDecisionAttributes)

instance
  Prelude.Hashable
    CancelTimerDecisionAttributes

instance Prelude.NFData CancelTimerDecisionAttributes

instance Prelude.ToJSON CancelTimerDecisionAttributes where
  toJSON CancelTimerDecisionAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("timerId" Prelude..= timerId)]
      )
