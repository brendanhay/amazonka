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
-- Module      : Amazonka.SSM.Types.FailureDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.FailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Automation failure.
--
-- /See:/ 'newFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The type of Automation failure. Failure types include the following:
    -- Action, Permission, Throttling, Verification, Internal.
    failureType :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the Automation step failure.
    details :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The stage of the Automation execution when the failure occurred. The
    -- stages include the following: InputValidation, PreVerification,
    -- Invocation, PostVerification.
    failureStage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureType', 'failureDetails_failureType' - The type of Automation failure. Failure types include the following:
-- Action, Permission, Throttling, Verification, Internal.
--
-- 'details', 'failureDetails_details' - Detailed information about the Automation step failure.
--
-- 'failureStage', 'failureDetails_failureStage' - The stage of the Automation execution when the failure occurred. The
-- stages include the following: InputValidation, PreVerification,
-- Invocation, PostVerification.
newFailureDetails ::
  FailureDetails
newFailureDetails =
  FailureDetails'
    { failureType = Prelude.Nothing,
      details = Prelude.Nothing,
      failureStage = Prelude.Nothing
    }

-- | The type of Automation failure. Failure types include the following:
-- Action, Permission, Throttling, Verification, Internal.
failureDetails_failureType :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_failureType = Lens.lens (\FailureDetails' {failureType} -> failureType) (\s@FailureDetails' {} a -> s {failureType = a} :: FailureDetails)

-- | Detailed information about the Automation step failure.
failureDetails_details :: Lens.Lens' FailureDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
failureDetails_details = Lens.lens (\FailureDetails' {details} -> details) (\s@FailureDetails' {} a -> s {details = a} :: FailureDetails) Prelude.. Lens.mapping Lens.coerced

-- | The stage of the Automation execution when the failure occurred. The
-- stages include the following: InputValidation, PreVerification,
-- Invocation, PostVerification.
failureDetails_failureStage :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_failureStage = Lens.lens (\FailureDetails' {failureStage} -> failureStage) (\s@FailureDetails' {} a -> s {failureStage = a} :: FailureDetails)

instance Core.FromJSON FailureDetails where
  parseJSON =
    Core.withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            Prelude.<$> (x Core..:? "FailureType")
            Prelude.<*> (x Core..:? "Details" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FailureStage")
      )

instance Prelude.Hashable FailureDetails where
  hashWithSalt _salt FailureDetails' {..} =
    _salt `Prelude.hashWithSalt` failureType
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` failureStage

instance Prelude.NFData FailureDetails where
  rnf FailureDetails' {..} =
    Prelude.rnf failureType
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf failureStage
