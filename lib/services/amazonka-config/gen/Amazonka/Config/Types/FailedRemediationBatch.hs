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
-- Module      : Amazonka.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.FailedRemediationBatch where

import Amazonka.Config.Types.RemediationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'newFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { -- | Returns remediation configurations of the failed items.
    failedItems :: Prelude.Maybe [RemediationConfiguration],
    -- | Returns a failure message. For example, the resource is already
    -- compliant.
    failureMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedRemediationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedItems', 'failedRemediationBatch_failedItems' - Returns remediation configurations of the failed items.
--
-- 'failureMessage', 'failedRemediationBatch_failureMessage' - Returns a failure message. For example, the resource is already
-- compliant.
newFailedRemediationBatch ::
  FailedRemediationBatch
newFailedRemediationBatch =
  FailedRemediationBatch'
    { failedItems =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing
    }

-- | Returns remediation configurations of the failed items.
failedRemediationBatch_failedItems :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe [RemediationConfiguration])
failedRemediationBatch_failedItems = Lens.lens (\FailedRemediationBatch' {failedItems} -> failedItems) (\s@FailedRemediationBatch' {} a -> s {failedItems = a} :: FailedRemediationBatch) Prelude.. Lens.mapping Lens.coerced

-- | Returns a failure message. For example, the resource is already
-- compliant.
failedRemediationBatch_failureMessage :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe Prelude.Text)
failedRemediationBatch_failureMessage = Lens.lens (\FailedRemediationBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationBatch' {} a -> s {failureMessage = a} :: FailedRemediationBatch)

instance Data.FromJSON FailedRemediationBatch where
  parseJSON =
    Data.withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            Prelude.<$> (x Data..:? "FailedItems" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FailureMessage")
      )

instance Prelude.Hashable FailedRemediationBatch where
  hashWithSalt _salt FailedRemediationBatch' {..} =
    _salt
      `Prelude.hashWithSalt` failedItems
      `Prelude.hashWithSalt` failureMessage

instance Prelude.NFData FailedRemediationBatch where
  rnf FailedRemediationBatch' {..} =
    Prelude.rnf failedItems
      `Prelude.seq` Prelude.rnf failureMessage
