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
-- Module      : Amazonka.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.FailedRemediationExceptionBatch where

import Amazonka.Config.Types.RemediationException
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of each of the failed remediation exceptions with specific reasons.
--
-- /See:/ 'newFailedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Prelude.Maybe [RemediationException],
    -- | Returns a failure message. For example, the auto-remediation has failed.
    failureMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedRemediationExceptionBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedItems', 'failedRemediationExceptionBatch_failedItems' - Returns remediation exception resource key object of the failed items.
--
-- 'failureMessage', 'failedRemediationExceptionBatch_failureMessage' - Returns a failure message. For example, the auto-remediation has failed.
newFailedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
newFailedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { failedItems =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing
    }

-- | Returns remediation exception resource key object of the failed items.
failedRemediationExceptionBatch_failedItems :: Lens.Lens' FailedRemediationExceptionBatch (Prelude.Maybe [RemediationException])
failedRemediationExceptionBatch_failedItems = Lens.lens (\FailedRemediationExceptionBatch' {failedItems} -> failedItems) (\s@FailedRemediationExceptionBatch' {} a -> s {failedItems = a} :: FailedRemediationExceptionBatch) Prelude.. Lens.mapping Lens.coerced

-- | Returns a failure message. For example, the auto-remediation has failed.
failedRemediationExceptionBatch_failureMessage :: Lens.Lens' FailedRemediationExceptionBatch (Prelude.Maybe Prelude.Text)
failedRemediationExceptionBatch_failureMessage = Lens.lens (\FailedRemediationExceptionBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationExceptionBatch' {} a -> s {failureMessage = a} :: FailedRemediationExceptionBatch)

instance
  Data.FromJSON
    FailedRemediationExceptionBatch
  where
  parseJSON =
    Data.withObject
      "FailedRemediationExceptionBatch"
      ( \x ->
          FailedRemediationExceptionBatch'
            Prelude.<$> (x Data..:? "FailedItems" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FailureMessage")
      )

instance
  Prelude.Hashable
    FailedRemediationExceptionBatch
  where
  hashWithSalt
    _salt
    FailedRemediationExceptionBatch' {..} =
      _salt `Prelude.hashWithSalt` failedItems
        `Prelude.hashWithSalt` failureMessage

instance
  Prelude.NFData
    FailedRemediationExceptionBatch
  where
  rnf FailedRemediationExceptionBatch' {..} =
    Prelude.rnf failedItems
      `Prelude.seq` Prelude.rnf failureMessage
