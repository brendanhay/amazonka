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
-- Module      : Network.AWS.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationExceptionBatch where

import Network.AWS.Config.Types.RemediationException
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of each of the failed remediation exceptions with specific reasons.
--
-- /See:/ 'newFailedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { -- | Returns a failure message. For example, the auto-remediation has failed.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Prelude.Maybe [RemediationException]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedRemediationExceptionBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'failedRemediationExceptionBatch_failureMessage' - Returns a failure message. For example, the auto-remediation has failed.
--
-- 'failedItems', 'failedRemediationExceptionBatch_failedItems' - Returns remediation exception resource key object of the failed items.
newFailedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
newFailedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { failureMessage =
        Prelude.Nothing,
      failedItems = Prelude.Nothing
    }

-- | Returns a failure message. For example, the auto-remediation has failed.
failedRemediationExceptionBatch_failureMessage :: Lens.Lens' FailedRemediationExceptionBatch (Prelude.Maybe Prelude.Text)
failedRemediationExceptionBatch_failureMessage = Lens.lens (\FailedRemediationExceptionBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationExceptionBatch' {} a -> s {failureMessage = a} :: FailedRemediationExceptionBatch)

-- | Returns remediation exception resource key object of the failed items.
failedRemediationExceptionBatch_failedItems :: Lens.Lens' FailedRemediationExceptionBatch (Prelude.Maybe [RemediationException])
failedRemediationExceptionBatch_failedItems = Lens.lens (\FailedRemediationExceptionBatch' {failedItems} -> failedItems) (\s@FailedRemediationExceptionBatch' {} a -> s {failedItems = a} :: FailedRemediationExceptionBatch) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    FailedRemediationExceptionBatch
  where
  parseJSON =
    Prelude.withObject
      "FailedRemediationExceptionBatch"
      ( \x ->
          FailedRemediationExceptionBatch'
            Prelude.<$> (x Prelude..:? "FailureMessage")
            Prelude.<*> ( x Prelude..:? "FailedItems"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    FailedRemediationExceptionBatch

instance
  Prelude.NFData
    FailedRemediationExceptionBatch
