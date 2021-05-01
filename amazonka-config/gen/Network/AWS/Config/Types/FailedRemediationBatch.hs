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
-- Module      : Network.AWS.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationBatch where

import Network.AWS.Config.Types.RemediationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'newFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { -- | Returns a failure message. For example, the resource is already
    -- compliant.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Returns remediation configurations of the failed items.
    failedItems :: Prelude.Maybe [RemediationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedRemediationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'failedRemediationBatch_failureMessage' - Returns a failure message. For example, the resource is already
-- compliant.
--
-- 'failedItems', 'failedRemediationBatch_failedItems' - Returns remediation configurations of the failed items.
newFailedRemediationBatch ::
  FailedRemediationBatch
newFailedRemediationBatch =
  FailedRemediationBatch'
    { failureMessage =
        Prelude.Nothing,
      failedItems = Prelude.Nothing
    }

-- | Returns a failure message. For example, the resource is already
-- compliant.
failedRemediationBatch_failureMessage :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe Prelude.Text)
failedRemediationBatch_failureMessage = Lens.lens (\FailedRemediationBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationBatch' {} a -> s {failureMessage = a} :: FailedRemediationBatch)

-- | Returns remediation configurations of the failed items.
failedRemediationBatch_failedItems :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe [RemediationConfiguration])
failedRemediationBatch_failedItems = Lens.lens (\FailedRemediationBatch' {failedItems} -> failedItems) (\s@FailedRemediationBatch' {} a -> s {failedItems = a} :: FailedRemediationBatch) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON FailedRemediationBatch where
  parseJSON =
    Prelude.withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            Prelude.<$> (x Prelude..:? "FailureMessage")
            Prelude.<*> ( x Prelude..:? "FailedItems"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FailedRemediationBatch

instance Prelude.NFData FailedRemediationBatch
