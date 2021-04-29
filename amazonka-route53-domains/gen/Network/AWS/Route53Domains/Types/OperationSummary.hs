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
-- Module      : Network.AWS.Route53Domains.Types.OperationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53Domains.Types.OperationStatus
import Network.AWS.Route53Domains.Types.OperationType

-- | OperationSummary includes the following elements.
--
-- /See:/ 'newOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { -- | Identifier returned to track the requested action.
    operationId :: Prelude.Text,
    -- | The current status of the requested operation in the system.
    status :: OperationStatus,
    -- | Type of the action requested.
    type' :: OperationType,
    -- | The date when the request was submitted.
    submittedDate :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'operationSummary_operationId' - Identifier returned to track the requested action.
--
-- 'status', 'operationSummary_status' - The current status of the requested operation in the system.
--
-- 'type'', 'operationSummary_type' - Type of the action requested.
--
-- 'submittedDate', 'operationSummary_submittedDate' - The date when the request was submitted.
newOperationSummary ::
  -- | 'operationId'
  Prelude.Text ->
  -- | 'status'
  OperationStatus ->
  -- | 'type''
  OperationType ->
  -- | 'submittedDate'
  Prelude.UTCTime ->
  OperationSummary
newOperationSummary
  pOperationId_
  pStatus_
  pType_
  pSubmittedDate_ =
    OperationSummary'
      { operationId = pOperationId_,
        status = pStatus_,
        type' = pType_,
        submittedDate = Prelude._Time Lens.# pSubmittedDate_
      }

-- | Identifier returned to track the requested action.
operationSummary_operationId :: Lens.Lens' OperationSummary Prelude.Text
operationSummary_operationId = Lens.lens (\OperationSummary' {operationId} -> operationId) (\s@OperationSummary' {} a -> s {operationId = a} :: OperationSummary)

-- | The current status of the requested operation in the system.
operationSummary_status :: Lens.Lens' OperationSummary OperationStatus
operationSummary_status = Lens.lens (\OperationSummary' {status} -> status) (\s@OperationSummary' {} a -> s {status = a} :: OperationSummary)

-- | Type of the action requested.
operationSummary_type :: Lens.Lens' OperationSummary OperationType
operationSummary_type = Lens.lens (\OperationSummary' {type'} -> type') (\s@OperationSummary' {} a -> s {type' = a} :: OperationSummary)

-- | The date when the request was submitted.
operationSummary_submittedDate :: Lens.Lens' OperationSummary Prelude.UTCTime
operationSummary_submittedDate = Lens.lens (\OperationSummary' {submittedDate} -> submittedDate) (\s@OperationSummary' {} a -> s {submittedDate = a} :: OperationSummary) Prelude.. Prelude._Time

instance Prelude.FromJSON OperationSummary where
  parseJSON =
    Prelude.withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            Prelude.<$> (x Prelude..: "OperationId")
            Prelude.<*> (x Prelude..: "Status")
            Prelude.<*> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "SubmittedDate")
      )

instance Prelude.Hashable OperationSummary

instance Prelude.NFData OperationSummary
