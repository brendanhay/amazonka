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
-- Module      : Amazonka.Route53Domains.Types.OperationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.OperationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.OperationStatus
import Amazonka.Route53Domains.Types.OperationType
import Amazonka.Route53Domains.Types.StatusFlag

-- | OperationSummary includes the following elements.
--
-- /See:/ 'newOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { -- | Name of the domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The date when the last change was made in Unix time format and
    -- Coordinated Universal Time (UTC).
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | Message about the operation.
    message :: Prelude.Maybe Prelude.Text,
    -- | Identifier returned to track the requested action.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the requested operation in the system.
    status :: Prelude.Maybe OperationStatus,
    -- | Automatically checks whether there are no outstanding operations on
    -- domains that need customer attention.
    --
    -- Valid values are:
    --
    -- -   @PENDING_ACCEPTANCE@: The operation is waiting for acceptance from
    --     the account that is receiving the domain.
    --
    -- -   @PENDING_CUSTOMER_ACTION@: The operation is waiting for customer
    --     action, for example, returning an email.
    --
    -- -   @PENDING_AUTHORIZATION@: The operation is waiting for the form of
    --     authorization. For more information, see
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ResendOperationAuthorization.html ResendOperationAuthorization>.
    --
    -- -   @PENDING_PAYMENT_VERIFICATION@: The operation is waiting for the
    --     payment method to validate.
    --
    -- -   @PENDING_SUPPORT_CASE@: The operation includes a support case and is
    --     waiting for its resolution.
    statusFlag :: Prelude.Maybe StatusFlag,
    -- | The date when the request was submitted.
    submittedDate :: Prelude.Maybe Data.POSIX,
    -- | Type of the action requested.
    type' :: Prelude.Maybe OperationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'operationSummary_domainName' - Name of the domain.
--
-- 'lastUpdatedDate', 'operationSummary_lastUpdatedDate' - The date when the last change was made in Unix time format and
-- Coordinated Universal Time (UTC).
--
-- 'message', 'operationSummary_message' - Message about the operation.
--
-- 'operationId', 'operationSummary_operationId' - Identifier returned to track the requested action.
--
-- 'status', 'operationSummary_status' - The current status of the requested operation in the system.
--
-- 'statusFlag', 'operationSummary_statusFlag' - Automatically checks whether there are no outstanding operations on
-- domains that need customer attention.
--
-- Valid values are:
--
-- -   @PENDING_ACCEPTANCE@: The operation is waiting for acceptance from
--     the account that is receiving the domain.
--
-- -   @PENDING_CUSTOMER_ACTION@: The operation is waiting for customer
--     action, for example, returning an email.
--
-- -   @PENDING_AUTHORIZATION@: The operation is waiting for the form of
--     authorization. For more information, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ResendOperationAuthorization.html ResendOperationAuthorization>.
--
-- -   @PENDING_PAYMENT_VERIFICATION@: The operation is waiting for the
--     payment method to validate.
--
-- -   @PENDING_SUPPORT_CASE@: The operation includes a support case and is
--     waiting for its resolution.
--
-- 'submittedDate', 'operationSummary_submittedDate' - The date when the request was submitted.
--
-- 'type'', 'operationSummary_type' - Type of the action requested.
newOperationSummary ::
  OperationSummary
newOperationSummary =
  OperationSummary'
    { domainName = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      message = Prelude.Nothing,
      operationId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusFlag = Prelude.Nothing,
      submittedDate = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Name of the domain.
operationSummary_domainName :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.Text)
operationSummary_domainName = Lens.lens (\OperationSummary' {domainName} -> domainName) (\s@OperationSummary' {} a -> s {domainName = a} :: OperationSummary)

-- | The date when the last change was made in Unix time format and
-- Coordinated Universal Time (UTC).
operationSummary_lastUpdatedDate :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.UTCTime)
operationSummary_lastUpdatedDate = Lens.lens (\OperationSummary' {lastUpdatedDate} -> lastUpdatedDate) (\s@OperationSummary' {} a -> s {lastUpdatedDate = a} :: OperationSummary) Prelude.. Lens.mapping Data._Time

-- | Message about the operation.
operationSummary_message :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.Text)
operationSummary_message = Lens.lens (\OperationSummary' {message} -> message) (\s@OperationSummary' {} a -> s {message = a} :: OperationSummary)

-- | Identifier returned to track the requested action.
operationSummary_operationId :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.Text)
operationSummary_operationId = Lens.lens (\OperationSummary' {operationId} -> operationId) (\s@OperationSummary' {} a -> s {operationId = a} :: OperationSummary)

-- | The current status of the requested operation in the system.
operationSummary_status :: Lens.Lens' OperationSummary (Prelude.Maybe OperationStatus)
operationSummary_status = Lens.lens (\OperationSummary' {status} -> status) (\s@OperationSummary' {} a -> s {status = a} :: OperationSummary)

-- | Automatically checks whether there are no outstanding operations on
-- domains that need customer attention.
--
-- Valid values are:
--
-- -   @PENDING_ACCEPTANCE@: The operation is waiting for acceptance from
--     the account that is receiving the domain.
--
-- -   @PENDING_CUSTOMER_ACTION@: The operation is waiting for customer
--     action, for example, returning an email.
--
-- -   @PENDING_AUTHORIZATION@: The operation is waiting for the form of
--     authorization. For more information, see
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ResendOperationAuthorization.html ResendOperationAuthorization>.
--
-- -   @PENDING_PAYMENT_VERIFICATION@: The operation is waiting for the
--     payment method to validate.
--
-- -   @PENDING_SUPPORT_CASE@: The operation includes a support case and is
--     waiting for its resolution.
operationSummary_statusFlag :: Lens.Lens' OperationSummary (Prelude.Maybe StatusFlag)
operationSummary_statusFlag = Lens.lens (\OperationSummary' {statusFlag} -> statusFlag) (\s@OperationSummary' {} a -> s {statusFlag = a} :: OperationSummary)

-- | The date when the request was submitted.
operationSummary_submittedDate :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.UTCTime)
operationSummary_submittedDate = Lens.lens (\OperationSummary' {submittedDate} -> submittedDate) (\s@OperationSummary' {} a -> s {submittedDate = a} :: OperationSummary) Prelude.. Lens.mapping Data._Time

-- | Type of the action requested.
operationSummary_type :: Lens.Lens' OperationSummary (Prelude.Maybe OperationType)
operationSummary_type = Lens.lens (\OperationSummary' {type'} -> type') (\s@OperationSummary' {} a -> s {type' = a} :: OperationSummary)

instance Data.FromJSON OperationSummary where
  parseJSON =
    Data.withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "LastUpdatedDate")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OperationId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusFlag")
            Prelude.<*> (x Data..:? "SubmittedDate")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable OperationSummary where
  hashWithSalt _salt OperationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusFlag
      `Prelude.hashWithSalt` submittedDate
      `Prelude.hashWithSalt` type'

instance Prelude.NFData OperationSummary where
  rnf OperationSummary' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusFlag
      `Prelude.seq` Prelude.rnf submittedDate
      `Prelude.seq` Prelude.rnf type'
