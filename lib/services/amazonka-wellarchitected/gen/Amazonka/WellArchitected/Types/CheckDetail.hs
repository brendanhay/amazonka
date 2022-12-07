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
-- Module      : Amazonka.WellArchitected.Types.CheckDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.CheckDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.CheckFailureReason
import Amazonka.WellArchitected.Types.CheckProvider
import Amazonka.WellArchitected.Types.CheckStatus

-- | Account details for a Well-Architected best practice in relation to
-- Trusted Advisor checks.
--
-- /See:/ 'newCheckDetail' smart constructor.
data CheckDetail = CheckDetail'
  { -- | Trusted Advisor check name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Well-Architected Lens ARN associated to the check.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | Provider of the check related to the best practice.
    provider :: Prelude.Maybe CheckProvider,
    questionId :: Prelude.Maybe Prelude.Text,
    -- | Status associated to the check.
    status :: Prelude.Maybe CheckStatus,
    -- | Trusted Advisor check ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Trusted Advisor check description.
    description :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text,
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Reason associated to the check.
    reason :: Prelude.Maybe CheckFailureReason,
    -- | Count of flagged resources associated to the check.
    flaggedResources :: Prelude.Maybe Prelude.Natural,
    pillarId :: Prelude.Maybe Prelude.Text,
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'checkDetail_name' - Trusted Advisor check name.
--
-- 'lensArn', 'checkDetail_lensArn' - Well-Architected Lens ARN associated to the check.
--
-- 'provider', 'checkDetail_provider' - Provider of the check related to the best practice.
--
-- 'questionId', 'checkDetail_questionId' - Undocumented member.
--
-- 'status', 'checkDetail_status' - Status associated to the check.
--
-- 'id', 'checkDetail_id' - Trusted Advisor check ID.
--
-- 'description', 'checkDetail_description' - Trusted Advisor check description.
--
-- 'choiceId', 'checkDetail_choiceId' - Undocumented member.
--
-- 'accountId', 'checkDetail_accountId' - Undocumented member.
--
-- 'reason', 'checkDetail_reason' - Reason associated to the check.
--
-- 'flaggedResources', 'checkDetail_flaggedResources' - Count of flagged resources associated to the check.
--
-- 'pillarId', 'checkDetail_pillarId' - Undocumented member.
--
-- 'updatedAt', 'checkDetail_updatedAt' - Undocumented member.
newCheckDetail ::
  CheckDetail
newCheckDetail =
  CheckDetail'
    { name = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      provider = Prelude.Nothing,
      questionId = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      reason = Prelude.Nothing,
      flaggedResources = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Trusted Advisor check name.
checkDetail_name :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_name = Lens.lens (\CheckDetail' {name} -> name) (\s@CheckDetail' {} a -> s {name = a} :: CheckDetail)

-- | Well-Architected Lens ARN associated to the check.
checkDetail_lensArn :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_lensArn = Lens.lens (\CheckDetail' {lensArn} -> lensArn) (\s@CheckDetail' {} a -> s {lensArn = a} :: CheckDetail)

-- | Provider of the check related to the best practice.
checkDetail_provider :: Lens.Lens' CheckDetail (Prelude.Maybe CheckProvider)
checkDetail_provider = Lens.lens (\CheckDetail' {provider} -> provider) (\s@CheckDetail' {} a -> s {provider = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_questionId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_questionId = Lens.lens (\CheckDetail' {questionId} -> questionId) (\s@CheckDetail' {} a -> s {questionId = a} :: CheckDetail)

-- | Status associated to the check.
checkDetail_status :: Lens.Lens' CheckDetail (Prelude.Maybe CheckStatus)
checkDetail_status = Lens.lens (\CheckDetail' {status} -> status) (\s@CheckDetail' {} a -> s {status = a} :: CheckDetail)

-- | Trusted Advisor check ID.
checkDetail_id :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_id = Lens.lens (\CheckDetail' {id} -> id) (\s@CheckDetail' {} a -> s {id = a} :: CheckDetail)

-- | Trusted Advisor check description.
checkDetail_description :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_description = Lens.lens (\CheckDetail' {description} -> description) (\s@CheckDetail' {} a -> s {description = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_choiceId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_choiceId = Lens.lens (\CheckDetail' {choiceId} -> choiceId) (\s@CheckDetail' {} a -> s {choiceId = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_accountId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_accountId = Lens.lens (\CheckDetail' {accountId} -> accountId) (\s@CheckDetail' {} a -> s {accountId = a} :: CheckDetail)

-- | Reason associated to the check.
checkDetail_reason :: Lens.Lens' CheckDetail (Prelude.Maybe CheckFailureReason)
checkDetail_reason = Lens.lens (\CheckDetail' {reason} -> reason) (\s@CheckDetail' {} a -> s {reason = a} :: CheckDetail)

-- | Count of flagged resources associated to the check.
checkDetail_flaggedResources :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Natural)
checkDetail_flaggedResources = Lens.lens (\CheckDetail' {flaggedResources} -> flaggedResources) (\s@CheckDetail' {} a -> s {flaggedResources = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_pillarId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_pillarId = Lens.lens (\CheckDetail' {pillarId} -> pillarId) (\s@CheckDetail' {} a -> s {pillarId = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_updatedAt :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.UTCTime)
checkDetail_updatedAt = Lens.lens (\CheckDetail' {updatedAt} -> updatedAt) (\s@CheckDetail' {} a -> s {updatedAt = a} :: CheckDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CheckDetail where
  parseJSON =
    Data.withObject
      "CheckDetail"
      ( \x ->
          CheckDetail'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "FlaggedResources")
            Prelude.<*> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable CheckDetail where
  hashWithSalt _salt CheckDetail' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` flaggedResources
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData CheckDetail where
  rnf CheckDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf flaggedResources
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf updatedAt
