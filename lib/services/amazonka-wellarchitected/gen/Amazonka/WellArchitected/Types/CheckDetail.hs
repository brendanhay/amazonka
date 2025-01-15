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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { accountId :: Prelude.Maybe Prelude.Text,
    choiceId :: Prelude.Maybe Prelude.Text,
    -- | Trusted Advisor check description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Count of flagged resources associated to the check.
    flaggedResources :: Prelude.Maybe Prelude.Natural,
    -- | Trusted Advisor check ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Well-Architected Lens ARN associated to the check.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | Trusted Advisor check name.
    name :: Prelude.Maybe Prelude.Text,
    pillarId :: Prelude.Maybe Prelude.Text,
    -- | Provider of the check related to the best practice.
    provider :: Prelude.Maybe CheckProvider,
    questionId :: Prelude.Maybe Prelude.Text,
    -- | Reason associated to the check.
    reason :: Prelude.Maybe CheckFailureReason,
    -- | Status associated to the check.
    status :: Prelude.Maybe CheckStatus,
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
-- 'accountId', 'checkDetail_accountId' - Undocumented member.
--
-- 'choiceId', 'checkDetail_choiceId' - Undocumented member.
--
-- 'description', 'checkDetail_description' - Trusted Advisor check description.
--
-- 'flaggedResources', 'checkDetail_flaggedResources' - Count of flagged resources associated to the check.
--
-- 'id', 'checkDetail_id' - Trusted Advisor check ID.
--
-- 'lensArn', 'checkDetail_lensArn' - Well-Architected Lens ARN associated to the check.
--
-- 'name', 'checkDetail_name' - Trusted Advisor check name.
--
-- 'pillarId', 'checkDetail_pillarId' - Undocumented member.
--
-- 'provider', 'checkDetail_provider' - Provider of the check related to the best practice.
--
-- 'questionId', 'checkDetail_questionId' - Undocumented member.
--
-- 'reason', 'checkDetail_reason' - Reason associated to the check.
--
-- 'status', 'checkDetail_status' - Status associated to the check.
--
-- 'updatedAt', 'checkDetail_updatedAt' - Undocumented member.
newCheckDetail ::
  CheckDetail
newCheckDetail =
  CheckDetail'
    { accountId = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      description = Prelude.Nothing,
      flaggedResources = Prelude.Nothing,
      id = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      name = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      provider = Prelude.Nothing,
      questionId = Prelude.Nothing,
      reason = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
checkDetail_accountId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_accountId = Lens.lens (\CheckDetail' {accountId} -> accountId) (\s@CheckDetail' {} a -> s {accountId = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_choiceId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_choiceId = Lens.lens (\CheckDetail' {choiceId} -> choiceId) (\s@CheckDetail' {} a -> s {choiceId = a} :: CheckDetail)

-- | Trusted Advisor check description.
checkDetail_description :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_description = Lens.lens (\CheckDetail' {description} -> description) (\s@CheckDetail' {} a -> s {description = a} :: CheckDetail)

-- | Count of flagged resources associated to the check.
checkDetail_flaggedResources :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Natural)
checkDetail_flaggedResources = Lens.lens (\CheckDetail' {flaggedResources} -> flaggedResources) (\s@CheckDetail' {} a -> s {flaggedResources = a} :: CheckDetail)

-- | Trusted Advisor check ID.
checkDetail_id :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_id = Lens.lens (\CheckDetail' {id} -> id) (\s@CheckDetail' {} a -> s {id = a} :: CheckDetail)

-- | Well-Architected Lens ARN associated to the check.
checkDetail_lensArn :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_lensArn = Lens.lens (\CheckDetail' {lensArn} -> lensArn) (\s@CheckDetail' {} a -> s {lensArn = a} :: CheckDetail)

-- | Trusted Advisor check name.
checkDetail_name :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_name = Lens.lens (\CheckDetail' {name} -> name) (\s@CheckDetail' {} a -> s {name = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_pillarId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_pillarId = Lens.lens (\CheckDetail' {pillarId} -> pillarId) (\s@CheckDetail' {} a -> s {pillarId = a} :: CheckDetail)

-- | Provider of the check related to the best practice.
checkDetail_provider :: Lens.Lens' CheckDetail (Prelude.Maybe CheckProvider)
checkDetail_provider = Lens.lens (\CheckDetail' {provider} -> provider) (\s@CheckDetail' {} a -> s {provider = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_questionId :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.Text)
checkDetail_questionId = Lens.lens (\CheckDetail' {questionId} -> questionId) (\s@CheckDetail' {} a -> s {questionId = a} :: CheckDetail)

-- | Reason associated to the check.
checkDetail_reason :: Lens.Lens' CheckDetail (Prelude.Maybe CheckFailureReason)
checkDetail_reason = Lens.lens (\CheckDetail' {reason} -> reason) (\s@CheckDetail' {} a -> s {reason = a} :: CheckDetail)

-- | Status associated to the check.
checkDetail_status :: Lens.Lens' CheckDetail (Prelude.Maybe CheckStatus)
checkDetail_status = Lens.lens (\CheckDetail' {status} -> status) (\s@CheckDetail' {} a -> s {status = a} :: CheckDetail)

-- | Undocumented member.
checkDetail_updatedAt :: Lens.Lens' CheckDetail (Prelude.Maybe Prelude.UTCTime)
checkDetail_updatedAt = Lens.lens (\CheckDetail' {updatedAt} -> updatedAt) (\s@CheckDetail' {} a -> s {updatedAt = a} :: CheckDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CheckDetail where
  parseJSON =
    Data.withObject
      "CheckDetail"
      ( \x ->
          CheckDetail'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FlaggedResources")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable CheckDetail where
  hashWithSalt _salt CheckDetail' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` flaggedResources
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData CheckDetail where
  rnf CheckDetail' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf choiceId `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf flaggedResources `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf lensArn `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf pillarId `Prelude.seq`
                    Prelude.rnf provider `Prelude.seq`
                      Prelude.rnf questionId `Prelude.seq`
                        Prelude.rnf reason `Prelude.seq`
                          Prelude.rnf status `Prelude.seq`
                            Prelude.rnf updatedAt
