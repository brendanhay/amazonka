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
-- Module      : Amazonka.WellArchitected.Types.CheckSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.CheckSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.CheckProvider
import Amazonka.WellArchitected.Types.CheckStatus

-- | Trusted Advisor check summary.
--
-- /See:/ 'newCheckSummary' smart constructor.
data CheckSummary = CheckSummary'
  { -- | Account summary associated to the check.
    accountSummary :: Prelude.Maybe (Prelude.HashMap CheckStatus Prelude.Natural),
    choiceId :: Prelude.Maybe Prelude.Text,
    -- | Trusted Advisor check description.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | Status associated to the check.
    status :: Prelude.Maybe CheckStatus,
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountSummary', 'checkSummary_accountSummary' - Account summary associated to the check.
--
-- 'choiceId', 'checkSummary_choiceId' - Undocumented member.
--
-- 'description', 'checkSummary_description' - Trusted Advisor check description.
--
-- 'id', 'checkSummary_id' - Trusted Advisor check ID.
--
-- 'lensArn', 'checkSummary_lensArn' - Well-Architected Lens ARN associated to the check.
--
-- 'name', 'checkSummary_name' - Trusted Advisor check name.
--
-- 'pillarId', 'checkSummary_pillarId' - Undocumented member.
--
-- 'provider', 'checkSummary_provider' - Provider of the check related to the best practice.
--
-- 'questionId', 'checkSummary_questionId' - Undocumented member.
--
-- 'status', 'checkSummary_status' - Status associated to the check.
--
-- 'updatedAt', 'checkSummary_updatedAt' - Undocumented member.
newCheckSummary ::
  CheckSummary
newCheckSummary =
  CheckSummary'
    { accountSummary = Prelude.Nothing,
      choiceId = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      name = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      provider = Prelude.Nothing,
      questionId = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Account summary associated to the check.
checkSummary_accountSummary :: Lens.Lens' CheckSummary (Prelude.Maybe (Prelude.HashMap CheckStatus Prelude.Natural))
checkSummary_accountSummary = Lens.lens (\CheckSummary' {accountSummary} -> accountSummary) (\s@CheckSummary' {} a -> s {accountSummary = a} :: CheckSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
checkSummary_choiceId :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_choiceId = Lens.lens (\CheckSummary' {choiceId} -> choiceId) (\s@CheckSummary' {} a -> s {choiceId = a} :: CheckSummary)

-- | Trusted Advisor check description.
checkSummary_description :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_description = Lens.lens (\CheckSummary' {description} -> description) (\s@CheckSummary' {} a -> s {description = a} :: CheckSummary)

-- | Trusted Advisor check ID.
checkSummary_id :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_id = Lens.lens (\CheckSummary' {id} -> id) (\s@CheckSummary' {} a -> s {id = a} :: CheckSummary)

-- | Well-Architected Lens ARN associated to the check.
checkSummary_lensArn :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_lensArn = Lens.lens (\CheckSummary' {lensArn} -> lensArn) (\s@CheckSummary' {} a -> s {lensArn = a} :: CheckSummary)

-- | Trusted Advisor check name.
checkSummary_name :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_name = Lens.lens (\CheckSummary' {name} -> name) (\s@CheckSummary' {} a -> s {name = a} :: CheckSummary)

-- | Undocumented member.
checkSummary_pillarId :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_pillarId = Lens.lens (\CheckSummary' {pillarId} -> pillarId) (\s@CheckSummary' {} a -> s {pillarId = a} :: CheckSummary)

-- | Provider of the check related to the best practice.
checkSummary_provider :: Lens.Lens' CheckSummary (Prelude.Maybe CheckProvider)
checkSummary_provider = Lens.lens (\CheckSummary' {provider} -> provider) (\s@CheckSummary' {} a -> s {provider = a} :: CheckSummary)

-- | Undocumented member.
checkSummary_questionId :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.Text)
checkSummary_questionId = Lens.lens (\CheckSummary' {questionId} -> questionId) (\s@CheckSummary' {} a -> s {questionId = a} :: CheckSummary)

-- | Status associated to the check.
checkSummary_status :: Lens.Lens' CheckSummary (Prelude.Maybe CheckStatus)
checkSummary_status = Lens.lens (\CheckSummary' {status} -> status) (\s@CheckSummary' {} a -> s {status = a} :: CheckSummary)

-- | Undocumented member.
checkSummary_updatedAt :: Lens.Lens' CheckSummary (Prelude.Maybe Prelude.UTCTime)
checkSummary_updatedAt = Lens.lens (\CheckSummary' {updatedAt} -> updatedAt) (\s@CheckSummary' {} a -> s {updatedAt = a} :: CheckSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CheckSummary where
  parseJSON =
    Data.withObject
      "CheckSummary"
      ( \x ->
          CheckSummary'
            Prelude.<$> (x Data..:? "AccountSummary" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable CheckSummary where
  hashWithSalt _salt CheckSummary' {..} =
    _salt `Prelude.hashWithSalt` accountSummary
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData CheckSummary where
  rnf CheckSummary' {..} =
    Prelude.rnf accountSummary
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
