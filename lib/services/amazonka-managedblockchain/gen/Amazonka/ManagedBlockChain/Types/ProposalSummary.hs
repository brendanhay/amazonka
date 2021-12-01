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
-- Module      : Amazonka.ManagedBlockChain.Types.ProposalSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.ProposalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.ProposalStatus
import qualified Amazonka.Prelude as Prelude

-- | Properties of a proposal.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newProposalSummary' smart constructor.
data ProposalSummary = ProposalSummary'
  { -- | The status of the proposal. Values are as follows:
    --
    -- -   @IN_PROGRESS@ - The proposal is active and open for member voting.
    --
    -- -   @APPROVED@ - The proposal was approved with sufficient @YES@ votes
    --     among members according to the @VotingPolicy@ specified for the
    --     @Network@. The specified proposal actions are carried out.
    --
    -- -   @REJECTED@ - The proposal was rejected with insufficient @YES@ votes
    --     among members according to the @VotingPolicy@ specified for the
    --     @Network@. The specified @ProposalActions@ are not carried out.
    --
    -- -   @EXPIRED@ - Members did not cast the number of votes required to
    --     determine the proposal outcome before the proposal expired. The
    --     specified @ProposalActions@ are not carried out.
    --
    -- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
    --     a proposal that was approved could not be completed because of an
    --     error.
    status :: Prelude.Maybe ProposalStatus,
    -- | The Amazon Resource Name (ARN) of the proposal. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the member that created the proposal.
    proposedByMemberId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the proposal.
    proposalId :: Prelude.Maybe Prelude.Text,
    -- | The name of the member that created the proposal.
    proposedByMemberName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the proposal expires. This is the @CreationDate@
    -- plus the @ProposalDurationInHours@ that is specified in the
    -- @ProposalThresholdPolicy@. After this date and time, if members have not
    -- cast enough votes to determine the outcome according to the voting
    -- policy, the proposal is @EXPIRED@ and @Actions@ are not carried out.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the proposal was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The description of the proposal.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProposalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'proposalSummary_status' - The status of the proposal. Values are as follows:
--
-- -   @IN_PROGRESS@ - The proposal is active and open for member voting.
--
-- -   @APPROVED@ - The proposal was approved with sufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified proposal actions are carried out.
--
-- -   @REJECTED@ - The proposal was rejected with insufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified @ProposalActions@ are not carried out.
--
-- -   @EXPIRED@ - Members did not cast the number of votes required to
--     determine the proposal outcome before the proposal expired. The
--     specified @ProposalActions@ are not carried out.
--
-- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
--     a proposal that was approved could not be completed because of an
--     error.
--
-- 'arn', 'proposalSummary_arn' - The Amazon Resource Name (ARN) of the proposal. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'proposedByMemberId', 'proposalSummary_proposedByMemberId' - The unique identifier of the member that created the proposal.
--
-- 'proposalId', 'proposalSummary_proposalId' - The unique identifier of the proposal.
--
-- 'proposedByMemberName', 'proposalSummary_proposedByMemberName' - The name of the member that created the proposal.
--
-- 'expirationDate', 'proposalSummary_expirationDate' - The date and time that the proposal expires. This is the @CreationDate@
-- plus the @ProposalDurationInHours@ that is specified in the
-- @ProposalThresholdPolicy@. After this date and time, if members have not
-- cast enough votes to determine the outcome according to the voting
-- policy, the proposal is @EXPIRED@ and @Actions@ are not carried out.
--
-- 'creationDate', 'proposalSummary_creationDate' - The date and time that the proposal was created.
--
-- 'description', 'proposalSummary_description' - The description of the proposal.
newProposalSummary ::
  ProposalSummary
newProposalSummary =
  ProposalSummary'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      proposedByMemberId = Prelude.Nothing,
      proposalId = Prelude.Nothing,
      proposedByMemberName = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the proposal. Values are as follows:
--
-- -   @IN_PROGRESS@ - The proposal is active and open for member voting.
--
-- -   @APPROVED@ - The proposal was approved with sufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified proposal actions are carried out.
--
-- -   @REJECTED@ - The proposal was rejected with insufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified @ProposalActions@ are not carried out.
--
-- -   @EXPIRED@ - Members did not cast the number of votes required to
--     determine the proposal outcome before the proposal expired. The
--     specified @ProposalActions@ are not carried out.
--
-- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
--     a proposal that was approved could not be completed because of an
--     error.
proposalSummary_status :: Lens.Lens' ProposalSummary (Prelude.Maybe ProposalStatus)
proposalSummary_status = Lens.lens (\ProposalSummary' {status} -> status) (\s@ProposalSummary' {} a -> s {status = a} :: ProposalSummary)

-- | The Amazon Resource Name (ARN) of the proposal. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
proposalSummary_arn :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.Text)
proposalSummary_arn = Lens.lens (\ProposalSummary' {arn} -> arn) (\s@ProposalSummary' {} a -> s {arn = a} :: ProposalSummary)

-- | The unique identifier of the member that created the proposal.
proposalSummary_proposedByMemberId :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.Text)
proposalSummary_proposedByMemberId = Lens.lens (\ProposalSummary' {proposedByMemberId} -> proposedByMemberId) (\s@ProposalSummary' {} a -> s {proposedByMemberId = a} :: ProposalSummary)

-- | The unique identifier of the proposal.
proposalSummary_proposalId :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.Text)
proposalSummary_proposalId = Lens.lens (\ProposalSummary' {proposalId} -> proposalId) (\s@ProposalSummary' {} a -> s {proposalId = a} :: ProposalSummary)

-- | The name of the member that created the proposal.
proposalSummary_proposedByMemberName :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.Text)
proposalSummary_proposedByMemberName = Lens.lens (\ProposalSummary' {proposedByMemberName} -> proposedByMemberName) (\s@ProposalSummary' {} a -> s {proposedByMemberName = a} :: ProposalSummary)

-- | The date and time that the proposal expires. This is the @CreationDate@
-- plus the @ProposalDurationInHours@ that is specified in the
-- @ProposalThresholdPolicy@. After this date and time, if members have not
-- cast enough votes to determine the outcome according to the voting
-- policy, the proposal is @EXPIRED@ and @Actions@ are not carried out.
proposalSummary_expirationDate :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.UTCTime)
proposalSummary_expirationDate = Lens.lens (\ProposalSummary' {expirationDate} -> expirationDate) (\s@ProposalSummary' {} a -> s {expirationDate = a} :: ProposalSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time that the proposal was created.
proposalSummary_creationDate :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.UTCTime)
proposalSummary_creationDate = Lens.lens (\ProposalSummary' {creationDate} -> creationDate) (\s@ProposalSummary' {} a -> s {creationDate = a} :: ProposalSummary) Prelude.. Lens.mapping Core._Time

-- | The description of the proposal.
proposalSummary_description :: Lens.Lens' ProposalSummary (Prelude.Maybe Prelude.Text)
proposalSummary_description = Lens.lens (\ProposalSummary' {description} -> description) (\s@ProposalSummary' {} a -> s {description = a} :: ProposalSummary)

instance Core.FromJSON ProposalSummary where
  parseJSON =
    Core.withObject
      "ProposalSummary"
      ( \x ->
          ProposalSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "ProposedByMemberId")
            Prelude.<*> (x Core..:? "ProposalId")
            Prelude.<*> (x Core..:? "ProposedByMemberName")
            Prelude.<*> (x Core..:? "ExpirationDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ProposalSummary where
  hashWithSalt salt' ProposalSummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` proposedByMemberName
      `Prelude.hashWithSalt` proposalId
      `Prelude.hashWithSalt` proposedByMemberId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ProposalSummary where
  rnf ProposalSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf proposedByMemberName
      `Prelude.seq` Prelude.rnf proposalId
      `Prelude.seq` Prelude.rnf proposedByMemberId
      `Prelude.seq` Prelude.rnf arn
