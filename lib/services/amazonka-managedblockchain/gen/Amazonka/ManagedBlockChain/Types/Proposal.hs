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
-- Module      : Amazonka.ManagedBlockChain.Types.Proposal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Proposal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.ProposalActions
import Amazonka.ManagedBlockChain.Types.ProposalStatus
import qualified Amazonka.Prelude as Prelude

-- | Properties of a proposal on a Managed Blockchain network.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newProposal' smart constructor.
data Proposal = Proposal'
  { -- | Tags assigned to the proposal. Each tag consists of a key and optional
    -- value.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of the proposal.
    proposalId :: Prelude.Maybe Prelude.Text,
    -- | The current total of @YES@ votes cast on the proposal by members.
    yesVoteCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the proposal. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the proposal was created.
    creationDate :: Prelude.Maybe Data.POSIX,
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
    --     @Network@. The specified @ProposalActions@ aren\'t carried out.
    --
    -- -   @EXPIRED@ - Members didn\'t cast the number of votes required to
    --     determine the proposal outcome before the proposal expired. The
    --     specified @ProposalActions@ aren\'t carried out.
    --
    -- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
    --     a proposal that was approved couldn\'t be completed because of an
    --     error. The @ACTION_FAILED@ status occurs even if only one
    --     ProposalAction fails and other actions are successful.
    status :: Prelude.Maybe ProposalStatus,
    -- | The description of the proposal.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of votes remaining to be cast on the proposal by members. In
    -- other words, the number of members minus the sum of @YES@ votes and @NO@
    -- votes.
    outstandingVoteCount :: Prelude.Maybe Prelude.Int,
    -- | The current total of @NO@ votes cast on the proposal by members.
    noVoteCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the member that created the proposal.
    proposedByMemberId :: Prelude.Maybe Prelude.Text,
    -- | The name of the member that created the proposal.
    proposedByMemberName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network for which the proposal is made.
    networkId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the proposal expires. This is the @CreationDate@
    -- plus the @ProposalDurationInHours@ that is specified in the
    -- @ProposalThresholdPolicy@. After this date and time, if members haven\'t
    -- cast enough votes to determine the outcome according to the voting
    -- policy, the proposal is @EXPIRED@ and @Actions@ aren\'t carried out.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | The actions to perform on the network if the proposal is @APPROVED@.
    actions :: Prelude.Maybe ProposalActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Proposal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'proposal_tags' - Tags assigned to the proposal. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'proposalId', 'proposal_proposalId' - The unique identifier of the proposal.
--
-- 'yesVoteCount', 'proposal_yesVoteCount' - The current total of @YES@ votes cast on the proposal by members.
--
-- 'arn', 'proposal_arn' - The Amazon Resource Name (ARN) of the proposal. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'proposal_creationDate' - The date and time that the proposal was created.
--
-- 'status', 'proposal_status' - The status of the proposal. Values are as follows:
--
-- -   @IN_PROGRESS@ - The proposal is active and open for member voting.
--
-- -   @APPROVED@ - The proposal was approved with sufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified proposal actions are carried out.
--
-- -   @REJECTED@ - The proposal was rejected with insufficient @YES@ votes
--     among members according to the @VotingPolicy@ specified for the
--     @Network@. The specified @ProposalActions@ aren\'t carried out.
--
-- -   @EXPIRED@ - Members didn\'t cast the number of votes required to
--     determine the proposal outcome before the proposal expired. The
--     specified @ProposalActions@ aren\'t carried out.
--
-- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
--     a proposal that was approved couldn\'t be completed because of an
--     error. The @ACTION_FAILED@ status occurs even if only one
--     ProposalAction fails and other actions are successful.
--
-- 'description', 'proposal_description' - The description of the proposal.
--
-- 'outstandingVoteCount', 'proposal_outstandingVoteCount' - The number of votes remaining to be cast on the proposal by members. In
-- other words, the number of members minus the sum of @YES@ votes and @NO@
-- votes.
--
-- 'noVoteCount', 'proposal_noVoteCount' - The current total of @NO@ votes cast on the proposal by members.
--
-- 'proposedByMemberId', 'proposal_proposedByMemberId' - The unique identifier of the member that created the proposal.
--
-- 'proposedByMemberName', 'proposal_proposedByMemberName' - The name of the member that created the proposal.
--
-- 'networkId', 'proposal_networkId' - The unique identifier of the network for which the proposal is made.
--
-- 'expirationDate', 'proposal_expirationDate' - The date and time that the proposal expires. This is the @CreationDate@
-- plus the @ProposalDurationInHours@ that is specified in the
-- @ProposalThresholdPolicy@. After this date and time, if members haven\'t
-- cast enough votes to determine the outcome according to the voting
-- policy, the proposal is @EXPIRED@ and @Actions@ aren\'t carried out.
--
-- 'actions', 'proposal_actions' - The actions to perform on the network if the proposal is @APPROVED@.
newProposal ::
  Proposal
newProposal =
  Proposal'
    { tags = Prelude.Nothing,
      proposalId = Prelude.Nothing,
      yesVoteCount = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      outstandingVoteCount = Prelude.Nothing,
      noVoteCount = Prelude.Nothing,
      proposedByMemberId = Prelude.Nothing,
      proposedByMemberName = Prelude.Nothing,
      networkId = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      actions = Prelude.Nothing
    }

-- | Tags assigned to the proposal. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
proposal_tags :: Lens.Lens' Proposal (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
proposal_tags = Lens.lens (\Proposal' {tags} -> tags) (\s@Proposal' {} a -> s {tags = a} :: Proposal) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the proposal.
proposal_proposalId :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_proposalId = Lens.lens (\Proposal' {proposalId} -> proposalId) (\s@Proposal' {} a -> s {proposalId = a} :: Proposal)

-- | The current total of @YES@ votes cast on the proposal by members.
proposal_yesVoteCount :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Int)
proposal_yesVoteCount = Lens.lens (\Proposal' {yesVoteCount} -> yesVoteCount) (\s@Proposal' {} a -> s {yesVoteCount = a} :: Proposal)

-- | The Amazon Resource Name (ARN) of the proposal. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
proposal_arn :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_arn = Lens.lens (\Proposal' {arn} -> arn) (\s@Proposal' {} a -> s {arn = a} :: Proposal)

-- | The date and time that the proposal was created.
proposal_creationDate :: Lens.Lens' Proposal (Prelude.Maybe Prelude.UTCTime)
proposal_creationDate = Lens.lens (\Proposal' {creationDate} -> creationDate) (\s@Proposal' {} a -> s {creationDate = a} :: Proposal) Prelude.. Lens.mapping Data._Time

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
--     @Network@. The specified @ProposalActions@ aren\'t carried out.
--
-- -   @EXPIRED@ - Members didn\'t cast the number of votes required to
--     determine the proposal outcome before the proposal expired. The
--     specified @ProposalActions@ aren\'t carried out.
--
-- -   @ACTION_FAILED@ - One or more of the specified @ProposalActions@ in
--     a proposal that was approved couldn\'t be completed because of an
--     error. The @ACTION_FAILED@ status occurs even if only one
--     ProposalAction fails and other actions are successful.
proposal_status :: Lens.Lens' Proposal (Prelude.Maybe ProposalStatus)
proposal_status = Lens.lens (\Proposal' {status} -> status) (\s@Proposal' {} a -> s {status = a} :: Proposal)

-- | The description of the proposal.
proposal_description :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_description = Lens.lens (\Proposal' {description} -> description) (\s@Proposal' {} a -> s {description = a} :: Proposal)

-- | The number of votes remaining to be cast on the proposal by members. In
-- other words, the number of members minus the sum of @YES@ votes and @NO@
-- votes.
proposal_outstandingVoteCount :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Int)
proposal_outstandingVoteCount = Lens.lens (\Proposal' {outstandingVoteCount} -> outstandingVoteCount) (\s@Proposal' {} a -> s {outstandingVoteCount = a} :: Proposal)

-- | The current total of @NO@ votes cast on the proposal by members.
proposal_noVoteCount :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Int)
proposal_noVoteCount = Lens.lens (\Proposal' {noVoteCount} -> noVoteCount) (\s@Proposal' {} a -> s {noVoteCount = a} :: Proposal)

-- | The unique identifier of the member that created the proposal.
proposal_proposedByMemberId :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_proposedByMemberId = Lens.lens (\Proposal' {proposedByMemberId} -> proposedByMemberId) (\s@Proposal' {} a -> s {proposedByMemberId = a} :: Proposal)

-- | The name of the member that created the proposal.
proposal_proposedByMemberName :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_proposedByMemberName = Lens.lens (\Proposal' {proposedByMemberName} -> proposedByMemberName) (\s@Proposal' {} a -> s {proposedByMemberName = a} :: Proposal)

-- | The unique identifier of the network for which the proposal is made.
proposal_networkId :: Lens.Lens' Proposal (Prelude.Maybe Prelude.Text)
proposal_networkId = Lens.lens (\Proposal' {networkId} -> networkId) (\s@Proposal' {} a -> s {networkId = a} :: Proposal)

-- | The date and time that the proposal expires. This is the @CreationDate@
-- plus the @ProposalDurationInHours@ that is specified in the
-- @ProposalThresholdPolicy@. After this date and time, if members haven\'t
-- cast enough votes to determine the outcome according to the voting
-- policy, the proposal is @EXPIRED@ and @Actions@ aren\'t carried out.
proposal_expirationDate :: Lens.Lens' Proposal (Prelude.Maybe Prelude.UTCTime)
proposal_expirationDate = Lens.lens (\Proposal' {expirationDate} -> expirationDate) (\s@Proposal' {} a -> s {expirationDate = a} :: Proposal) Prelude.. Lens.mapping Data._Time

-- | The actions to perform on the network if the proposal is @APPROVED@.
proposal_actions :: Lens.Lens' Proposal (Prelude.Maybe ProposalActions)
proposal_actions = Lens.lens (\Proposal' {actions} -> actions) (\s@Proposal' {} a -> s {actions = a} :: Proposal)

instance Data.FromJSON Proposal where
  parseJSON =
    Data.withObject
      "Proposal"
      ( \x ->
          Proposal'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProposalId")
            Prelude.<*> (x Data..:? "YesVoteCount")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "OutstandingVoteCount")
            Prelude.<*> (x Data..:? "NoVoteCount")
            Prelude.<*> (x Data..:? "ProposedByMemberId")
            Prelude.<*> (x Data..:? "ProposedByMemberName")
            Prelude.<*> (x Data..:? "NetworkId")
            Prelude.<*> (x Data..:? "ExpirationDate")
            Prelude.<*> (x Data..:? "Actions")
      )

instance Prelude.Hashable Proposal where
  hashWithSalt _salt Proposal' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` proposalId
      `Prelude.hashWithSalt` yesVoteCount
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` outstandingVoteCount
      `Prelude.hashWithSalt` noVoteCount
      `Prelude.hashWithSalt` proposedByMemberId
      `Prelude.hashWithSalt` proposedByMemberName
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` actions

instance Prelude.NFData Proposal where
  rnf Proposal' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf proposalId
      `Prelude.seq` Prelude.rnf yesVoteCount
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf outstandingVoteCount
      `Prelude.seq` Prelude.rnf noVoteCount
      `Prelude.seq` Prelude.rnf proposedByMemberId
      `Prelude.seq` Prelude.rnf proposedByMemberName
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf actions
