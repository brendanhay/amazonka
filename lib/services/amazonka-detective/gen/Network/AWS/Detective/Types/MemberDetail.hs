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
-- Module      : Network.AWS.Detective.Types.MemberDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Detective.Types.MemberDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.Detective.Types.MemberDisabledReason
import Network.AWS.Detective.Types.MemberStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a member account that was invited to contribute to a
-- behavior graph.
--
-- /See:/ 'newMemberDetail' smart constructor.
data MemberDetail = MemberDetail'
  { -- | The date and time when the graph utilization percentage was last
    -- updated.
    percentOfGraphUtilizationUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The current membership status of the member account. The status can have
    -- one of the following values:
    --
    -- -   @INVITED@ - Indicates that the member was sent an invitation but has
    --     not yet responded.
    --
    -- -   @VERIFICATION_IN_PROGRESS@ - Indicates that Detective is verifying
    --     that the account identifier and email address provided for the
    --     member account match. If they do match, then Detective sends the
    --     invitation. If the email address and account identifier don\'t
    --     match, then the member cannot be added to the behavior graph.
    --
    -- -   @VERIFICATION_FAILED@ - Indicates that the account and email address
    --     provided for the member account do not match, and Detective did not
    --     send an invitation to the account.
    --
    -- -   @ENABLED@ - Indicates that the member account accepted the
    --     invitation to contribute to the behavior graph.
    --
    -- -   @ACCEPTED_BUT_DISABLED@ - Indicates that the member account accepted
    --     the invitation but is prevented from contributing data to the
    --     behavior graph. @DisabledReason@ provides the reason why the member
    --     account is not enabled.
    --
    -- Member accounts that declined an invitation or that were removed from
    -- the behavior graph are not included.
    status :: Prelude.Maybe MemberStatus,
    -- | The date and time that Detective sent the invitation to the member
    -- account. The value is in milliseconds since the epoch.
    invitedTime :: Prelude.Maybe Core.POSIX,
    -- | The AWS account identifier of the administrator account for the behavior
    -- graph.
    administratorId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the behavior graph that the member account was invited to.
    graphArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS account identifier of the administrator account for the behavior
    -- graph.
    masterId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account identifier for the member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | For member accounts with a status of @ACCEPTED_BUT_DISABLED@, the reason
    -- that the member account is not enabled.
    --
    -- The reason can have one of the following values:
    --
    -- -   @VOLUME_TOO_HIGH@ - Indicates that adding the member account would
    --     cause the data volume for the behavior graph to be too high.
    --
    -- -   @VOLUME_UNKNOWN@ - Indicates that Detective is unable to verify the
    --     data volume for the member account. This is usually because the
    --     member account is not enrolled in Amazon GuardDuty.
    disabledReason :: Prelude.Maybe MemberDisabledReason,
    -- | The member account data volume as a percentage of the maximum allowed
    -- data volume. 0 indicates 0 percent, and 100 indicates 100 percent.
    --
    -- Note that this is not the percentage of the behavior graph data volume.
    --
    -- For example, the data volume for the behavior graph is 80 GB per day.
    -- The maximum data volume is 160 GB per day. If the data volume for the
    -- member account is 40 GB per day, then @PercentOfGraphUtilization@ is 25.
    -- It represents 25% of the maximum allowed data volume.
    percentOfGraphUtilization :: Prelude.Maybe Prelude.Double,
    -- | The AWS account root user email address for the member account.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The data and time when the member account data volume was last updated.
    volumeUsageUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the member account was last updated. The value is
    -- in milliseconds since the epoch.
    updatedTime :: Prelude.Maybe Core.POSIX,
    -- | The data volume in bytes per day for the member account.
    volumeUsageInBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentOfGraphUtilizationUpdatedTime', 'memberDetail_percentOfGraphUtilizationUpdatedTime' - The date and time when the graph utilization percentage was last
-- updated.
--
-- 'status', 'memberDetail_status' - The current membership status of the member account. The status can have
-- one of the following values:
--
-- -   @INVITED@ - Indicates that the member was sent an invitation but has
--     not yet responded.
--
-- -   @VERIFICATION_IN_PROGRESS@ - Indicates that Detective is verifying
--     that the account identifier and email address provided for the
--     member account match. If they do match, then Detective sends the
--     invitation. If the email address and account identifier don\'t
--     match, then the member cannot be added to the behavior graph.
--
-- -   @VERIFICATION_FAILED@ - Indicates that the account and email address
--     provided for the member account do not match, and Detective did not
--     send an invitation to the account.
--
-- -   @ENABLED@ - Indicates that the member account accepted the
--     invitation to contribute to the behavior graph.
--
-- -   @ACCEPTED_BUT_DISABLED@ - Indicates that the member account accepted
--     the invitation but is prevented from contributing data to the
--     behavior graph. @DisabledReason@ provides the reason why the member
--     account is not enabled.
--
-- Member accounts that declined an invitation or that were removed from
-- the behavior graph are not included.
--
-- 'invitedTime', 'memberDetail_invitedTime' - The date and time that Detective sent the invitation to the member
-- account. The value is in milliseconds since the epoch.
--
-- 'administratorId', 'memberDetail_administratorId' - The AWS account identifier of the administrator account for the behavior
-- graph.
--
-- 'graphArn', 'memberDetail_graphArn' - The ARN of the behavior graph that the member account was invited to.
--
-- 'masterId', 'memberDetail_masterId' - The AWS account identifier of the administrator account for the behavior
-- graph.
--
-- 'accountId', 'memberDetail_accountId' - The AWS account identifier for the member account.
--
-- 'disabledReason', 'memberDetail_disabledReason' - For member accounts with a status of @ACCEPTED_BUT_DISABLED@, the reason
-- that the member account is not enabled.
--
-- The reason can have one of the following values:
--
-- -   @VOLUME_TOO_HIGH@ - Indicates that adding the member account would
--     cause the data volume for the behavior graph to be too high.
--
-- -   @VOLUME_UNKNOWN@ - Indicates that Detective is unable to verify the
--     data volume for the member account. This is usually because the
--     member account is not enrolled in Amazon GuardDuty.
--
-- 'percentOfGraphUtilization', 'memberDetail_percentOfGraphUtilization' - The member account data volume as a percentage of the maximum allowed
-- data volume. 0 indicates 0 percent, and 100 indicates 100 percent.
--
-- Note that this is not the percentage of the behavior graph data volume.
--
-- For example, the data volume for the behavior graph is 80 GB per day.
-- The maximum data volume is 160 GB per day. If the data volume for the
-- member account is 40 GB per day, then @PercentOfGraphUtilization@ is 25.
-- It represents 25% of the maximum allowed data volume.
--
-- 'emailAddress', 'memberDetail_emailAddress' - The AWS account root user email address for the member account.
--
-- 'volumeUsageUpdatedTime', 'memberDetail_volumeUsageUpdatedTime' - The data and time when the member account data volume was last updated.
--
-- 'updatedTime', 'memberDetail_updatedTime' - The date and time that the member account was last updated. The value is
-- in milliseconds since the epoch.
--
-- 'volumeUsageInBytes', 'memberDetail_volumeUsageInBytes' - The data volume in bytes per day for the member account.
newMemberDetail ::
  MemberDetail
newMemberDetail =
  MemberDetail'
    { percentOfGraphUtilizationUpdatedTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      invitedTime = Prelude.Nothing,
      administratorId = Prelude.Nothing,
      graphArn = Prelude.Nothing,
      masterId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      percentOfGraphUtilization = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      volumeUsageUpdatedTime = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      volumeUsageInBytes = Prelude.Nothing
    }

-- | The date and time when the graph utilization percentage was last
-- updated.
memberDetail_percentOfGraphUtilizationUpdatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_percentOfGraphUtilizationUpdatedTime = Lens.lens (\MemberDetail' {percentOfGraphUtilizationUpdatedTime} -> percentOfGraphUtilizationUpdatedTime) (\s@MemberDetail' {} a -> s {percentOfGraphUtilizationUpdatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The current membership status of the member account. The status can have
-- one of the following values:
--
-- -   @INVITED@ - Indicates that the member was sent an invitation but has
--     not yet responded.
--
-- -   @VERIFICATION_IN_PROGRESS@ - Indicates that Detective is verifying
--     that the account identifier and email address provided for the
--     member account match. If they do match, then Detective sends the
--     invitation. If the email address and account identifier don\'t
--     match, then the member cannot be added to the behavior graph.
--
-- -   @VERIFICATION_FAILED@ - Indicates that the account and email address
--     provided for the member account do not match, and Detective did not
--     send an invitation to the account.
--
-- -   @ENABLED@ - Indicates that the member account accepted the
--     invitation to contribute to the behavior graph.
--
-- -   @ACCEPTED_BUT_DISABLED@ - Indicates that the member account accepted
--     the invitation but is prevented from contributing data to the
--     behavior graph. @DisabledReason@ provides the reason why the member
--     account is not enabled.
--
-- Member accounts that declined an invitation or that were removed from
-- the behavior graph are not included.
memberDetail_status :: Lens.Lens' MemberDetail (Prelude.Maybe MemberStatus)
memberDetail_status = Lens.lens (\MemberDetail' {status} -> status) (\s@MemberDetail' {} a -> s {status = a} :: MemberDetail)

-- | The date and time that Detective sent the invitation to the member
-- account. The value is in milliseconds since the epoch.
memberDetail_invitedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_invitedTime = Lens.lens (\MemberDetail' {invitedTime} -> invitedTime) (\s@MemberDetail' {} a -> s {invitedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The AWS account identifier of the administrator account for the behavior
-- graph.
memberDetail_administratorId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_administratorId = Lens.lens (\MemberDetail' {administratorId} -> administratorId) (\s@MemberDetail' {} a -> s {administratorId = a} :: MemberDetail)

-- | The ARN of the behavior graph that the member account was invited to.
memberDetail_graphArn :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_graphArn = Lens.lens (\MemberDetail' {graphArn} -> graphArn) (\s@MemberDetail' {} a -> s {graphArn = a} :: MemberDetail)

-- | The AWS account identifier of the administrator account for the behavior
-- graph.
memberDetail_masterId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_masterId = Lens.lens (\MemberDetail' {masterId} -> masterId) (\s@MemberDetail' {} a -> s {masterId = a} :: MemberDetail)

-- | The AWS account identifier for the member account.
memberDetail_accountId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_accountId = Lens.lens (\MemberDetail' {accountId} -> accountId) (\s@MemberDetail' {} a -> s {accountId = a} :: MemberDetail)

-- | For member accounts with a status of @ACCEPTED_BUT_DISABLED@, the reason
-- that the member account is not enabled.
--
-- The reason can have one of the following values:
--
-- -   @VOLUME_TOO_HIGH@ - Indicates that adding the member account would
--     cause the data volume for the behavior graph to be too high.
--
-- -   @VOLUME_UNKNOWN@ - Indicates that Detective is unable to verify the
--     data volume for the member account. This is usually because the
--     member account is not enrolled in Amazon GuardDuty.
memberDetail_disabledReason :: Lens.Lens' MemberDetail (Prelude.Maybe MemberDisabledReason)
memberDetail_disabledReason = Lens.lens (\MemberDetail' {disabledReason} -> disabledReason) (\s@MemberDetail' {} a -> s {disabledReason = a} :: MemberDetail)

-- | The member account data volume as a percentage of the maximum allowed
-- data volume. 0 indicates 0 percent, and 100 indicates 100 percent.
--
-- Note that this is not the percentage of the behavior graph data volume.
--
-- For example, the data volume for the behavior graph is 80 GB per day.
-- The maximum data volume is 160 GB per day. If the data volume for the
-- member account is 40 GB per day, then @PercentOfGraphUtilization@ is 25.
-- It represents 25% of the maximum allowed data volume.
memberDetail_percentOfGraphUtilization :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Double)
memberDetail_percentOfGraphUtilization = Lens.lens (\MemberDetail' {percentOfGraphUtilization} -> percentOfGraphUtilization) (\s@MemberDetail' {} a -> s {percentOfGraphUtilization = a} :: MemberDetail)

-- | The AWS account root user email address for the member account.
memberDetail_emailAddress :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_emailAddress = Lens.lens (\MemberDetail' {emailAddress} -> emailAddress) (\s@MemberDetail' {} a -> s {emailAddress = a} :: MemberDetail)

-- | The data and time when the member account data volume was last updated.
memberDetail_volumeUsageUpdatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_volumeUsageUpdatedTime = Lens.lens (\MemberDetail' {volumeUsageUpdatedTime} -> volumeUsageUpdatedTime) (\s@MemberDetail' {} a -> s {volumeUsageUpdatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The date and time that the member account was last updated. The value is
-- in milliseconds since the epoch.
memberDetail_updatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_updatedTime = Lens.lens (\MemberDetail' {updatedTime} -> updatedTime) (\s@MemberDetail' {} a -> s {updatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The data volume in bytes per day for the member account.
memberDetail_volumeUsageInBytes :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Integer)
memberDetail_volumeUsageInBytes = Lens.lens (\MemberDetail' {volumeUsageInBytes} -> volumeUsageInBytes) (\s@MemberDetail' {} a -> s {volumeUsageInBytes = a} :: MemberDetail)

instance Core.FromJSON MemberDetail where
  parseJSON =
    Core.withObject
      "MemberDetail"
      ( \x ->
          MemberDetail'
            Prelude.<$> (x Core..:? "PercentOfGraphUtilizationUpdatedTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "InvitedTime")
            Prelude.<*> (x Core..:? "AdministratorId")
            Prelude.<*> (x Core..:? "GraphArn")
            Prelude.<*> (x Core..:? "MasterId")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "DisabledReason")
            Prelude.<*> (x Core..:? "PercentOfGraphUtilization")
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "VolumeUsageUpdatedTime")
            Prelude.<*> (x Core..:? "UpdatedTime")
            Prelude.<*> (x Core..:? "VolumeUsageInBytes")
      )

instance Prelude.Hashable MemberDetail

instance Prelude.NFData MemberDetail
