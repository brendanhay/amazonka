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
-- Module      : Amazonka.Detective.Types.MemberDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.MemberDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types.DatasourcePackage
import Amazonka.Detective.Types.DatasourcePackageIngestState
import Amazonka.Detective.Types.DatasourcePackageUsageInfo
import Amazonka.Detective.Types.InvitationType
import Amazonka.Detective.Types.MemberDisabledReason
import Amazonka.Detective.Types.MemberStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about a member account in a behavior graph.
--
-- /See:/ 'newMemberDetail' smart constructor.
data MemberDetail = MemberDetail'
  { -- | The member account data volume as a percentage of the maximum allowed
    -- data volume. 0 indicates 0 percent, and 100 indicates 100 percent.
    --
    -- Note that this is not the percentage of the behavior graph data volume.
    --
    -- For example, the data volume for the behavior graph is 80 GB per day.
    -- The maximum data volume is 160 GB per day. If the data volume for the
    -- member account is 40 GB per day, then @PercentOfGraphUtilization@ is 25.
    -- It represents 25% of the maximum allowed data volume.
    percentOfGraphUtilization :: Prelude.Maybe Prelude.Double,
    -- | The data volume in bytes per day for the member account.
    volumeUsageInBytes :: Prelude.Maybe Prelude.Integer,
    -- | For invited accounts, the date and time that Detective sent the
    -- invitation to the account. The value is an ISO8601 formatted string. For
    -- example, @2021-08-18T16:35:56.284Z@.
    invitedTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the behavior graph.
    graphArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account identifier of the administrator account
    -- for the behavior graph.
    administratorId :: Prelude.Maybe Prelude.Text,
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
    -- | The current membership status of the member account. The status can have
    -- one of the following values:
    --
    -- -   @INVITED@ - For invited accounts only. Indicates that the member was
    --     sent an invitation but has not yet responded.
    --
    -- -   @VERIFICATION_IN_PROGRESS@ - For invited accounts only, indicates
    --     that Detective is verifying that the account identifier and email
    --     address provided for the member account match. If they do match,
    --     then Detective sends the invitation. If the email address and
    --     account identifier don\'t match, then the member cannot be added to
    --     the behavior graph.
    --
    --     For organization accounts in the organization behavior graph,
    --     indicates that Detective is verifying that the account belongs to
    --     the organization.
    --
    -- -   @VERIFICATION_FAILED@ - For invited accounts only. Indicates that
    --     the account and email address provided for the member account do not
    --     match, and Detective did not send an invitation to the account.
    --
    -- -   @ENABLED@ - Indicates that the member account currently contributes
    --     data to the behavior graph. For invited accounts, the member account
    --     accepted the invitation. For organization accounts in the
    --     organization behavior graph, the Detective administrator account
    --     enabled the organization account as a member account.
    --
    -- -   @ACCEPTED_BUT_DISABLED@ - The account accepted the invitation, or
    --     was enabled by the Detective administrator account, but is prevented
    --     from contributing data to the behavior graph. @DisabledReason@
    --     provides the reason why the member account is not enabled.
    --
    -- Invited accounts that declined an invitation or that were removed from
    -- the behavior graph are not included. In the organization behavior graph,
    -- organization accounts that the Detective administrator account did not
    -- enable are not included.
    status :: Prelude.Maybe MemberStatus,
    -- | Details on the volume of usage for each data source package in a
    -- behavior graph.
    volumeUsageByDatasourcePackage :: Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageUsageInfo),
    -- | The date and time when the graph utilization percentage was last
    -- updated. The value is an ISO8601 formatted string. For example,
    -- @2021-08-18T16:35:56.284Z@.
    percentOfGraphUtilizationUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account identifier for the member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The data and time when the member account data volume was last updated.
    -- The value is an ISO8601 formatted string. For example,
    -- @2021-08-18T16:35:56.284Z@.
    volumeUsageUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account identifier of the administrator account
    -- for the behavior graph.
    masterId :: Prelude.Maybe Prelude.Text,
    -- | The type of behavior graph membership.
    --
    -- For an organization account in the organization behavior graph, the type
    -- is @ORGANIZATION@.
    --
    -- For an account that was invited to a behavior graph, the type is
    -- @INVITATION@.
    invitationType :: Prelude.Maybe InvitationType,
    -- | The Amazon Web Services account root user email address for the member
    -- account.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the member account was last updated. The value is
    -- an ISO8601 formatted string. For example, @2021-08-18T16:35:56.284Z@.
    updatedTime :: Prelude.Maybe Core.POSIX,
    -- | The state of a data source package for the behavior graph.
    datasourcePackageIngestStates :: Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageIngestState)
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
-- 'volumeUsageInBytes', 'memberDetail_volumeUsageInBytes' - The data volume in bytes per day for the member account.
--
-- 'invitedTime', 'memberDetail_invitedTime' - For invited accounts, the date and time that Detective sent the
-- invitation to the account. The value is an ISO8601 formatted string. For
-- example, @2021-08-18T16:35:56.284Z@.
--
-- 'graphArn', 'memberDetail_graphArn' - The ARN of the behavior graph.
--
-- 'administratorId', 'memberDetail_administratorId' - The Amazon Web Services account identifier of the administrator account
-- for the behavior graph.
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
-- 'status', 'memberDetail_status' - The current membership status of the member account. The status can have
-- one of the following values:
--
-- -   @INVITED@ - For invited accounts only. Indicates that the member was
--     sent an invitation but has not yet responded.
--
-- -   @VERIFICATION_IN_PROGRESS@ - For invited accounts only, indicates
--     that Detective is verifying that the account identifier and email
--     address provided for the member account match. If they do match,
--     then Detective sends the invitation. If the email address and
--     account identifier don\'t match, then the member cannot be added to
--     the behavior graph.
--
--     For organization accounts in the organization behavior graph,
--     indicates that Detective is verifying that the account belongs to
--     the organization.
--
-- -   @VERIFICATION_FAILED@ - For invited accounts only. Indicates that
--     the account and email address provided for the member account do not
--     match, and Detective did not send an invitation to the account.
--
-- -   @ENABLED@ - Indicates that the member account currently contributes
--     data to the behavior graph. For invited accounts, the member account
--     accepted the invitation. For organization accounts in the
--     organization behavior graph, the Detective administrator account
--     enabled the organization account as a member account.
--
-- -   @ACCEPTED_BUT_DISABLED@ - The account accepted the invitation, or
--     was enabled by the Detective administrator account, but is prevented
--     from contributing data to the behavior graph. @DisabledReason@
--     provides the reason why the member account is not enabled.
--
-- Invited accounts that declined an invitation or that were removed from
-- the behavior graph are not included. In the organization behavior graph,
-- organization accounts that the Detective administrator account did not
-- enable are not included.
--
-- 'volumeUsageByDatasourcePackage', 'memberDetail_volumeUsageByDatasourcePackage' - Details on the volume of usage for each data source package in a
-- behavior graph.
--
-- 'percentOfGraphUtilizationUpdatedTime', 'memberDetail_percentOfGraphUtilizationUpdatedTime' - The date and time when the graph utilization percentage was last
-- updated. The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
--
-- 'accountId', 'memberDetail_accountId' - The Amazon Web Services account identifier for the member account.
--
-- 'volumeUsageUpdatedTime', 'memberDetail_volumeUsageUpdatedTime' - The data and time when the member account data volume was last updated.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
--
-- 'masterId', 'memberDetail_masterId' - The Amazon Web Services account identifier of the administrator account
-- for the behavior graph.
--
-- 'invitationType', 'memberDetail_invitationType' - The type of behavior graph membership.
--
-- For an organization account in the organization behavior graph, the type
-- is @ORGANIZATION@.
--
-- For an account that was invited to a behavior graph, the type is
-- @INVITATION@.
--
-- 'emailAddress', 'memberDetail_emailAddress' - The Amazon Web Services account root user email address for the member
-- account.
--
-- 'updatedTime', 'memberDetail_updatedTime' - The date and time that the member account was last updated. The value is
-- an ISO8601 formatted string. For example, @2021-08-18T16:35:56.284Z@.
--
-- 'datasourcePackageIngestStates', 'memberDetail_datasourcePackageIngestStates' - The state of a data source package for the behavior graph.
newMemberDetail ::
  MemberDetail
newMemberDetail =
  MemberDetail'
    { percentOfGraphUtilization =
        Prelude.Nothing,
      volumeUsageInBytes = Prelude.Nothing,
      invitedTime = Prelude.Nothing,
      graphArn = Prelude.Nothing,
      administratorId = Prelude.Nothing,
      disabledReason = Prelude.Nothing,
      status = Prelude.Nothing,
      volumeUsageByDatasourcePackage = Prelude.Nothing,
      percentOfGraphUtilizationUpdatedTime =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      volumeUsageUpdatedTime = Prelude.Nothing,
      masterId = Prelude.Nothing,
      invitationType = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      datasourcePackageIngestStates = Prelude.Nothing
    }

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

-- | The data volume in bytes per day for the member account.
memberDetail_volumeUsageInBytes :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Integer)
memberDetail_volumeUsageInBytes = Lens.lens (\MemberDetail' {volumeUsageInBytes} -> volumeUsageInBytes) (\s@MemberDetail' {} a -> s {volumeUsageInBytes = a} :: MemberDetail)

-- | For invited accounts, the date and time that Detective sent the
-- invitation to the account. The value is an ISO8601 formatted string. For
-- example, @2021-08-18T16:35:56.284Z@.
memberDetail_invitedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_invitedTime = Lens.lens (\MemberDetail' {invitedTime} -> invitedTime) (\s@MemberDetail' {} a -> s {invitedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The ARN of the behavior graph.
memberDetail_graphArn :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_graphArn = Lens.lens (\MemberDetail' {graphArn} -> graphArn) (\s@MemberDetail' {} a -> s {graphArn = a} :: MemberDetail)

-- | The Amazon Web Services account identifier of the administrator account
-- for the behavior graph.
memberDetail_administratorId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_administratorId = Lens.lens (\MemberDetail' {administratorId} -> administratorId) (\s@MemberDetail' {} a -> s {administratorId = a} :: MemberDetail)

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

-- | The current membership status of the member account. The status can have
-- one of the following values:
--
-- -   @INVITED@ - For invited accounts only. Indicates that the member was
--     sent an invitation but has not yet responded.
--
-- -   @VERIFICATION_IN_PROGRESS@ - For invited accounts only, indicates
--     that Detective is verifying that the account identifier and email
--     address provided for the member account match. If they do match,
--     then Detective sends the invitation. If the email address and
--     account identifier don\'t match, then the member cannot be added to
--     the behavior graph.
--
--     For organization accounts in the organization behavior graph,
--     indicates that Detective is verifying that the account belongs to
--     the organization.
--
-- -   @VERIFICATION_FAILED@ - For invited accounts only. Indicates that
--     the account and email address provided for the member account do not
--     match, and Detective did not send an invitation to the account.
--
-- -   @ENABLED@ - Indicates that the member account currently contributes
--     data to the behavior graph. For invited accounts, the member account
--     accepted the invitation. For organization accounts in the
--     organization behavior graph, the Detective administrator account
--     enabled the organization account as a member account.
--
-- -   @ACCEPTED_BUT_DISABLED@ - The account accepted the invitation, or
--     was enabled by the Detective administrator account, but is prevented
--     from contributing data to the behavior graph. @DisabledReason@
--     provides the reason why the member account is not enabled.
--
-- Invited accounts that declined an invitation or that were removed from
-- the behavior graph are not included. In the organization behavior graph,
-- organization accounts that the Detective administrator account did not
-- enable are not included.
memberDetail_status :: Lens.Lens' MemberDetail (Prelude.Maybe MemberStatus)
memberDetail_status = Lens.lens (\MemberDetail' {status} -> status) (\s@MemberDetail' {} a -> s {status = a} :: MemberDetail)

-- | Details on the volume of usage for each data source package in a
-- behavior graph.
memberDetail_volumeUsageByDatasourcePackage :: Lens.Lens' MemberDetail (Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageUsageInfo))
memberDetail_volumeUsageByDatasourcePackage = Lens.lens (\MemberDetail' {volumeUsageByDatasourcePackage} -> volumeUsageByDatasourcePackage) (\s@MemberDetail' {} a -> s {volumeUsageByDatasourcePackage = a} :: MemberDetail) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the graph utilization percentage was last
-- updated. The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
memberDetail_percentOfGraphUtilizationUpdatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_percentOfGraphUtilizationUpdatedTime = Lens.lens (\MemberDetail' {percentOfGraphUtilizationUpdatedTime} -> percentOfGraphUtilizationUpdatedTime) (\s@MemberDetail' {} a -> s {percentOfGraphUtilizationUpdatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account identifier for the member account.
memberDetail_accountId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_accountId = Lens.lens (\MemberDetail' {accountId} -> accountId) (\s@MemberDetail' {} a -> s {accountId = a} :: MemberDetail)

-- | The data and time when the member account data volume was last updated.
-- The value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
memberDetail_volumeUsageUpdatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_volumeUsageUpdatedTime = Lens.lens (\MemberDetail' {volumeUsageUpdatedTime} -> volumeUsageUpdatedTime) (\s@MemberDetail' {} a -> s {volumeUsageUpdatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account identifier of the administrator account
-- for the behavior graph.
memberDetail_masterId :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_masterId = Lens.lens (\MemberDetail' {masterId} -> masterId) (\s@MemberDetail' {} a -> s {masterId = a} :: MemberDetail)

-- | The type of behavior graph membership.
--
-- For an organization account in the organization behavior graph, the type
-- is @ORGANIZATION@.
--
-- For an account that was invited to a behavior graph, the type is
-- @INVITATION@.
memberDetail_invitationType :: Lens.Lens' MemberDetail (Prelude.Maybe InvitationType)
memberDetail_invitationType = Lens.lens (\MemberDetail' {invitationType} -> invitationType) (\s@MemberDetail' {} a -> s {invitationType = a} :: MemberDetail)

-- | The Amazon Web Services account root user email address for the member
-- account.
memberDetail_emailAddress :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.Text)
memberDetail_emailAddress = Lens.lens (\MemberDetail' {emailAddress} -> emailAddress) (\s@MemberDetail' {} a -> s {emailAddress = a} :: MemberDetail)

-- | The date and time that the member account was last updated. The value is
-- an ISO8601 formatted string. For example, @2021-08-18T16:35:56.284Z@.
memberDetail_updatedTime :: Lens.Lens' MemberDetail (Prelude.Maybe Prelude.UTCTime)
memberDetail_updatedTime = Lens.lens (\MemberDetail' {updatedTime} -> updatedTime) (\s@MemberDetail' {} a -> s {updatedTime = a} :: MemberDetail) Prelude.. Lens.mapping Core._Time

-- | The state of a data source package for the behavior graph.
memberDetail_datasourcePackageIngestStates :: Lens.Lens' MemberDetail (Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageIngestState))
memberDetail_datasourcePackageIngestStates = Lens.lens (\MemberDetail' {datasourcePackageIngestStates} -> datasourcePackageIngestStates) (\s@MemberDetail' {} a -> s {datasourcePackageIngestStates = a} :: MemberDetail) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MemberDetail where
  parseJSON =
    Core.withObject
      "MemberDetail"
      ( \x ->
          MemberDetail'
            Prelude.<$> (x Core..:? "PercentOfGraphUtilization")
            Prelude.<*> (x Core..:? "VolumeUsageInBytes")
            Prelude.<*> (x Core..:? "InvitedTime")
            Prelude.<*> (x Core..:? "GraphArn")
            Prelude.<*> (x Core..:? "AdministratorId")
            Prelude.<*> (x Core..:? "DisabledReason")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> ( x Core..:? "VolumeUsageByDatasourcePackage"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PercentOfGraphUtilizationUpdatedTime")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "VolumeUsageUpdatedTime")
            Prelude.<*> (x Core..:? "MasterId")
            Prelude.<*> (x Core..:? "InvitationType")
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "UpdatedTime")
            Prelude.<*> ( x Core..:? "DatasourcePackageIngestStates"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MemberDetail where
  hashWithSalt _salt MemberDetail' {..} =
    _salt
      `Prelude.hashWithSalt` percentOfGraphUtilization
      `Prelude.hashWithSalt` volumeUsageInBytes
      `Prelude.hashWithSalt` invitedTime
      `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` administratorId
      `Prelude.hashWithSalt` disabledReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` volumeUsageByDatasourcePackage
      `Prelude.hashWithSalt` percentOfGraphUtilizationUpdatedTime
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` volumeUsageUpdatedTime
      `Prelude.hashWithSalt` masterId
      `Prelude.hashWithSalt` invitationType
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` updatedTime
      `Prelude.hashWithSalt` datasourcePackageIngestStates

instance Prelude.NFData MemberDetail where
  rnf MemberDetail' {..} =
    Prelude.rnf percentOfGraphUtilization
      `Prelude.seq` Prelude.rnf volumeUsageInBytes
      `Prelude.seq` Prelude.rnf invitedTime
      `Prelude.seq` Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf administratorId
      `Prelude.seq` Prelude.rnf disabledReason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf volumeUsageByDatasourcePackage
      `Prelude.seq` Prelude.rnf percentOfGraphUtilizationUpdatedTime
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf volumeUsageUpdatedTime
      `Prelude.seq` Prelude.rnf masterId
      `Prelude.seq` Prelude.rnf invitationType
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf updatedTime
      `Prelude.seq` Prelude.rnf
        datasourcePackageIngestStates
