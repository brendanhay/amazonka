{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.DescribeFeatureGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to describe a @FeatureGroup@. The response includes
-- information on the creation time, @FeatureGroup@ name, the unique
-- identifier for each @FeatureGroup@, and more.
module Amazonka.SageMaker.DescribeFeatureGroup
  ( -- * Creating a Request
    DescribeFeatureGroup (..),
    newDescribeFeatureGroup,

    -- * Request Lenses
    describeFeatureGroup_nextToken,
    describeFeatureGroup_featureGroupName,

    -- * Destructuring the Response
    DescribeFeatureGroupResponse (..),
    newDescribeFeatureGroupResponse,

    -- * Response Lenses
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_lastModifiedTime,
    describeFeatureGroupResponse_lastUpdateStatus,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_onlineStoreConfig,
    describeFeatureGroupResponse_onlineStoreTotalSizeBytes,
    describeFeatureGroupResponse_roleArn,
    describeFeatureGroupResponse_httpStatus,
    describeFeatureGroupResponse_featureGroupArn,
    describeFeatureGroupResponse_featureGroupName,
    describeFeatureGroupResponse_recordIdentifierFeatureName,
    describeFeatureGroupResponse_eventTimeFeatureName,
    describeFeatureGroupResponse_featureDefinitions,
    describeFeatureGroupResponse_creationTime,
    describeFeatureGroupResponse_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeFeatureGroup' smart constructor.
data DescribeFeatureGroup = DescribeFeatureGroup'
  { -- | A token to resume pagination of the list of @Features@
    -- (@FeatureDefinitions@). 2,500 @Features@ are returned by default.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the @FeatureGroup@ you want described.
    featureGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFeatureGroup_nextToken' - A token to resume pagination of the list of @Features@
-- (@FeatureDefinitions@). 2,500 @Features@ are returned by default.
--
-- 'featureGroupName', 'describeFeatureGroup_featureGroupName' - The name of the @FeatureGroup@ you want described.
newDescribeFeatureGroup ::
  -- | 'featureGroupName'
  Prelude.Text ->
  DescribeFeatureGroup
newDescribeFeatureGroup pFeatureGroupName_ =
  DescribeFeatureGroup'
    { nextToken = Prelude.Nothing,
      featureGroupName = pFeatureGroupName_
    }

-- | A token to resume pagination of the list of @Features@
-- (@FeatureDefinitions@). 2,500 @Features@ are returned by default.
describeFeatureGroup_nextToken :: Lens.Lens' DescribeFeatureGroup (Prelude.Maybe Prelude.Text)
describeFeatureGroup_nextToken = Lens.lens (\DescribeFeatureGroup' {nextToken} -> nextToken) (\s@DescribeFeatureGroup' {} a -> s {nextToken = a} :: DescribeFeatureGroup)

-- | The name of the @FeatureGroup@ you want described.
describeFeatureGroup_featureGroupName :: Lens.Lens' DescribeFeatureGroup Prelude.Text
describeFeatureGroup_featureGroupName = Lens.lens (\DescribeFeatureGroup' {featureGroupName} -> featureGroupName) (\s@DescribeFeatureGroup' {} a -> s {featureGroupName = a} :: DescribeFeatureGroup)

instance Core.AWSRequest DescribeFeatureGroup where
  type
    AWSResponse DescribeFeatureGroup =
      DescribeFeatureGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeatureGroupResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "FeatureGroupStatus")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LastUpdateStatus")
            Prelude.<*> (x Data..?> "OfflineStoreConfig")
            Prelude.<*> (x Data..?> "OfflineStoreStatus")
            Prelude.<*> (x Data..?> "OnlineStoreConfig")
            Prelude.<*> (x Data..?> "OnlineStoreTotalSizeBytes")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FeatureGroupArn")
            Prelude.<*> (x Data..:> "FeatureGroupName")
            Prelude.<*> (x Data..:> "RecordIdentifierFeatureName")
            Prelude.<*> (x Data..:> "EventTimeFeatureName")
            Prelude.<*> (x Data..:> "FeatureDefinitions")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "NextToken")
      )

instance Prelude.Hashable DescribeFeatureGroup where
  hashWithSalt _salt DescribeFeatureGroup' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` featureGroupName

instance Prelude.NFData DescribeFeatureGroup where
  rnf DescribeFeatureGroup' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf featureGroupName

instance Data.ToHeaders DescribeFeatureGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeFeatureGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFeatureGroup where
  toJSON DescribeFeatureGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("FeatureGroupName" Data..= featureGroupName)
          ]
      )

instance Data.ToPath DescribeFeatureGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFeatureGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeatureGroupResponse' smart constructor.
data DescribeFeatureGroupResponse = DescribeFeatureGroupResponse'
  { -- | A free form description of the feature group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason that the @FeatureGroup@ failed to be replicated in the
    -- @OfflineStore@. This is failure can occur because:
    --
    -- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
    --
    -- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the feature group.
    featureGroupStatus :: Prelude.Maybe FeatureGroupStatus,
    -- | A timestamp indicating when the feature group was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A value indicating whether the update made to the feature group was
    -- successful.
    lastUpdateStatus :: Prelude.Maybe LastUpdateStatus,
    -- | The configuration of the offline store. It includes the following
    -- configurations:
    --
    -- -   Amazon S3 location of the offline store.
    --
    -- -   Configuration of the Glue data catalog.
    --
    -- -   Table format of the offline store.
    --
    -- -   Option to disable the automatic creation of a Glue table for the
    --     offline store.
    --
    -- -   Encryption configuration.
    offlineStoreConfig :: Prelude.Maybe OfflineStoreConfig,
    -- | The status of the @OfflineStore@. Notifies you if replicating data into
    -- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
    offlineStoreStatus :: Prelude.Maybe OfflineStoreStatus,
    -- | The configuration for the @OnlineStore@.
    onlineStoreConfig :: Prelude.Maybe OnlineStoreConfig,
    -- | The size of the @OnlineStore@ in bytes.
    onlineStoreTotalSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
    -- data into the OfflineStore if an OfflineStoreConfig is provided.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the @FeatureGroup@.
    featureGroupArn :: Prelude.Text,
    -- | he name of the @FeatureGroup@.
    featureGroupName :: Prelude.Text,
    -- | The name of the @Feature@ used for @RecordIdentifier@, whose value
    -- uniquely identifies a record stored in the feature store.
    recordIdentifierFeatureName :: Prelude.Text,
    -- | The name of the feature that stores the @EventTime@ of a Record in a
    -- @FeatureGroup@.
    --
    -- An @EventTime@ is a point in time when a new event occurs that
    -- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
    -- All @Records@ in the @FeatureGroup@ have a corresponding @EventTime@.
    eventTimeFeatureName :: Prelude.Text,
    -- | A list of the @Features@ in the @FeatureGroup@. Each feature is defined
    -- by a @FeatureName@ and @FeatureType@.
    featureDefinitions :: Prelude.NonEmpty FeatureDefinition,
    -- | A timestamp indicating when SageMaker created the @FeatureGroup@.
    creationTime :: Data.POSIX,
    -- | A token to resume pagination of the list of @Features@
    -- (@FeatureDefinitions@).
    nextToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeFeatureGroupResponse_description' - A free form description of the feature group.
--
-- 'failureReason', 'describeFeatureGroupResponse_failureReason' - The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure can occur because:
--
-- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
--
-- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
--
-- 'featureGroupStatus', 'describeFeatureGroupResponse_featureGroupStatus' - The status of the feature group.
--
-- 'lastModifiedTime', 'describeFeatureGroupResponse_lastModifiedTime' - A timestamp indicating when the feature group was last updated.
--
-- 'lastUpdateStatus', 'describeFeatureGroupResponse_lastUpdateStatus' - A value indicating whether the update made to the feature group was
-- successful.
--
-- 'offlineStoreConfig', 'describeFeatureGroupResponse_offlineStoreConfig' - The configuration of the offline store. It includes the following
-- configurations:
--
-- -   Amazon S3 location of the offline store.
--
-- -   Configuration of the Glue data catalog.
--
-- -   Table format of the offline store.
--
-- -   Option to disable the automatic creation of a Glue table for the
--     offline store.
--
-- -   Encryption configuration.
--
-- 'offlineStoreStatus', 'describeFeatureGroupResponse_offlineStoreStatus' - The status of the @OfflineStore@. Notifies you if replicating data into
-- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
--
-- 'onlineStoreConfig', 'describeFeatureGroupResponse_onlineStoreConfig' - The configuration for the @OnlineStore@.
--
-- 'onlineStoreTotalSizeBytes', 'describeFeatureGroupResponse_onlineStoreTotalSizeBytes' - The size of the @OnlineStore@ in bytes.
--
-- 'roleArn', 'describeFeatureGroupResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the OfflineStore if an OfflineStoreConfig is provided.
--
-- 'httpStatus', 'describeFeatureGroupResponse_httpStatus' - The response's http status code.
--
-- 'featureGroupArn', 'describeFeatureGroupResponse_featureGroupArn' - The Amazon Resource Name (ARN) of the @FeatureGroup@.
--
-- 'featureGroupName', 'describeFeatureGroupResponse_featureGroupName' - he name of the @FeatureGroup@.
--
-- 'recordIdentifierFeatureName', 'describeFeatureGroupResponse_recordIdentifierFeatureName' - The name of the @Feature@ used for @RecordIdentifier@, whose value
-- uniquely identifies a record stored in the feature store.
--
-- 'eventTimeFeatureName', 'describeFeatureGroupResponse_eventTimeFeatureName' - The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- An @EventTime@ is a point in time when a new event occurs that
-- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
-- All @Records@ in the @FeatureGroup@ have a corresponding @EventTime@.
--
-- 'featureDefinitions', 'describeFeatureGroupResponse_featureDefinitions' - A list of the @Features@ in the @FeatureGroup@. Each feature is defined
-- by a @FeatureName@ and @FeatureType@.
--
-- 'creationTime', 'describeFeatureGroupResponse_creationTime' - A timestamp indicating when SageMaker created the @FeatureGroup@.
--
-- 'nextToken', 'describeFeatureGroupResponse_nextToken' - A token to resume pagination of the list of @Features@
-- (@FeatureDefinitions@).
newDescribeFeatureGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'featureGroupArn'
  Prelude.Text ->
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierFeatureName'
  Prelude.Text ->
  -- | 'eventTimeFeatureName'
  Prelude.Text ->
  -- | 'featureDefinitions'
  Prelude.NonEmpty FeatureDefinition ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'nextToken'
  Prelude.Text ->
  DescribeFeatureGroupResponse
newDescribeFeatureGroupResponse
  pHttpStatus_
  pFeatureGroupArn_
  pFeatureGroupName_
  pRecordIdentifierFeatureName_
  pEventTimeFeatureName_
  pFeatureDefinitions_
  pCreationTime_
  pNextToken_ =
    DescribeFeatureGroupResponse'
      { description =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        featureGroupStatus = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        lastUpdateStatus = Prelude.Nothing,
        offlineStoreConfig = Prelude.Nothing,
        offlineStoreStatus = Prelude.Nothing,
        onlineStoreConfig = Prelude.Nothing,
        onlineStoreTotalSizeBytes = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        featureGroupArn = pFeatureGroupArn_,
        featureGroupName = pFeatureGroupName_,
        recordIdentifierFeatureName =
          pRecordIdentifierFeatureName_,
        eventTimeFeatureName = pEventTimeFeatureName_,
        featureDefinitions =
          Lens.coerced Lens.# pFeatureDefinitions_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        nextToken = pNextToken_
      }

-- | A free form description of the feature group.
describeFeatureGroupResponse_description :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_description = Lens.lens (\DescribeFeatureGroupResponse' {description} -> description) (\s@DescribeFeatureGroupResponse' {} a -> s {description = a} :: DescribeFeatureGroupResponse)

-- | The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure can occur because:
--
-- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
--
-- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
describeFeatureGroupResponse_failureReason :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_failureReason = Lens.lens (\DescribeFeatureGroupResponse' {failureReason} -> failureReason) (\s@DescribeFeatureGroupResponse' {} a -> s {failureReason = a} :: DescribeFeatureGroupResponse)

-- | The status of the feature group.
describeFeatureGroupResponse_featureGroupStatus :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe FeatureGroupStatus)
describeFeatureGroupResponse_featureGroupStatus = Lens.lens (\DescribeFeatureGroupResponse' {featureGroupStatus} -> featureGroupStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {featureGroupStatus = a} :: DescribeFeatureGroupResponse)

-- | A timestamp indicating when the feature group was last updated.
describeFeatureGroupResponse_lastModifiedTime :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeFeatureGroupResponse_lastModifiedTime = Lens.lens (\DescribeFeatureGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeFeatureGroupResponse' {} a -> s {lastModifiedTime = a} :: DescribeFeatureGroupResponse) Prelude.. Lens.mapping Data._Time

-- | A value indicating whether the update made to the feature group was
-- successful.
describeFeatureGroupResponse_lastUpdateStatus :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe LastUpdateStatus)
describeFeatureGroupResponse_lastUpdateStatus = Lens.lens (\DescribeFeatureGroupResponse' {lastUpdateStatus} -> lastUpdateStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {lastUpdateStatus = a} :: DescribeFeatureGroupResponse)

-- | The configuration of the offline store. It includes the following
-- configurations:
--
-- -   Amazon S3 location of the offline store.
--
-- -   Configuration of the Glue data catalog.
--
-- -   Table format of the offline store.
--
-- -   Option to disable the automatic creation of a Glue table for the
--     offline store.
--
-- -   Encryption configuration.
describeFeatureGroupResponse_offlineStoreConfig :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OfflineStoreConfig)
describeFeatureGroupResponse_offlineStoreConfig = Lens.lens (\DescribeFeatureGroupResponse' {offlineStoreConfig} -> offlineStoreConfig) (\s@DescribeFeatureGroupResponse' {} a -> s {offlineStoreConfig = a} :: DescribeFeatureGroupResponse)

-- | The status of the @OfflineStore@. Notifies you if replicating data into
-- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
describeFeatureGroupResponse_offlineStoreStatus :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OfflineStoreStatus)
describeFeatureGroupResponse_offlineStoreStatus = Lens.lens (\DescribeFeatureGroupResponse' {offlineStoreStatus} -> offlineStoreStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {offlineStoreStatus = a} :: DescribeFeatureGroupResponse)

-- | The configuration for the @OnlineStore@.
describeFeatureGroupResponse_onlineStoreConfig :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OnlineStoreConfig)
describeFeatureGroupResponse_onlineStoreConfig = Lens.lens (\DescribeFeatureGroupResponse' {onlineStoreConfig} -> onlineStoreConfig) (\s@DescribeFeatureGroupResponse' {} a -> s {onlineStoreConfig = a} :: DescribeFeatureGroupResponse)

-- | The size of the @OnlineStore@ in bytes.
describeFeatureGroupResponse_onlineStoreTotalSizeBytes :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Integer)
describeFeatureGroupResponse_onlineStoreTotalSizeBytes = Lens.lens (\DescribeFeatureGroupResponse' {onlineStoreTotalSizeBytes} -> onlineStoreTotalSizeBytes) (\s@DescribeFeatureGroupResponse' {} a -> s {onlineStoreTotalSizeBytes = a} :: DescribeFeatureGroupResponse)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the OfflineStore if an OfflineStoreConfig is provided.
describeFeatureGroupResponse_roleArn :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_roleArn = Lens.lens (\DescribeFeatureGroupResponse' {roleArn} -> roleArn) (\s@DescribeFeatureGroupResponse' {} a -> s {roleArn = a} :: DescribeFeatureGroupResponse)

-- | The response's http status code.
describeFeatureGroupResponse_httpStatus :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Int
describeFeatureGroupResponse_httpStatus = Lens.lens (\DescribeFeatureGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {httpStatus = a} :: DescribeFeatureGroupResponse)

-- | The Amazon Resource Name (ARN) of the @FeatureGroup@.
describeFeatureGroupResponse_featureGroupArn :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_featureGroupArn = Lens.lens (\DescribeFeatureGroupResponse' {featureGroupArn} -> featureGroupArn) (\s@DescribeFeatureGroupResponse' {} a -> s {featureGroupArn = a} :: DescribeFeatureGroupResponse)

-- | he name of the @FeatureGroup@.
describeFeatureGroupResponse_featureGroupName :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_featureGroupName = Lens.lens (\DescribeFeatureGroupResponse' {featureGroupName} -> featureGroupName) (\s@DescribeFeatureGroupResponse' {} a -> s {featureGroupName = a} :: DescribeFeatureGroupResponse)

-- | The name of the @Feature@ used for @RecordIdentifier@, whose value
-- uniquely identifies a record stored in the feature store.
describeFeatureGroupResponse_recordIdentifierFeatureName :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_recordIdentifierFeatureName = Lens.lens (\DescribeFeatureGroupResponse' {recordIdentifierFeatureName} -> recordIdentifierFeatureName) (\s@DescribeFeatureGroupResponse' {} a -> s {recordIdentifierFeatureName = a} :: DescribeFeatureGroupResponse)

-- | The name of the feature that stores the @EventTime@ of a Record in a
-- @FeatureGroup@.
--
-- An @EventTime@ is a point in time when a new event occurs that
-- corresponds to the creation or update of a @Record@ in a @FeatureGroup@.
-- All @Records@ in the @FeatureGroup@ have a corresponding @EventTime@.
describeFeatureGroupResponse_eventTimeFeatureName :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_eventTimeFeatureName = Lens.lens (\DescribeFeatureGroupResponse' {eventTimeFeatureName} -> eventTimeFeatureName) (\s@DescribeFeatureGroupResponse' {} a -> s {eventTimeFeatureName = a} :: DescribeFeatureGroupResponse)

-- | A list of the @Features@ in the @FeatureGroup@. Each feature is defined
-- by a @FeatureName@ and @FeatureType@.
describeFeatureGroupResponse_featureDefinitions :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.NonEmpty FeatureDefinition)
describeFeatureGroupResponse_featureDefinitions = Lens.lens (\DescribeFeatureGroupResponse' {featureDefinitions} -> featureDefinitions) (\s@DescribeFeatureGroupResponse' {} a -> s {featureDefinitions = a} :: DescribeFeatureGroupResponse) Prelude.. Lens.coerced

-- | A timestamp indicating when SageMaker created the @FeatureGroup@.
describeFeatureGroupResponse_creationTime :: Lens.Lens' DescribeFeatureGroupResponse Prelude.UTCTime
describeFeatureGroupResponse_creationTime = Lens.lens (\DescribeFeatureGroupResponse' {creationTime} -> creationTime) (\s@DescribeFeatureGroupResponse' {} a -> s {creationTime = a} :: DescribeFeatureGroupResponse) Prelude.. Data._Time

-- | A token to resume pagination of the list of @Features@
-- (@FeatureDefinitions@).
describeFeatureGroupResponse_nextToken :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_nextToken = Lens.lens (\DescribeFeatureGroupResponse' {nextToken} -> nextToken) (\s@DescribeFeatureGroupResponse' {} a -> s {nextToken = a} :: DescribeFeatureGroupResponse)

instance Prelude.NFData DescribeFeatureGroupResponse where
  rnf DescribeFeatureGroupResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf featureGroupStatus
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf offlineStoreConfig
      `Prelude.seq` Prelude.rnf offlineStoreStatus
      `Prelude.seq` Prelude.rnf onlineStoreConfig
      `Prelude.seq` Prelude.rnf onlineStoreTotalSizeBytes
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf featureGroupArn
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf recordIdentifierFeatureName
      `Prelude.seq` Prelude.rnf eventTimeFeatureName
      `Prelude.seq` Prelude.rnf featureDefinitions
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf nextToken
