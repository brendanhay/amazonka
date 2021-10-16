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
-- Module      : Network.AWS.SageMaker.DescribeFeatureGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to describe a @FeatureGroup@. The response includes
-- information on the creation time, @FeatureGroup@ name, the unique
-- identifier for each @FeatureGroup@, and more.
module Network.AWS.SageMaker.DescribeFeatureGroup
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
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_roleArn,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_onlineStoreConfig,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeatureGroupResponse'
            Prelude.<$> (x Core..?> "FeatureGroupStatus")
            Prelude.<*> (x Core..?> "OfflineStoreConfig")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "OfflineStoreStatus")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "OnlineStoreConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FeatureGroupArn")
            Prelude.<*> (x Core..:> "FeatureGroupName")
            Prelude.<*> (x Core..:> "RecordIdentifierFeatureName")
            Prelude.<*> (x Core..:> "EventTimeFeatureName")
            Prelude.<*> (x Core..:> "FeatureDefinitions")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "NextToken")
      )

instance Prelude.Hashable DescribeFeatureGroup

instance Prelude.NFData DescribeFeatureGroup

instance Core.ToHeaders DescribeFeatureGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeFeatureGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFeatureGroup where
  toJSON DescribeFeatureGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just
              ("FeatureGroupName" Core..= featureGroupName)
          ]
      )

instance Core.ToPath DescribeFeatureGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFeatureGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeatureGroupResponse' smart constructor.
data DescribeFeatureGroupResponse = DescribeFeatureGroupResponse'
  { -- | The status of the feature group.
    featureGroupStatus :: Prelude.Maybe FeatureGroupStatus,
    -- | The configuration of the @OfflineStore@, inducing the S3 location of the
    -- @OfflineStore@, Amazon Web Services Glue or Amazon Web Services Hive
    -- data catalogue configurations, and the security configuration.
    offlineStoreConfig :: Prelude.Maybe OfflineStoreConfig,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
    -- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the @OfflineStore@. Notifies you if replicating data into
    -- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
    offlineStoreStatus :: Prelude.Maybe OfflineStoreStatus,
    -- | The reason that the @FeatureGroup@ failed to be replicated in the
    -- @OfflineStore@. This is failure can occur because:
    --
    -- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
    --
    -- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A free form description of the feature group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the @OnlineStore@.
    onlineStoreConfig :: Prelude.Maybe OnlineStoreConfig,
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
    creationTime :: Core.POSIX,
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
-- 'featureGroupStatus', 'describeFeatureGroupResponse_featureGroupStatus' - The status of the feature group.
--
-- 'offlineStoreConfig', 'describeFeatureGroupResponse_offlineStoreConfig' - The configuration of the @OfflineStore@, inducing the S3 location of the
-- @OfflineStore@, Amazon Web Services Glue or Amazon Web Services Hive
-- data catalogue configurations, and the security configuration.
--
-- 'roleArn', 'describeFeatureGroupResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
--
-- 'offlineStoreStatus', 'describeFeatureGroupResponse_offlineStoreStatus' - The status of the @OfflineStore@. Notifies you if replicating data into
-- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
--
-- 'failureReason', 'describeFeatureGroupResponse_failureReason' - The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure can occur because:
--
-- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
--
-- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
--
-- 'description', 'describeFeatureGroupResponse_description' - A free form description of the feature group.
--
-- 'onlineStoreConfig', 'describeFeatureGroupResponse_onlineStoreConfig' - The configuration for the @OnlineStore@.
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
      { featureGroupStatus =
          Prelude.Nothing,
        offlineStoreConfig = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        offlineStoreStatus = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        description = Prelude.Nothing,
        onlineStoreConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        featureGroupArn = pFeatureGroupArn_,
        featureGroupName = pFeatureGroupName_,
        recordIdentifierFeatureName =
          pRecordIdentifierFeatureName_,
        eventTimeFeatureName = pEventTimeFeatureName_,
        featureDefinitions =
          Lens._Coerce Lens.# pFeatureDefinitions_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        nextToken = pNextToken_
      }

-- | The status of the feature group.
describeFeatureGroupResponse_featureGroupStatus :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe FeatureGroupStatus)
describeFeatureGroupResponse_featureGroupStatus = Lens.lens (\DescribeFeatureGroupResponse' {featureGroupStatus} -> featureGroupStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {featureGroupStatus = a} :: DescribeFeatureGroupResponse)

-- | The configuration of the @OfflineStore@, inducing the S3 location of the
-- @OfflineStore@, Amazon Web Services Glue or Amazon Web Services Hive
-- data catalogue configurations, and the security configuration.
describeFeatureGroupResponse_offlineStoreConfig :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OfflineStoreConfig)
describeFeatureGroupResponse_offlineStoreConfig = Lens.lens (\DescribeFeatureGroupResponse' {offlineStoreConfig} -> offlineStoreConfig) (\s@DescribeFeatureGroupResponse' {} a -> s {offlineStoreConfig = a} :: DescribeFeatureGroupResponse)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to persist
-- data into the @OfflineStore@ if an @OfflineStoreConfig@ is provided.
describeFeatureGroupResponse_roleArn :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_roleArn = Lens.lens (\DescribeFeatureGroupResponse' {roleArn} -> roleArn) (\s@DescribeFeatureGroupResponse' {} a -> s {roleArn = a} :: DescribeFeatureGroupResponse)

-- | The status of the @OfflineStore@. Notifies you if replicating data into
-- the @OfflineStore@ has failed. Returns either: @Active@ or @Blocked@
describeFeatureGroupResponse_offlineStoreStatus :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OfflineStoreStatus)
describeFeatureGroupResponse_offlineStoreStatus = Lens.lens (\DescribeFeatureGroupResponse' {offlineStoreStatus} -> offlineStoreStatus) (\s@DescribeFeatureGroupResponse' {} a -> s {offlineStoreStatus = a} :: DescribeFeatureGroupResponse)

-- | The reason that the @FeatureGroup@ failed to be replicated in the
-- @OfflineStore@. This is failure can occur because:
--
-- -   The @FeatureGroup@ could not be created in the @OfflineStore@.
--
-- -   The @FeatureGroup@ could not be deleted from the @OfflineStore@.
describeFeatureGroupResponse_failureReason :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_failureReason = Lens.lens (\DescribeFeatureGroupResponse' {failureReason} -> failureReason) (\s@DescribeFeatureGroupResponse' {} a -> s {failureReason = a} :: DescribeFeatureGroupResponse)

-- | A free form description of the feature group.
describeFeatureGroupResponse_description :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe Prelude.Text)
describeFeatureGroupResponse_description = Lens.lens (\DescribeFeatureGroupResponse' {description} -> description) (\s@DescribeFeatureGroupResponse' {} a -> s {description = a} :: DescribeFeatureGroupResponse)

-- | The configuration for the @OnlineStore@.
describeFeatureGroupResponse_onlineStoreConfig :: Lens.Lens' DescribeFeatureGroupResponse (Prelude.Maybe OnlineStoreConfig)
describeFeatureGroupResponse_onlineStoreConfig = Lens.lens (\DescribeFeatureGroupResponse' {onlineStoreConfig} -> onlineStoreConfig) (\s@DescribeFeatureGroupResponse' {} a -> s {onlineStoreConfig = a} :: DescribeFeatureGroupResponse)

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
describeFeatureGroupResponse_featureDefinitions = Lens.lens (\DescribeFeatureGroupResponse' {featureDefinitions} -> featureDefinitions) (\s@DescribeFeatureGroupResponse' {} a -> s {featureDefinitions = a} :: DescribeFeatureGroupResponse) Prelude.. Lens._Coerce

-- | A timestamp indicating when SageMaker created the @FeatureGroup@.
describeFeatureGroupResponse_creationTime :: Lens.Lens' DescribeFeatureGroupResponse Prelude.UTCTime
describeFeatureGroupResponse_creationTime = Lens.lens (\DescribeFeatureGroupResponse' {creationTime} -> creationTime) (\s@DescribeFeatureGroupResponse' {} a -> s {creationTime = a} :: DescribeFeatureGroupResponse) Prelude.. Core._Time

-- | A token to resume pagination of the list of @Features@
-- (@FeatureDefinitions@).
describeFeatureGroupResponse_nextToken :: Lens.Lens' DescribeFeatureGroupResponse Prelude.Text
describeFeatureGroupResponse_nextToken = Lens.lens (\DescribeFeatureGroupResponse' {nextToken} -> nextToken) (\s@DescribeFeatureGroupResponse' {} a -> s {nextToken = a} :: DescribeFeatureGroupResponse)

instance Prelude.NFData DescribeFeatureGroupResponse
