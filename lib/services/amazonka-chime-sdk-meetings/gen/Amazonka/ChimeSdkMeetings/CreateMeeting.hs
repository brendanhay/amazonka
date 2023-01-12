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
-- Module      : Amazonka.ChimeSdkMeetings.CreateMeeting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Chime SDK meeting in the specified media Region
-- with no initial attendees. For more information about specifying media
-- Regions, see
-- <https://docs.aws.amazon.com/chime/latest/dg/chime-sdk-meetings-regions.html Amazon Chime SDK Media Regions>
-- in the /Amazon Chime Developer Guide/. For more information about the
-- Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.ChimeSdkMeetings.CreateMeeting
  ( -- * Creating a Request
    CreateMeeting (..),
    newCreateMeeting,

    -- * Request Lenses
    createMeeting_meetingFeatures,
    createMeeting_meetingHostId,
    createMeeting_notificationsConfiguration,
    createMeeting_primaryMeetingId,
    createMeeting_tags,
    createMeeting_tenantIds,
    createMeeting_clientRequestToken,
    createMeeting_mediaRegion,
    createMeeting_externalMeetingId,

    -- * Destructuring the Response
    CreateMeetingResponse (..),
    newCreateMeetingResponse,

    -- * Response Lenses
    createMeetingResponse_meeting,
    createMeetingResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMeeting' smart constructor.
data CreateMeeting = CreateMeeting'
  { -- | Lists the audio and video features enabled for a meeting, such as echo
    -- reduction.
    meetingFeatures :: Prelude.Maybe MeetingFeaturesConfiguration,
    -- | Reserved.
    meetingHostId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The configuration for resource targets to receive notifications when
    -- meeting and attendee events occur.
    notificationsConfiguration :: Prelude.Maybe NotificationsConfiguration,
    -- | When specified, replicates the media from the primary meeting to the new
    -- meeting.
    primaryMeetingId :: Prelude.Maybe Prelude.Text,
    -- | Applies one or more tags to an Amazon Chime SDK meeting. Note the
    -- following:
    --
    -- -   Not all resources have tags. For a list of services with resources
    --     that support tagging using this operation, see
    --     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
    --     If the resource doesn\'t yet support this operation, the resource\'s
    --     service might support tagging using its own API operations. For more
    --     information, refer to the documentation for that service.
    --
    -- -   Each resource can have up to 50 tags. For other limits, see
    --     <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions>
    --     in the /AWS General Reference/.
    --
    -- -   You can only tag resources that are located in the specified AWS
    --     Region for the AWS account.
    --
    -- -   To add tags to a resource, you need the necessary permissions for
    --     the service that the resource belongs to as well as permissions for
    --     adding tags. For more information, see the documentation for each
    --     service.
    --
    -- Do not store personally identifiable information (PII) or other
    -- confidential or sensitive information in tags. We use tags to provide
    -- you with billing and administration services. Tags are not intended to
    -- be used for private or sensitive data.
    --
    -- __Minimum permissions__
    --
    -- In addition to the @tag:TagResources @permission required by this
    -- operation, you must also have the tagging permission defined by the
    -- service that created the resource. For example, to tag a
    -- @ChimeSDKMeetings@ instance using the @TagResources@ operation, you must
    -- have both of the following permissions:
    --
    -- @tag:TagResources@
    --
    -- @ChimeSDKMeetings:CreateTags@
    --
    -- Some services might have specific requirements for tagging some
    -- resources. For example, to tag an Amazon S3 bucket, you must also have
    -- the @s3:GetBucketTagging@ permission. If the expected minimum
    -- permissions don\'t work, check the documentation for that service\'s
    -- tagging APIs for more information.
    tags :: Prelude.Maybe [Tag],
    -- | A consistent and opaque identifier, created and maintained by the
    -- builder to represent a segment of their users.
    tenantIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The unique identifier for the client request. Use a different token for
    -- different meetings.
    clientRequestToken :: Data.Sensitive Prelude.Text,
    -- | The Region in which to create the meeting.
    --
    -- Available values: @af-south-1@, @ap-northeast-1@, @ap-northeast-2@,
    -- @ap-south-1@, @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@,
    -- @eu-central-1@, @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@,
    -- @eu-west-3@, @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@,
    -- @us-west-2@.
    --
    -- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
    -- @us-gov-west-1@.
    mediaRegion :: Prelude.Text,
    -- | The external meeting ID.
    externalMeetingId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingFeatures', 'createMeeting_meetingFeatures' - Lists the audio and video features enabled for a meeting, such as echo
-- reduction.
--
-- 'meetingHostId', 'createMeeting_meetingHostId' - Reserved.
--
-- 'notificationsConfiguration', 'createMeeting_notificationsConfiguration' - The configuration for resource targets to receive notifications when
-- meeting and attendee events occur.
--
-- 'primaryMeetingId', 'createMeeting_primaryMeetingId' - When specified, replicates the media from the primary meeting to the new
-- meeting.
--
-- 'tags', 'createMeeting_tags' - Applies one or more tags to an Amazon Chime SDK meeting. Note the
-- following:
--
-- -   Not all resources have tags. For a list of services with resources
--     that support tagging using this operation, see
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--     If the resource doesn\'t yet support this operation, the resource\'s
--     service might support tagging using its own API operations. For more
--     information, refer to the documentation for that service.
--
-- -   Each resource can have up to 50 tags. For other limits, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions>
--     in the /AWS General Reference/.
--
-- -   You can only tag resources that are located in the specified AWS
--     Region for the AWS account.
--
-- -   To add tags to a resource, you need the necessary permissions for
--     the service that the resource belongs to as well as permissions for
--     adding tags. For more information, see the documentation for each
--     service.
--
-- Do not store personally identifiable information (PII) or other
-- confidential or sensitive information in tags. We use tags to provide
-- you with billing and administration services. Tags are not intended to
-- be used for private or sensitive data.
--
-- __Minimum permissions__
--
-- In addition to the @tag:TagResources @permission required by this
-- operation, you must also have the tagging permission defined by the
-- service that created the resource. For example, to tag a
-- @ChimeSDKMeetings@ instance using the @TagResources@ operation, you must
-- have both of the following permissions:
--
-- @tag:TagResources@
--
-- @ChimeSDKMeetings:CreateTags@
--
-- Some services might have specific requirements for tagging some
-- resources. For example, to tag an Amazon S3 bucket, you must also have
-- the @s3:GetBucketTagging@ permission. If the expected minimum
-- permissions don\'t work, check the documentation for that service\'s
-- tagging APIs for more information.
--
-- 'tenantIds', 'createMeeting_tenantIds' - A consistent and opaque identifier, created and maintained by the
-- builder to represent a segment of their users.
--
-- 'clientRequestToken', 'createMeeting_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different meetings.
--
-- 'mediaRegion', 'createMeeting_mediaRegion' - The Region in which to create the meeting.
--
-- Available values: @af-south-1@, @ap-northeast-1@, @ap-northeast-2@,
-- @ap-south-1@, @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@,
-- @eu-central-1@, @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@,
-- @eu-west-3@, @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@,
-- @us-west-2@.
--
-- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
-- @us-gov-west-1@.
--
-- 'externalMeetingId', 'createMeeting_externalMeetingId' - The external meeting ID.
newCreateMeeting ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'mediaRegion'
  Prelude.Text ->
  -- | 'externalMeetingId'
  Prelude.Text ->
  CreateMeeting
newCreateMeeting
  pClientRequestToken_
  pMediaRegion_
  pExternalMeetingId_ =
    CreateMeeting'
      { meetingFeatures = Prelude.Nothing,
        meetingHostId = Prelude.Nothing,
        notificationsConfiguration = Prelude.Nothing,
        primaryMeetingId = Prelude.Nothing,
        tags = Prelude.Nothing,
        tenantIds = Prelude.Nothing,
        clientRequestToken =
          Data._Sensitive Lens.# pClientRequestToken_,
        mediaRegion = pMediaRegion_,
        externalMeetingId =
          Data._Sensitive Lens.# pExternalMeetingId_
      }

-- | Lists the audio and video features enabled for a meeting, such as echo
-- reduction.
createMeeting_meetingFeatures :: Lens.Lens' CreateMeeting (Prelude.Maybe MeetingFeaturesConfiguration)
createMeeting_meetingFeatures = Lens.lens (\CreateMeeting' {meetingFeatures} -> meetingFeatures) (\s@CreateMeeting' {} a -> s {meetingFeatures = a} :: CreateMeeting)

-- | Reserved.
createMeeting_meetingHostId :: Lens.Lens' CreateMeeting (Prelude.Maybe Prelude.Text)
createMeeting_meetingHostId = Lens.lens (\CreateMeeting' {meetingHostId} -> meetingHostId) (\s@CreateMeeting' {} a -> s {meetingHostId = a} :: CreateMeeting) Prelude.. Lens.mapping Data._Sensitive

-- | The configuration for resource targets to receive notifications when
-- meeting and attendee events occur.
createMeeting_notificationsConfiguration :: Lens.Lens' CreateMeeting (Prelude.Maybe NotificationsConfiguration)
createMeeting_notificationsConfiguration = Lens.lens (\CreateMeeting' {notificationsConfiguration} -> notificationsConfiguration) (\s@CreateMeeting' {} a -> s {notificationsConfiguration = a} :: CreateMeeting)

-- | When specified, replicates the media from the primary meeting to the new
-- meeting.
createMeeting_primaryMeetingId :: Lens.Lens' CreateMeeting (Prelude.Maybe Prelude.Text)
createMeeting_primaryMeetingId = Lens.lens (\CreateMeeting' {primaryMeetingId} -> primaryMeetingId) (\s@CreateMeeting' {} a -> s {primaryMeetingId = a} :: CreateMeeting)

-- | Applies one or more tags to an Amazon Chime SDK meeting. Note the
-- following:
--
-- -   Not all resources have tags. For a list of services with resources
--     that support tagging using this operation, see
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--     If the resource doesn\'t yet support this operation, the resource\'s
--     service might support tagging using its own API operations. For more
--     information, refer to the documentation for that service.
--
-- -   Each resource can have up to 50 tags. For other limits, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions>
--     in the /AWS General Reference/.
--
-- -   You can only tag resources that are located in the specified AWS
--     Region for the AWS account.
--
-- -   To add tags to a resource, you need the necessary permissions for
--     the service that the resource belongs to as well as permissions for
--     adding tags. For more information, see the documentation for each
--     service.
--
-- Do not store personally identifiable information (PII) or other
-- confidential or sensitive information in tags. We use tags to provide
-- you with billing and administration services. Tags are not intended to
-- be used for private or sensitive data.
--
-- __Minimum permissions__
--
-- In addition to the @tag:TagResources @permission required by this
-- operation, you must also have the tagging permission defined by the
-- service that created the resource. For example, to tag a
-- @ChimeSDKMeetings@ instance using the @TagResources@ operation, you must
-- have both of the following permissions:
--
-- @tag:TagResources@
--
-- @ChimeSDKMeetings:CreateTags@
--
-- Some services might have specific requirements for tagging some
-- resources. For example, to tag an Amazon S3 bucket, you must also have
-- the @s3:GetBucketTagging@ permission. If the expected minimum
-- permissions don\'t work, check the documentation for that service\'s
-- tagging APIs for more information.
createMeeting_tags :: Lens.Lens' CreateMeeting (Prelude.Maybe [Tag])
createMeeting_tags = Lens.lens (\CreateMeeting' {tags} -> tags) (\s@CreateMeeting' {} a -> s {tags = a} :: CreateMeeting) Prelude.. Lens.mapping Lens.coerced

-- | A consistent and opaque identifier, created and maintained by the
-- builder to represent a segment of their users.
createMeeting_tenantIds :: Lens.Lens' CreateMeeting (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createMeeting_tenantIds = Lens.lens (\CreateMeeting' {tenantIds} -> tenantIds) (\s@CreateMeeting' {} a -> s {tenantIds = a} :: CreateMeeting) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the client request. Use a different token for
-- different meetings.
createMeeting_clientRequestToken :: Lens.Lens' CreateMeeting Prelude.Text
createMeeting_clientRequestToken = Lens.lens (\CreateMeeting' {clientRequestToken} -> clientRequestToken) (\s@CreateMeeting' {} a -> s {clientRequestToken = a} :: CreateMeeting) Prelude.. Data._Sensitive

-- | The Region in which to create the meeting.
--
-- Available values: @af-south-1@, @ap-northeast-1@, @ap-northeast-2@,
-- @ap-south-1@, @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@,
-- @eu-central-1@, @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@,
-- @eu-west-3@, @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@,
-- @us-west-2@.
--
-- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
-- @us-gov-west-1@.
createMeeting_mediaRegion :: Lens.Lens' CreateMeeting Prelude.Text
createMeeting_mediaRegion = Lens.lens (\CreateMeeting' {mediaRegion} -> mediaRegion) (\s@CreateMeeting' {} a -> s {mediaRegion = a} :: CreateMeeting)

-- | The external meeting ID.
createMeeting_externalMeetingId :: Lens.Lens' CreateMeeting Prelude.Text
createMeeting_externalMeetingId = Lens.lens (\CreateMeeting' {externalMeetingId} -> externalMeetingId) (\s@CreateMeeting' {} a -> s {externalMeetingId = a} :: CreateMeeting) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateMeeting where
  type
    AWSResponse CreateMeeting =
      CreateMeetingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMeetingResponse'
            Prelude.<$> (x Data..?> "Meeting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMeeting where
  hashWithSalt _salt CreateMeeting' {..} =
    _salt `Prelude.hashWithSalt` meetingFeatures
      `Prelude.hashWithSalt` meetingHostId
      `Prelude.hashWithSalt` notificationsConfiguration
      `Prelude.hashWithSalt` primaryMeetingId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tenantIds
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` mediaRegion
      `Prelude.hashWithSalt` externalMeetingId

instance Prelude.NFData CreateMeeting where
  rnf CreateMeeting' {..} =
    Prelude.rnf meetingFeatures
      `Prelude.seq` Prelude.rnf meetingHostId
      `Prelude.seq` Prelude.rnf notificationsConfiguration
      `Prelude.seq` Prelude.rnf primaryMeetingId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tenantIds
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf mediaRegion
      `Prelude.seq` Prelude.rnf externalMeetingId

instance Data.ToHeaders CreateMeeting where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMeeting where
  toJSON CreateMeeting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MeetingFeatures" Data..=)
              Prelude.<$> meetingFeatures,
            ("MeetingHostId" Data..=) Prelude.<$> meetingHostId,
            ("NotificationsConfiguration" Data..=)
              Prelude.<$> notificationsConfiguration,
            ("PrimaryMeetingId" Data..=)
              Prelude.<$> primaryMeetingId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TenantIds" Data..=) Prelude.<$> tenantIds,
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just ("MediaRegion" Data..= mediaRegion),
            Prelude.Just
              ("ExternalMeetingId" Data..= externalMeetingId)
          ]
      )

instance Data.ToPath CreateMeeting where
  toPath = Prelude.const "/meetings"

instance Data.ToQuery CreateMeeting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMeetingResponse' smart constructor.
data CreateMeetingResponse = CreateMeetingResponse'
  { -- | The meeting information, including the meeting ID and @MediaPlacement@.
    meeting :: Prelude.Maybe Meeting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meeting', 'createMeetingResponse_meeting' - The meeting information, including the meeting ID and @MediaPlacement@.
--
-- 'httpStatus', 'createMeetingResponse_httpStatus' - The response's http status code.
newCreateMeetingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMeetingResponse
newCreateMeetingResponse pHttpStatus_ =
  CreateMeetingResponse'
    { meeting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The meeting information, including the meeting ID and @MediaPlacement@.
createMeetingResponse_meeting :: Lens.Lens' CreateMeetingResponse (Prelude.Maybe Meeting)
createMeetingResponse_meeting = Lens.lens (\CreateMeetingResponse' {meeting} -> meeting) (\s@CreateMeetingResponse' {} a -> s {meeting = a} :: CreateMeetingResponse)

-- | The response's http status code.
createMeetingResponse_httpStatus :: Lens.Lens' CreateMeetingResponse Prelude.Int
createMeetingResponse_httpStatus = Lens.lens (\CreateMeetingResponse' {httpStatus} -> httpStatus) (\s@CreateMeetingResponse' {} a -> s {httpStatus = a} :: CreateMeetingResponse)

instance Prelude.NFData CreateMeetingResponse where
  rnf CreateMeetingResponse' {..} =
    Prelude.rnf meeting
      `Prelude.seq` Prelude.rnf httpStatus
