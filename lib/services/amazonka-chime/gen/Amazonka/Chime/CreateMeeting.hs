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
-- Module      : Amazonka.Chime.CreateMeeting
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
-- in the /Amazon Chime Developer Guide/ . For more information about the
-- Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/ .
module Amazonka.Chime.CreateMeeting
  ( -- * Creating a Request
    CreateMeeting (..),
    newCreateMeeting,

    -- * Request Lenses
    createMeeting_externalMeetingId,
    createMeeting_mediaRegion,
    createMeeting_meetingHostId,
    createMeeting_notificationsConfiguration,
    createMeeting_tags,
    createMeeting_clientRequestToken,

    -- * Destructuring the Response
    CreateMeetingResponse (..),
    newCreateMeetingResponse,

    -- * Response Lenses
    createMeetingResponse_meeting,
    createMeetingResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMeeting' smart constructor.
data CreateMeeting = CreateMeeting'
  { -- | The external meeting ID.
    externalMeetingId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Region in which to create the meeting. Default: @us-east-1@.
    --
    -- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
    -- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
    -- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
    -- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
    -- @us-west-2@ .
    mediaRegion :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    meetingHostId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The configuration for resource targets to receive notifications when
    -- meeting and attendee events occur.
    notificationsConfiguration :: Prelude.Maybe MeetingNotificationConfiguration,
    -- | The tag key-value pairs.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The unique identifier for the client request. Use a different token for
    -- different meetings.
    clientRequestToken :: Data.Sensitive Prelude.Text
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
-- 'externalMeetingId', 'createMeeting_externalMeetingId' - The external meeting ID.
--
-- 'mediaRegion', 'createMeeting_mediaRegion' - The Region in which to create the meeting. Default: @us-east-1@.
--
-- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
-- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
-- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
-- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
-- @us-west-2@ .
--
-- 'meetingHostId', 'createMeeting_meetingHostId' - Reserved.
--
-- 'notificationsConfiguration', 'createMeeting_notificationsConfiguration' - The configuration for resource targets to receive notifications when
-- meeting and attendee events occur.
--
-- 'tags', 'createMeeting_tags' - The tag key-value pairs.
--
-- 'clientRequestToken', 'createMeeting_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different meetings.
newCreateMeeting ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateMeeting
newCreateMeeting pClientRequestToken_ =
  CreateMeeting'
    { externalMeetingId = Prelude.Nothing,
      mediaRegion = Prelude.Nothing,
      meetingHostId = Prelude.Nothing,
      notificationsConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      clientRequestToken =
        Data._Sensitive Lens.# pClientRequestToken_
    }

-- | The external meeting ID.
createMeeting_externalMeetingId :: Lens.Lens' CreateMeeting (Prelude.Maybe Prelude.Text)
createMeeting_externalMeetingId = Lens.lens (\CreateMeeting' {externalMeetingId} -> externalMeetingId) (\s@CreateMeeting' {} a -> s {externalMeetingId = a} :: CreateMeeting) Prelude.. Lens.mapping Data._Sensitive

-- | The Region in which to create the meeting. Default: @us-east-1@.
--
-- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
-- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
-- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
-- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
-- @us-west-2@ .
createMeeting_mediaRegion :: Lens.Lens' CreateMeeting (Prelude.Maybe Prelude.Text)
createMeeting_mediaRegion = Lens.lens (\CreateMeeting' {mediaRegion} -> mediaRegion) (\s@CreateMeeting' {} a -> s {mediaRegion = a} :: CreateMeeting)

-- | Reserved.
createMeeting_meetingHostId :: Lens.Lens' CreateMeeting (Prelude.Maybe Prelude.Text)
createMeeting_meetingHostId = Lens.lens (\CreateMeeting' {meetingHostId} -> meetingHostId) (\s@CreateMeeting' {} a -> s {meetingHostId = a} :: CreateMeeting) Prelude.. Lens.mapping Data._Sensitive

-- | The configuration for resource targets to receive notifications when
-- meeting and attendee events occur.
createMeeting_notificationsConfiguration :: Lens.Lens' CreateMeeting (Prelude.Maybe MeetingNotificationConfiguration)
createMeeting_notificationsConfiguration = Lens.lens (\CreateMeeting' {notificationsConfiguration} -> notificationsConfiguration) (\s@CreateMeeting' {} a -> s {notificationsConfiguration = a} :: CreateMeeting)

-- | The tag key-value pairs.
createMeeting_tags :: Lens.Lens' CreateMeeting (Prelude.Maybe (Prelude.NonEmpty Tag))
createMeeting_tags = Lens.lens (\CreateMeeting' {tags} -> tags) (\s@CreateMeeting' {} a -> s {tags = a} :: CreateMeeting) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the client request. Use a different token for
-- different meetings.
createMeeting_clientRequestToken :: Lens.Lens' CreateMeeting Prelude.Text
createMeeting_clientRequestToken = Lens.lens (\CreateMeeting' {clientRequestToken} -> clientRequestToken) (\s@CreateMeeting' {} a -> s {clientRequestToken = a} :: CreateMeeting) Prelude.. Data._Sensitive

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
    _salt `Prelude.hashWithSalt` externalMeetingId
      `Prelude.hashWithSalt` mediaRegion
      `Prelude.hashWithSalt` meetingHostId
      `Prelude.hashWithSalt` notificationsConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateMeeting where
  rnf CreateMeeting' {..} =
    Prelude.rnf externalMeetingId
      `Prelude.seq` Prelude.rnf mediaRegion
      `Prelude.seq` Prelude.rnf meetingHostId
      `Prelude.seq` Prelude.rnf notificationsConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateMeeting where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMeeting where
  toJSON CreateMeeting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExternalMeetingId" Data..=)
              Prelude.<$> externalMeetingId,
            ("MediaRegion" Data..=) Prelude.<$> mediaRegion,
            ("MeetingHostId" Data..=) Prelude.<$> meetingHostId,
            ("NotificationsConfiguration" Data..=)
              Prelude.<$> notificationsConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateMeeting where
  toPath = Prelude.const "/meetings"

instance Data.ToQuery CreateMeeting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMeetingResponse' smart constructor.
data CreateMeetingResponse = CreateMeetingResponse'
  { -- | The meeting information, including the meeting ID and @MediaPlacement@ .
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
-- 'meeting', 'createMeetingResponse_meeting' - The meeting information, including the meeting ID and @MediaPlacement@ .
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

-- | The meeting information, including the meeting ID and @MediaPlacement@ .
createMeetingResponse_meeting :: Lens.Lens' CreateMeetingResponse (Prelude.Maybe Meeting)
createMeetingResponse_meeting = Lens.lens (\CreateMeetingResponse' {meeting} -> meeting) (\s@CreateMeetingResponse' {} a -> s {meeting = a} :: CreateMeetingResponse)

-- | The response's http status code.
createMeetingResponse_httpStatus :: Lens.Lens' CreateMeetingResponse Prelude.Int
createMeetingResponse_httpStatus = Lens.lens (\CreateMeetingResponse' {httpStatus} -> httpStatus) (\s@CreateMeetingResponse' {} a -> s {httpStatus = a} :: CreateMeetingResponse)

instance Prelude.NFData CreateMeetingResponse where
  rnf CreateMeetingResponse' {..} =
    Prelude.rnf meeting
      `Prelude.seq` Prelude.rnf httpStatus
