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
-- Module      : Amazonka.ChimeSdkMeetings.Types.Meeting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.Meeting where

import Amazonka.ChimeSdkMeetings.Types.MediaPlacement
import Amazonka.ChimeSdkMeetings.Types.MeetingFeaturesConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A meeting created using the Amazon Chime SDK.
--
-- /See:/ 'newMeeting' smart constructor.
data Meeting = Meeting'
  { -- | The external meeting ID.
    --
    -- Pattern: @[-_&\@+=,(){}\\[\\]\\\/«».:|\'\"#a-zA-Z0-9À-ÿ\\s]*@
    --
    -- Values that begin with @aws:@ are reserved. You can\'t configure a value
    -- that uses this prefix. Case insensitive.
    externalMeetingId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The media placement for the meeting.
    mediaPlacement :: Prelude.Maybe MediaPlacement,
    -- | The Region in which you create the meeting. Available values:
    -- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
    -- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
    -- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
    -- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
    --
    -- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
    -- @us-gov-west-1@.
    mediaRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the meeting.
    meetingArn :: Prelude.Maybe Prelude.Text,
    -- | The features available to a meeting, such as echo reduction.
    meetingFeatures :: Prelude.Maybe MeetingFeaturesConfiguration,
    -- | Reserved.
    meetingHostId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Maybe Prelude.Text,
    -- | When specified, replicates the media from the primary meeting to this
    -- meeting.
    primaryMeetingId :: Prelude.Maybe Prelude.Text,
    -- | Array of strings.
    tenantIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Meeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalMeetingId', 'meeting_externalMeetingId' - The external meeting ID.
--
-- Pattern: @[-_&\@+=,(){}\\[\\]\\\/«».:|\'\"#a-zA-Z0-9À-ÿ\\s]*@
--
-- Values that begin with @aws:@ are reserved. You can\'t configure a value
-- that uses this prefix. Case insensitive.
--
-- 'mediaPlacement', 'meeting_mediaPlacement' - The media placement for the meeting.
--
-- 'mediaRegion', 'meeting_mediaRegion' - The Region in which you create the meeting. Available values:
-- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
-- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
-- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
-- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
--
-- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
-- @us-gov-west-1@.
--
-- 'meetingArn', 'meeting_meetingArn' - The ARN of the meeting.
--
-- 'meetingFeatures', 'meeting_meetingFeatures' - The features available to a meeting, such as echo reduction.
--
-- 'meetingHostId', 'meeting_meetingHostId' - Reserved.
--
-- 'meetingId', 'meeting_meetingId' - The Amazon Chime SDK meeting ID.
--
-- 'primaryMeetingId', 'meeting_primaryMeetingId' - When specified, replicates the media from the primary meeting to this
-- meeting.
--
-- 'tenantIds', 'meeting_tenantIds' - Array of strings.
newMeeting ::
  Meeting
newMeeting =
  Meeting'
    { externalMeetingId = Prelude.Nothing,
      mediaPlacement = Prelude.Nothing,
      mediaRegion = Prelude.Nothing,
      meetingArn = Prelude.Nothing,
      meetingFeatures = Prelude.Nothing,
      meetingHostId = Prelude.Nothing,
      meetingId = Prelude.Nothing,
      primaryMeetingId = Prelude.Nothing,
      tenantIds = Prelude.Nothing
    }

-- | The external meeting ID.
--
-- Pattern: @[-_&\@+=,(){}\\[\\]\\\/«».:|\'\"#a-zA-Z0-9À-ÿ\\s]*@
--
-- Values that begin with @aws:@ are reserved. You can\'t configure a value
-- that uses this prefix. Case insensitive.
meeting_externalMeetingId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_externalMeetingId = Lens.lens (\Meeting' {externalMeetingId} -> externalMeetingId) (\s@Meeting' {} a -> s {externalMeetingId = a} :: Meeting) Prelude.. Lens.mapping Data._Sensitive

-- | The media placement for the meeting.
meeting_mediaPlacement :: Lens.Lens' Meeting (Prelude.Maybe MediaPlacement)
meeting_mediaPlacement = Lens.lens (\Meeting' {mediaPlacement} -> mediaPlacement) (\s@Meeting' {} a -> s {mediaPlacement = a} :: Meeting)

-- | The Region in which you create the meeting. Available values:
-- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
-- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
-- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
-- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
--
-- Available values in AWS GovCloud (US) Regions: @us-gov-east-1@,
-- @us-gov-west-1@.
meeting_mediaRegion :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_mediaRegion = Lens.lens (\Meeting' {mediaRegion} -> mediaRegion) (\s@Meeting' {} a -> s {mediaRegion = a} :: Meeting)

-- | The ARN of the meeting.
meeting_meetingArn :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_meetingArn = Lens.lens (\Meeting' {meetingArn} -> meetingArn) (\s@Meeting' {} a -> s {meetingArn = a} :: Meeting)

-- | The features available to a meeting, such as echo reduction.
meeting_meetingFeatures :: Lens.Lens' Meeting (Prelude.Maybe MeetingFeaturesConfiguration)
meeting_meetingFeatures = Lens.lens (\Meeting' {meetingFeatures} -> meetingFeatures) (\s@Meeting' {} a -> s {meetingFeatures = a} :: Meeting)

-- | Reserved.
meeting_meetingHostId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_meetingHostId = Lens.lens (\Meeting' {meetingHostId} -> meetingHostId) (\s@Meeting' {} a -> s {meetingHostId = a} :: Meeting) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Chime SDK meeting ID.
meeting_meetingId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_meetingId = Lens.lens (\Meeting' {meetingId} -> meetingId) (\s@Meeting' {} a -> s {meetingId = a} :: Meeting)

-- | When specified, replicates the media from the primary meeting to this
-- meeting.
meeting_primaryMeetingId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_primaryMeetingId = Lens.lens (\Meeting' {primaryMeetingId} -> primaryMeetingId) (\s@Meeting' {} a -> s {primaryMeetingId = a} :: Meeting)

-- | Array of strings.
meeting_tenantIds :: Lens.Lens' Meeting (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
meeting_tenantIds = Lens.lens (\Meeting' {tenantIds} -> tenantIds) (\s@Meeting' {} a -> s {tenantIds = a} :: Meeting) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Meeting where
  parseJSON =
    Data.withObject
      "Meeting"
      ( \x ->
          Meeting'
            Prelude.<$> (x Data..:? "ExternalMeetingId")
            Prelude.<*> (x Data..:? "MediaPlacement")
            Prelude.<*> (x Data..:? "MediaRegion")
            Prelude.<*> (x Data..:? "MeetingArn")
            Prelude.<*> (x Data..:? "MeetingFeatures")
            Prelude.<*> (x Data..:? "MeetingHostId")
            Prelude.<*> (x Data..:? "MeetingId")
            Prelude.<*> (x Data..:? "PrimaryMeetingId")
            Prelude.<*> (x Data..:? "TenantIds")
      )

instance Prelude.Hashable Meeting where
  hashWithSalt _salt Meeting' {..} =
    _salt
      `Prelude.hashWithSalt` externalMeetingId
      `Prelude.hashWithSalt` mediaPlacement
      `Prelude.hashWithSalt` mediaRegion
      `Prelude.hashWithSalt` meetingArn
      `Prelude.hashWithSalt` meetingFeatures
      `Prelude.hashWithSalt` meetingHostId
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` primaryMeetingId
      `Prelude.hashWithSalt` tenantIds

instance Prelude.NFData Meeting where
  rnf Meeting' {..} =
    Prelude.rnf externalMeetingId
      `Prelude.seq` Prelude.rnf mediaPlacement
      `Prelude.seq` Prelude.rnf mediaRegion
      `Prelude.seq` Prelude.rnf meetingArn
      `Prelude.seq` Prelude.rnf meetingFeatures
      `Prelude.seq` Prelude.rnf meetingHostId
      `Prelude.seq` Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf primaryMeetingId
      `Prelude.seq` Prelude.rnf tenantIds
