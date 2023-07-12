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
-- Module      : Amazonka.GroundStation.DescribeContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing contact.
module Amazonka.GroundStation.DescribeContact
  ( -- * Creating a Request
    DescribeContact (..),
    newDescribeContact,

    -- * Request Lenses
    describeContact_contactId,

    -- * Destructuring the Response
    DescribeContactResponse (..),
    newDescribeContactResponse,

    -- * Response Lenses
    describeContactResponse_contactId,
    describeContactResponse_contactStatus,
    describeContactResponse_dataflowList,
    describeContactResponse_endTime,
    describeContactResponse_errorMessage,
    describeContactResponse_groundStation,
    describeContactResponse_maximumElevation,
    describeContactResponse_missionProfileArn,
    describeContactResponse_postPassEndTime,
    describeContactResponse_prePassStartTime,
    describeContactResponse_region,
    describeContactResponse_satelliteArn,
    describeContactResponse_startTime,
    describeContactResponse_tags,
    describeContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeContact' smart constructor.
data DescribeContact = DescribeContact'
  { -- | UUID of a contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'describeContact_contactId' - UUID of a contact.
newDescribeContact ::
  -- | 'contactId'
  Prelude.Text ->
  DescribeContact
newDescribeContact pContactId_ =
  DescribeContact' {contactId = pContactId_}

-- | UUID of a contact.
describeContact_contactId :: Lens.Lens' DescribeContact Prelude.Text
describeContact_contactId = Lens.lens (\DescribeContact' {contactId} -> contactId) (\s@DescribeContact' {} a -> s {contactId = a} :: DescribeContact)

instance Core.AWSRequest DescribeContact where
  type
    AWSResponse DescribeContact =
      DescribeContactResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactResponse'
            Prelude.<$> (x Data..?> "contactId")
            Prelude.<*> (x Data..?> "contactStatus")
            Prelude.<*> (x Data..?> "dataflowList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "errorMessage")
            Prelude.<*> (x Data..?> "groundStation")
            Prelude.<*> (x Data..?> "maximumElevation")
            Prelude.<*> (x Data..?> "missionProfileArn")
            Prelude.<*> (x Data..?> "postPassEndTime")
            Prelude.<*> (x Data..?> "prePassStartTime")
            Prelude.<*> (x Data..?> "region")
            Prelude.<*> (x Data..?> "satelliteArn")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContact where
  hashWithSalt _salt DescribeContact' {..} =
    _salt `Prelude.hashWithSalt` contactId

instance Prelude.NFData DescribeContact where
  rnf DescribeContact' {..} = Prelude.rnf contactId

instance Data.ToHeaders DescribeContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeContact where
  toPath DescribeContact' {..} =
    Prelude.mconcat ["/contact/", Data.toBS contactId]

instance Data.ToQuery DescribeContact where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeContactResponse' smart constructor.
data DescribeContactResponse = DescribeContactResponse'
  { -- | UUID of a contact.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | Status of a contact.
    contactStatus :: Prelude.Maybe ContactStatus,
    -- | List describing source and destination details for each dataflow edge.
    dataflowList :: Prelude.Maybe [DataflowDetail],
    -- | End time of a contact in UTC.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Error message for a contact.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Ground station for a contact.
    groundStation :: Prelude.Maybe Prelude.Text,
    -- | Maximum elevation angle of a contact.
    maximumElevation :: Prelude.Maybe Elevation,
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    postPassEndTime :: Prelude.Maybe Data.POSIX,
    -- | Amount of time prior to contact start you’d like to receive a CloudWatch
    -- event indicating an upcoming pass.
    prePassStartTime :: Prelude.Maybe Data.POSIX,
    -- | Region of a contact.
    region :: Prelude.Maybe Prelude.Text,
    -- | ARN of a satellite.
    satelliteArn :: Prelude.Maybe Prelude.Text,
    -- | Start time of a contact in UTC.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Tags assigned to a contact.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'describeContactResponse_contactId' - UUID of a contact.
--
-- 'contactStatus', 'describeContactResponse_contactStatus' - Status of a contact.
--
-- 'dataflowList', 'describeContactResponse_dataflowList' - List describing source and destination details for each dataflow edge.
--
-- 'endTime', 'describeContactResponse_endTime' - End time of a contact in UTC.
--
-- 'errorMessage', 'describeContactResponse_errorMessage' - Error message for a contact.
--
-- 'groundStation', 'describeContactResponse_groundStation' - Ground station for a contact.
--
-- 'maximumElevation', 'describeContactResponse_maximumElevation' - Maximum elevation angle of a contact.
--
-- 'missionProfileArn', 'describeContactResponse_missionProfileArn' - ARN of a mission profile.
--
-- 'postPassEndTime', 'describeContactResponse_postPassEndTime' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'prePassStartTime', 'describeContactResponse_prePassStartTime' - Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
--
-- 'region', 'describeContactResponse_region' - Region of a contact.
--
-- 'satelliteArn', 'describeContactResponse_satelliteArn' - ARN of a satellite.
--
-- 'startTime', 'describeContactResponse_startTime' - Start time of a contact in UTC.
--
-- 'tags', 'describeContactResponse_tags' - Tags assigned to a contact.
--
-- 'httpStatus', 'describeContactResponse_httpStatus' - The response's http status code.
newDescribeContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContactResponse
newDescribeContactResponse pHttpStatus_ =
  DescribeContactResponse'
    { contactId =
        Prelude.Nothing,
      contactStatus = Prelude.Nothing,
      dataflowList = Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      groundStation = Prelude.Nothing,
      maximumElevation = Prelude.Nothing,
      missionProfileArn = Prelude.Nothing,
      postPassEndTime = Prelude.Nothing,
      prePassStartTime = Prelude.Nothing,
      region = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | UUID of a contact.
describeContactResponse_contactId :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_contactId = Lens.lens (\DescribeContactResponse' {contactId} -> contactId) (\s@DescribeContactResponse' {} a -> s {contactId = a} :: DescribeContactResponse)

-- | Status of a contact.
describeContactResponse_contactStatus :: Lens.Lens' DescribeContactResponse (Prelude.Maybe ContactStatus)
describeContactResponse_contactStatus = Lens.lens (\DescribeContactResponse' {contactStatus} -> contactStatus) (\s@DescribeContactResponse' {} a -> s {contactStatus = a} :: DescribeContactResponse)

-- | List describing source and destination details for each dataflow edge.
describeContactResponse_dataflowList :: Lens.Lens' DescribeContactResponse (Prelude.Maybe [DataflowDetail])
describeContactResponse_dataflowList = Lens.lens (\DescribeContactResponse' {dataflowList} -> dataflowList) (\s@DescribeContactResponse' {} a -> s {dataflowList = a} :: DescribeContactResponse) Prelude.. Lens.mapping Lens.coerced

-- | End time of a contact in UTC.
describeContactResponse_endTime :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.UTCTime)
describeContactResponse_endTime = Lens.lens (\DescribeContactResponse' {endTime} -> endTime) (\s@DescribeContactResponse' {} a -> s {endTime = a} :: DescribeContactResponse) Prelude.. Lens.mapping Data._Time

-- | Error message for a contact.
describeContactResponse_errorMessage :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_errorMessage = Lens.lens (\DescribeContactResponse' {errorMessage} -> errorMessage) (\s@DescribeContactResponse' {} a -> s {errorMessage = a} :: DescribeContactResponse)

-- | Ground station for a contact.
describeContactResponse_groundStation :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_groundStation = Lens.lens (\DescribeContactResponse' {groundStation} -> groundStation) (\s@DescribeContactResponse' {} a -> s {groundStation = a} :: DescribeContactResponse)

-- | Maximum elevation angle of a contact.
describeContactResponse_maximumElevation :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Elevation)
describeContactResponse_maximumElevation = Lens.lens (\DescribeContactResponse' {maximumElevation} -> maximumElevation) (\s@DescribeContactResponse' {} a -> s {maximumElevation = a} :: DescribeContactResponse)

-- | ARN of a mission profile.
describeContactResponse_missionProfileArn :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_missionProfileArn = Lens.lens (\DescribeContactResponse' {missionProfileArn} -> missionProfileArn) (\s@DescribeContactResponse' {} a -> s {missionProfileArn = a} :: DescribeContactResponse)

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
describeContactResponse_postPassEndTime :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.UTCTime)
describeContactResponse_postPassEndTime = Lens.lens (\DescribeContactResponse' {postPassEndTime} -> postPassEndTime) (\s@DescribeContactResponse' {} a -> s {postPassEndTime = a} :: DescribeContactResponse) Prelude.. Lens.mapping Data._Time

-- | Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
describeContactResponse_prePassStartTime :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.UTCTime)
describeContactResponse_prePassStartTime = Lens.lens (\DescribeContactResponse' {prePassStartTime} -> prePassStartTime) (\s@DescribeContactResponse' {} a -> s {prePassStartTime = a} :: DescribeContactResponse) Prelude.. Lens.mapping Data._Time

-- | Region of a contact.
describeContactResponse_region :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_region = Lens.lens (\DescribeContactResponse' {region} -> region) (\s@DescribeContactResponse' {} a -> s {region = a} :: DescribeContactResponse)

-- | ARN of a satellite.
describeContactResponse_satelliteArn :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.Text)
describeContactResponse_satelliteArn = Lens.lens (\DescribeContactResponse' {satelliteArn} -> satelliteArn) (\s@DescribeContactResponse' {} a -> s {satelliteArn = a} :: DescribeContactResponse)

-- | Start time of a contact in UTC.
describeContactResponse_startTime :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Prelude.UTCTime)
describeContactResponse_startTime = Lens.lens (\DescribeContactResponse' {startTime} -> startTime) (\s@DescribeContactResponse' {} a -> s {startTime = a} :: DescribeContactResponse) Prelude.. Lens.mapping Data._Time

-- | Tags assigned to a contact.
describeContactResponse_tags :: Lens.Lens' DescribeContactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeContactResponse_tags = Lens.lens (\DescribeContactResponse' {tags} -> tags) (\s@DescribeContactResponse' {} a -> s {tags = a} :: DescribeContactResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeContactResponse_httpStatus :: Lens.Lens' DescribeContactResponse Prelude.Int
describeContactResponse_httpStatus = Lens.lens (\DescribeContactResponse' {httpStatus} -> httpStatus) (\s@DescribeContactResponse' {} a -> s {httpStatus = a} :: DescribeContactResponse)

instance Prelude.NFData DescribeContactResponse where
  rnf DescribeContactResponse' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf contactStatus
      `Prelude.seq` Prelude.rnf dataflowList
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf groundStation
      `Prelude.seq` Prelude.rnf maximumElevation
      `Prelude.seq` Prelude.rnf missionProfileArn
      `Prelude.seq` Prelude.rnf postPassEndTime
      `Prelude.seq` Prelude.rnf prePassStartTime
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
