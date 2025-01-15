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
-- Module      : Amazonka.LookoutEquipment.DescribeLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name of the label.
module Amazonka.LookoutEquipment.DescribeLabel
  ( -- * Creating a Request
    DescribeLabel (..),
    newDescribeLabel,

    -- * Request Lenses
    describeLabel_labelGroupName,
    describeLabel_labelId,

    -- * Destructuring the Response
    DescribeLabelResponse (..),
    newDescribeLabelResponse,

    -- * Response Lenses
    describeLabelResponse_createdAt,
    describeLabelResponse_endTime,
    describeLabelResponse_equipment,
    describeLabelResponse_faultCode,
    describeLabelResponse_labelGroupArn,
    describeLabelResponse_labelGroupName,
    describeLabelResponse_labelId,
    describeLabelResponse_notes,
    describeLabelResponse_rating,
    describeLabelResponse_startTime,
    describeLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLabel' smart constructor.
data DescribeLabel = DescribeLabel'
  { -- | Returns the name of the group containing the label.
    labelGroupName :: Prelude.Text,
    -- | Returns the ID of the label.
    labelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupName', 'describeLabel_labelGroupName' - Returns the name of the group containing the label.
--
-- 'labelId', 'describeLabel_labelId' - Returns the ID of the label.
newDescribeLabel ::
  -- | 'labelGroupName'
  Prelude.Text ->
  -- | 'labelId'
  Prelude.Text ->
  DescribeLabel
newDescribeLabel pLabelGroupName_ pLabelId_ =
  DescribeLabel'
    { labelGroupName = pLabelGroupName_,
      labelId = pLabelId_
    }

-- | Returns the name of the group containing the label.
describeLabel_labelGroupName :: Lens.Lens' DescribeLabel Prelude.Text
describeLabel_labelGroupName = Lens.lens (\DescribeLabel' {labelGroupName} -> labelGroupName) (\s@DescribeLabel' {} a -> s {labelGroupName = a} :: DescribeLabel)

-- | Returns the ID of the label.
describeLabel_labelId :: Lens.Lens' DescribeLabel Prelude.Text
describeLabel_labelId = Lens.lens (\DescribeLabel' {labelId} -> labelId) (\s@DescribeLabel' {} a -> s {labelId = a} :: DescribeLabel)

instance Core.AWSRequest DescribeLabel where
  type
    AWSResponse DescribeLabel =
      DescribeLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLabelResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "Equipment")
            Prelude.<*> (x Data..?> "FaultCode")
            Prelude.<*> (x Data..?> "LabelGroupArn")
            Prelude.<*> (x Data..?> "LabelGroupName")
            Prelude.<*> (x Data..?> "LabelId")
            Prelude.<*> (x Data..?> "Notes")
            Prelude.<*> (x Data..?> "Rating")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLabel where
  hashWithSalt _salt DescribeLabel' {..} =
    _salt
      `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` labelId

instance Prelude.NFData DescribeLabel where
  rnf DescribeLabel' {..} =
    Prelude.rnf labelGroupName `Prelude.seq`
      Prelude.rnf labelId

instance Data.ToHeaders DescribeLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DescribeLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLabel where
  toJSON DescribeLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelGroupName" Data..= labelGroupName),
            Prelude.Just ("LabelId" Data..= labelId)
          ]
      )

instance Data.ToPath DescribeLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLabelResponse' smart constructor.
data DescribeLabelResponse = DescribeLabelResponse'
  { -- | The time at which the label was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The end time of the requested label.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates that a label pertains to a particular piece of equipment.
    equipment :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of anomaly associated with the label.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    faultCode :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the requested label group.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the requested label group.
    labelGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the requested label.
    labelId :: Prelude.Maybe Prelude.Text,
    -- | Metadata providing additional information about the label.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    notes :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a labeled event represents an anomaly.
    rating :: Prelude.Maybe LabelRating,
    -- | The start time of the requested label.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'describeLabelResponse_createdAt' - The time at which the label was created.
--
-- 'endTime', 'describeLabelResponse_endTime' - The end time of the requested label.
--
-- 'equipment', 'describeLabelResponse_equipment' - Indicates that a label pertains to a particular piece of equipment.
--
-- 'faultCode', 'describeLabelResponse_faultCode' - Indicates the type of anomaly associated with the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'labelGroupArn', 'describeLabelResponse_labelGroupArn' - The ARN of the requested label group.
--
-- 'labelGroupName', 'describeLabelResponse_labelGroupName' - The name of the requested label group.
--
-- 'labelId', 'describeLabelResponse_labelId' - The ID of the requested label.
--
-- 'notes', 'describeLabelResponse_notes' - Metadata providing additional information about the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'rating', 'describeLabelResponse_rating' - Indicates whether a labeled event represents an anomaly.
--
-- 'startTime', 'describeLabelResponse_startTime' - The start time of the requested label.
--
-- 'httpStatus', 'describeLabelResponse_httpStatus' - The response's http status code.
newDescribeLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLabelResponse
newDescribeLabelResponse pHttpStatus_ =
  DescribeLabelResponse'
    { createdAt = Prelude.Nothing,
      endTime = Prelude.Nothing,
      equipment = Prelude.Nothing,
      faultCode = Prelude.Nothing,
      labelGroupArn = Prelude.Nothing,
      labelGroupName = Prelude.Nothing,
      labelId = Prelude.Nothing,
      notes = Prelude.Nothing,
      rating = Prelude.Nothing,
      startTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the label was created.
describeLabelResponse_createdAt :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.UTCTime)
describeLabelResponse_createdAt = Lens.lens (\DescribeLabelResponse' {createdAt} -> createdAt) (\s@DescribeLabelResponse' {} a -> s {createdAt = a} :: DescribeLabelResponse) Prelude.. Lens.mapping Data._Time

-- | The end time of the requested label.
describeLabelResponse_endTime :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.UTCTime)
describeLabelResponse_endTime = Lens.lens (\DescribeLabelResponse' {endTime} -> endTime) (\s@DescribeLabelResponse' {} a -> s {endTime = a} :: DescribeLabelResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates that a label pertains to a particular piece of equipment.
describeLabelResponse_equipment :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_equipment = Lens.lens (\DescribeLabelResponse' {equipment} -> equipment) (\s@DescribeLabelResponse' {} a -> s {equipment = a} :: DescribeLabelResponse)

-- | Indicates the type of anomaly associated with the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
describeLabelResponse_faultCode :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_faultCode = Lens.lens (\DescribeLabelResponse' {faultCode} -> faultCode) (\s@DescribeLabelResponse' {} a -> s {faultCode = a} :: DescribeLabelResponse)

-- | The ARN of the requested label group.
describeLabelResponse_labelGroupArn :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_labelGroupArn = Lens.lens (\DescribeLabelResponse' {labelGroupArn} -> labelGroupArn) (\s@DescribeLabelResponse' {} a -> s {labelGroupArn = a} :: DescribeLabelResponse)

-- | The name of the requested label group.
describeLabelResponse_labelGroupName :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_labelGroupName = Lens.lens (\DescribeLabelResponse' {labelGroupName} -> labelGroupName) (\s@DescribeLabelResponse' {} a -> s {labelGroupName = a} :: DescribeLabelResponse)

-- | The ID of the requested label.
describeLabelResponse_labelId :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_labelId = Lens.lens (\DescribeLabelResponse' {labelId} -> labelId) (\s@DescribeLabelResponse' {} a -> s {labelId = a} :: DescribeLabelResponse)

-- | Metadata providing additional information about the label.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
describeLabelResponse_notes :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.Text)
describeLabelResponse_notes = Lens.lens (\DescribeLabelResponse' {notes} -> notes) (\s@DescribeLabelResponse' {} a -> s {notes = a} :: DescribeLabelResponse)

-- | Indicates whether a labeled event represents an anomaly.
describeLabelResponse_rating :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe LabelRating)
describeLabelResponse_rating = Lens.lens (\DescribeLabelResponse' {rating} -> rating) (\s@DescribeLabelResponse' {} a -> s {rating = a} :: DescribeLabelResponse)

-- | The start time of the requested label.
describeLabelResponse_startTime :: Lens.Lens' DescribeLabelResponse (Prelude.Maybe Prelude.UTCTime)
describeLabelResponse_startTime = Lens.lens (\DescribeLabelResponse' {startTime} -> startTime) (\s@DescribeLabelResponse' {} a -> s {startTime = a} :: DescribeLabelResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeLabelResponse_httpStatus :: Lens.Lens' DescribeLabelResponse Prelude.Int
describeLabelResponse_httpStatus = Lens.lens (\DescribeLabelResponse' {httpStatus} -> httpStatus) (\s@DescribeLabelResponse' {} a -> s {httpStatus = a} :: DescribeLabelResponse)

instance Prelude.NFData DescribeLabelResponse where
  rnf DescribeLabelResponse' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf equipment `Prelude.seq`
          Prelude.rnf faultCode `Prelude.seq`
            Prelude.rnf labelGroupArn `Prelude.seq`
              Prelude.rnf labelGroupName `Prelude.seq`
                Prelude.rnf labelId `Prelude.seq`
                  Prelude.rnf notes `Prelude.seq`
                    Prelude.rnf rating `Prelude.seq`
                      Prelude.rnf startTime `Prelude.seq`
                        Prelude.rnf httpStatus
