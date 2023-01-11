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
-- Module      : Amazonka.LookoutEquipment.DescribeLabelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the label group.
module Amazonka.LookoutEquipment.DescribeLabelGroup
  ( -- * Creating a Request
    DescribeLabelGroup (..),
    newDescribeLabelGroup,

    -- * Request Lenses
    describeLabelGroup_labelGroupName,

    -- * Destructuring the Response
    DescribeLabelGroupResponse (..),
    newDescribeLabelGroupResponse,

    -- * Response Lenses
    describeLabelGroupResponse_createdAt,
    describeLabelGroupResponse_faultCodes,
    describeLabelGroupResponse_labelGroupArn,
    describeLabelGroupResponse_labelGroupName,
    describeLabelGroupResponse_updatedAt,
    describeLabelGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLabelGroup' smart constructor.
data DescribeLabelGroup = DescribeLabelGroup'
  { -- | Returns the name of the label group.
    labelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupName', 'describeLabelGroup_labelGroupName' - Returns the name of the label group.
newDescribeLabelGroup ::
  -- | 'labelGroupName'
  Prelude.Text ->
  DescribeLabelGroup
newDescribeLabelGroup pLabelGroupName_ =
  DescribeLabelGroup'
    { labelGroupName =
        pLabelGroupName_
    }

-- | Returns the name of the label group.
describeLabelGroup_labelGroupName :: Lens.Lens' DescribeLabelGroup Prelude.Text
describeLabelGroup_labelGroupName = Lens.lens (\DescribeLabelGroup' {labelGroupName} -> labelGroupName) (\s@DescribeLabelGroup' {} a -> s {labelGroupName = a} :: DescribeLabelGroup)

instance Core.AWSRequest DescribeLabelGroup where
  type
    AWSResponse DescribeLabelGroup =
      DescribeLabelGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLabelGroupResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "FaultCodes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LabelGroupArn")
            Prelude.<*> (x Data..?> "LabelGroupName")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLabelGroup where
  hashWithSalt _salt DescribeLabelGroup' {..} =
    _salt `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData DescribeLabelGroup where
  rnf DescribeLabelGroup' {..} =
    Prelude.rnf labelGroupName

instance Data.ToHeaders DescribeLabelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DescribeLabelGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLabelGroup where
  toJSON DescribeLabelGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelGroupName" Data..= labelGroupName)
          ]
      )

instance Data.ToPath DescribeLabelGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLabelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLabelGroupResponse' smart constructor.
data DescribeLabelGroupResponse = DescribeLabelGroupResponse'
  { -- | The time at which the label group was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Codes indicating the type of anomaly associated with the labels in the
    -- lagbel group.
    faultCodes :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the label group.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the label group.
    labelGroupName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the label group was updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLabelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'describeLabelGroupResponse_createdAt' - The time at which the label group was created.
--
-- 'faultCodes', 'describeLabelGroupResponse_faultCodes' - Codes indicating the type of anomaly associated with the labels in the
-- lagbel group.
--
-- 'labelGroupArn', 'describeLabelGroupResponse_labelGroupArn' - The ARN of the label group.
--
-- 'labelGroupName', 'describeLabelGroupResponse_labelGroupName' - The name of the label group.
--
-- 'updatedAt', 'describeLabelGroupResponse_updatedAt' - The time at which the label group was updated.
--
-- 'httpStatus', 'describeLabelGroupResponse_httpStatus' - The response's http status code.
newDescribeLabelGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLabelGroupResponse
newDescribeLabelGroupResponse pHttpStatus_ =
  DescribeLabelGroupResponse'
    { createdAt =
        Prelude.Nothing,
      faultCodes = Prelude.Nothing,
      labelGroupArn = Prelude.Nothing,
      labelGroupName = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the label group was created.
describeLabelGroupResponse_createdAt :: Lens.Lens' DescribeLabelGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeLabelGroupResponse_createdAt = Lens.lens (\DescribeLabelGroupResponse' {createdAt} -> createdAt) (\s@DescribeLabelGroupResponse' {} a -> s {createdAt = a} :: DescribeLabelGroupResponse) Prelude.. Lens.mapping Data._Time

-- | Codes indicating the type of anomaly associated with the labels in the
-- lagbel group.
describeLabelGroupResponse_faultCodes :: Lens.Lens' DescribeLabelGroupResponse (Prelude.Maybe [Prelude.Text])
describeLabelGroupResponse_faultCodes = Lens.lens (\DescribeLabelGroupResponse' {faultCodes} -> faultCodes) (\s@DescribeLabelGroupResponse' {} a -> s {faultCodes = a} :: DescribeLabelGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the label group.
describeLabelGroupResponse_labelGroupArn :: Lens.Lens' DescribeLabelGroupResponse (Prelude.Maybe Prelude.Text)
describeLabelGroupResponse_labelGroupArn = Lens.lens (\DescribeLabelGroupResponse' {labelGroupArn} -> labelGroupArn) (\s@DescribeLabelGroupResponse' {} a -> s {labelGroupArn = a} :: DescribeLabelGroupResponse)

-- | The name of the label group.
describeLabelGroupResponse_labelGroupName :: Lens.Lens' DescribeLabelGroupResponse (Prelude.Maybe Prelude.Text)
describeLabelGroupResponse_labelGroupName = Lens.lens (\DescribeLabelGroupResponse' {labelGroupName} -> labelGroupName) (\s@DescribeLabelGroupResponse' {} a -> s {labelGroupName = a} :: DescribeLabelGroupResponse)

-- | The time at which the label group was updated.
describeLabelGroupResponse_updatedAt :: Lens.Lens' DescribeLabelGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeLabelGroupResponse_updatedAt = Lens.lens (\DescribeLabelGroupResponse' {updatedAt} -> updatedAt) (\s@DescribeLabelGroupResponse' {} a -> s {updatedAt = a} :: DescribeLabelGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeLabelGroupResponse_httpStatus :: Lens.Lens' DescribeLabelGroupResponse Prelude.Int
describeLabelGroupResponse_httpStatus = Lens.lens (\DescribeLabelGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeLabelGroupResponse' {} a -> s {httpStatus = a} :: DescribeLabelGroupResponse)

instance Prelude.NFData DescribeLabelGroupResponse where
  rnf DescribeLabelGroupResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf faultCodes
      `Prelude.seq` Prelude.rnf labelGroupArn
      `Prelude.seq` Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
