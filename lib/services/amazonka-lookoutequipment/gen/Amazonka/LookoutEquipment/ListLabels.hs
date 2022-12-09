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
-- Module      : Amazonka.LookoutEquipment.ListLabels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of labels.
module Amazonka.LookoutEquipment.ListLabels
  ( -- * Creating a Request
    ListLabels (..),
    newListLabels,

    -- * Request Lenses
    listLabels_equipment,
    listLabels_faultCode,
    listLabels_intervalEndTime,
    listLabels_intervalStartTime,
    listLabels_maxResults,
    listLabels_nextToken,
    listLabels_labelGroupName,

    -- * Destructuring the Response
    ListLabelsResponse (..),
    newListLabelsResponse,

    -- * Response Lenses
    listLabelsResponse_labelSummaries,
    listLabelsResponse_nextToken,
    listLabelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLabels' smart constructor.
data ListLabels = ListLabels'
  { -- | Lists the labels that pertain to a particular piece of equipment.
    equipment :: Prelude.Maybe Prelude.Text,
    -- | Returns labels with a particular fault code.
    faultCode :: Prelude.Maybe Prelude.Text,
    -- | Returns all labels with a start time earlier than the end time given.
    intervalEndTime :: Prelude.Maybe Data.POSIX,
    -- | Returns all the labels with a end time equal to or later than the start
    -- time given.
    intervalStartTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the maximum number of labels to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque pagination token indicating where to continue the listing of
    -- label groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Retruns the name of the label group.
    labelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'equipment', 'listLabels_equipment' - Lists the labels that pertain to a particular piece of equipment.
--
-- 'faultCode', 'listLabels_faultCode' - Returns labels with a particular fault code.
--
-- 'intervalEndTime', 'listLabels_intervalEndTime' - Returns all labels with a start time earlier than the end time given.
--
-- 'intervalStartTime', 'listLabels_intervalStartTime' - Returns all the labels with a end time equal to or later than the start
-- time given.
--
-- 'maxResults', 'listLabels_maxResults' - Specifies the maximum number of labels to list.
--
-- 'nextToken', 'listLabels_nextToken' - An opaque pagination token indicating where to continue the listing of
-- label groups.
--
-- 'labelGroupName', 'listLabels_labelGroupName' - Retruns the name of the label group.
newListLabels ::
  -- | 'labelGroupName'
  Prelude.Text ->
  ListLabels
newListLabels pLabelGroupName_ =
  ListLabels'
    { equipment = Prelude.Nothing,
      faultCode = Prelude.Nothing,
      intervalEndTime = Prelude.Nothing,
      intervalStartTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      labelGroupName = pLabelGroupName_
    }

-- | Lists the labels that pertain to a particular piece of equipment.
listLabels_equipment :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.Text)
listLabels_equipment = Lens.lens (\ListLabels' {equipment} -> equipment) (\s@ListLabels' {} a -> s {equipment = a} :: ListLabels)

-- | Returns labels with a particular fault code.
listLabels_faultCode :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.Text)
listLabels_faultCode = Lens.lens (\ListLabels' {faultCode} -> faultCode) (\s@ListLabels' {} a -> s {faultCode = a} :: ListLabels)

-- | Returns all labels with a start time earlier than the end time given.
listLabels_intervalEndTime :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.UTCTime)
listLabels_intervalEndTime = Lens.lens (\ListLabels' {intervalEndTime} -> intervalEndTime) (\s@ListLabels' {} a -> s {intervalEndTime = a} :: ListLabels) Prelude.. Lens.mapping Data._Time

-- | Returns all the labels with a end time equal to or later than the start
-- time given.
listLabels_intervalStartTime :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.UTCTime)
listLabels_intervalStartTime = Lens.lens (\ListLabels' {intervalStartTime} -> intervalStartTime) (\s@ListLabels' {} a -> s {intervalStartTime = a} :: ListLabels) Prelude.. Lens.mapping Data._Time

-- | Specifies the maximum number of labels to list.
listLabels_maxResults :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.Natural)
listLabels_maxResults = Lens.lens (\ListLabels' {maxResults} -> maxResults) (\s@ListLabels' {} a -> s {maxResults = a} :: ListLabels)

-- | An opaque pagination token indicating where to continue the listing of
-- label groups.
listLabels_nextToken :: Lens.Lens' ListLabels (Prelude.Maybe Prelude.Text)
listLabels_nextToken = Lens.lens (\ListLabels' {nextToken} -> nextToken) (\s@ListLabels' {} a -> s {nextToken = a} :: ListLabels)

-- | Retruns the name of the label group.
listLabels_labelGroupName :: Lens.Lens' ListLabels Prelude.Text
listLabels_labelGroupName = Lens.lens (\ListLabels' {labelGroupName} -> labelGroupName) (\s@ListLabels' {} a -> s {labelGroupName = a} :: ListLabels)

instance Core.AWSRequest ListLabels where
  type AWSResponse ListLabels = ListLabelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelsResponse'
            Prelude.<$> (x Data..?> "LabelSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLabels where
  hashWithSalt _salt ListLabels' {..} =
    _salt `Prelude.hashWithSalt` equipment
      `Prelude.hashWithSalt` faultCode
      `Prelude.hashWithSalt` intervalEndTime
      `Prelude.hashWithSalt` intervalStartTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData ListLabels where
  rnf ListLabels' {..} =
    Prelude.rnf equipment
      `Prelude.seq` Prelude.rnf faultCode
      `Prelude.seq` Prelude.rnf intervalEndTime
      `Prelude.seq` Prelude.rnf intervalStartTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf labelGroupName

instance Data.ToHeaders ListLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListLabels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLabels where
  toJSON ListLabels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Equipment" Data..=) Prelude.<$> equipment,
            ("FaultCode" Data..=) Prelude.<$> faultCode,
            ("IntervalEndTime" Data..=)
              Prelude.<$> intervalEndTime,
            ("IntervalStartTime" Data..=)
              Prelude.<$> intervalStartTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("LabelGroupName" Data..= labelGroupName)
          ]
      )

instance Data.ToPath ListLabels where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLabelsResponse' smart constructor.
data ListLabelsResponse = ListLabelsResponse'
  { -- | A summary of the items in the label group.
    labelSummaries :: Prelude.Maybe [LabelSummary],
    -- | An opaque pagination token indicating where to continue the listing of
    -- datasets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelSummaries', 'listLabelsResponse_labelSummaries' - A summary of the items in the label group.
--
-- 'nextToken', 'listLabelsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- datasets.
--
-- 'httpStatus', 'listLabelsResponse_httpStatus' - The response's http status code.
newListLabelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLabelsResponse
newListLabelsResponse pHttpStatus_ =
  ListLabelsResponse'
    { labelSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of the items in the label group.
listLabelsResponse_labelSummaries :: Lens.Lens' ListLabelsResponse (Prelude.Maybe [LabelSummary])
listLabelsResponse_labelSummaries = Lens.lens (\ListLabelsResponse' {labelSummaries} -> labelSummaries) (\s@ListLabelsResponse' {} a -> s {labelSummaries = a} :: ListLabelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque pagination token indicating where to continue the listing of
-- datasets.
listLabelsResponse_nextToken :: Lens.Lens' ListLabelsResponse (Prelude.Maybe Prelude.Text)
listLabelsResponse_nextToken = Lens.lens (\ListLabelsResponse' {nextToken} -> nextToken) (\s@ListLabelsResponse' {} a -> s {nextToken = a} :: ListLabelsResponse)

-- | The response's http status code.
listLabelsResponse_httpStatus :: Lens.Lens' ListLabelsResponse Prelude.Int
listLabelsResponse_httpStatus = Lens.lens (\ListLabelsResponse' {httpStatus} -> httpStatus) (\s@ListLabelsResponse' {} a -> s {httpStatus = a} :: ListLabelsResponse)

instance Prelude.NFData ListLabelsResponse where
  rnf ListLabelsResponse' {..} =
    Prelude.rnf labelSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
