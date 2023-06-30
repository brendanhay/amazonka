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
-- Module      : Amazonka.GuardDuty.GetThreatIntelSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
module Amazonka.GuardDuty.GetThreatIntelSet
  ( -- * Creating a Request
    GetThreatIntelSet (..),
    newGetThreatIntelSet,

    -- * Request Lenses
    getThreatIntelSet_detectorId,
    getThreatIntelSet_threatIntelSetId,

    -- * Destructuring the Response
    GetThreatIntelSetResponse (..),
    newGetThreatIntelSetResponse,

    -- * Response Lenses
    getThreatIntelSetResponse_tags,
    getThreatIntelSetResponse_httpStatus,
    getThreatIntelSetResponse_name,
    getThreatIntelSetResponse_format,
    getThreatIntelSetResponse_location,
    getThreatIntelSetResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetThreatIntelSet' smart constructor.
data GetThreatIntelSet = GetThreatIntelSet'
  { -- | The unique ID of the detector that the threatIntelSet is associated
    -- with.
    detectorId :: Prelude.Text,
    -- | The unique ID of the threatIntelSet that you want to get.
    threatIntelSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThreatIntelSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getThreatIntelSet_detectorId' - The unique ID of the detector that the threatIntelSet is associated
-- with.
--
-- 'threatIntelSetId', 'getThreatIntelSet_threatIntelSetId' - The unique ID of the threatIntelSet that you want to get.
newGetThreatIntelSet ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'threatIntelSetId'
  Prelude.Text ->
  GetThreatIntelSet
newGetThreatIntelSet pDetectorId_ pThreatIntelSetId_ =
  GetThreatIntelSet'
    { detectorId = pDetectorId_,
      threatIntelSetId = pThreatIntelSetId_
    }

-- | The unique ID of the detector that the threatIntelSet is associated
-- with.
getThreatIntelSet_detectorId :: Lens.Lens' GetThreatIntelSet Prelude.Text
getThreatIntelSet_detectorId = Lens.lens (\GetThreatIntelSet' {detectorId} -> detectorId) (\s@GetThreatIntelSet' {} a -> s {detectorId = a} :: GetThreatIntelSet)

-- | The unique ID of the threatIntelSet that you want to get.
getThreatIntelSet_threatIntelSetId :: Lens.Lens' GetThreatIntelSet Prelude.Text
getThreatIntelSet_threatIntelSetId = Lens.lens (\GetThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@GetThreatIntelSet' {} a -> s {threatIntelSetId = a} :: GetThreatIntelSet)

instance Core.AWSRequest GetThreatIntelSet where
  type
    AWSResponse GetThreatIntelSet =
      GetThreatIntelSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThreatIntelSetResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "format")
            Prelude.<*> (x Data..:> "location")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetThreatIntelSet where
  hashWithSalt _salt GetThreatIntelSet' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` threatIntelSetId

instance Prelude.NFData GetThreatIntelSet where
  rnf GetThreatIntelSet' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf threatIntelSetId

instance Data.ToHeaders GetThreatIntelSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetThreatIntelSet where
  toPath GetThreatIntelSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/threatintelset/",
        Data.toBS threatIntelSetId
      ]

instance Data.ToQuery GetThreatIntelSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetThreatIntelSetResponse' smart constructor.
data GetThreatIntelSetResponse = GetThreatIntelSetResponse'
  { -- | The tags of the threat list resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A user-friendly ThreatIntelSet name displayed in all findings that are
    -- generated by activity that involves IP addresses included in this
    -- ThreatIntelSet.
    name :: Prelude.Text,
    -- | The format of the threatIntelSet.
    format :: ThreatIntelSetFormat,
    -- | The URI of the file that contains the ThreatIntelSet.
    location :: Prelude.Text,
    -- | The status of threatIntelSet file uploaded.
    status :: ThreatIntelSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThreatIntelSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getThreatIntelSetResponse_tags' - The tags of the threat list resource.
--
-- 'httpStatus', 'getThreatIntelSetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getThreatIntelSetResponse_name' - A user-friendly ThreatIntelSet name displayed in all findings that are
-- generated by activity that involves IP addresses included in this
-- ThreatIntelSet.
--
-- 'format', 'getThreatIntelSetResponse_format' - The format of the threatIntelSet.
--
-- 'location', 'getThreatIntelSetResponse_location' - The URI of the file that contains the ThreatIntelSet.
--
-- 'status', 'getThreatIntelSetResponse_status' - The status of threatIntelSet file uploaded.
newGetThreatIntelSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'format'
  ThreatIntelSetFormat ->
  -- | 'location'
  Prelude.Text ->
  -- | 'status'
  ThreatIntelSetStatus ->
  GetThreatIntelSetResponse
newGetThreatIntelSetResponse
  pHttpStatus_
  pName_
  pFormat_
  pLocation_
  pStatus_ =
    GetThreatIntelSetResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        status = pStatus_
      }

-- | The tags of the threat list resource.
getThreatIntelSetResponse_tags :: Lens.Lens' GetThreatIntelSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getThreatIntelSetResponse_tags = Lens.lens (\GetThreatIntelSetResponse' {tags} -> tags) (\s@GetThreatIntelSetResponse' {} a -> s {tags = a} :: GetThreatIntelSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getThreatIntelSetResponse_httpStatus :: Lens.Lens' GetThreatIntelSetResponse Prelude.Int
getThreatIntelSetResponse_httpStatus = Lens.lens (\GetThreatIntelSetResponse' {httpStatus} -> httpStatus) (\s@GetThreatIntelSetResponse' {} a -> s {httpStatus = a} :: GetThreatIntelSetResponse)

-- | A user-friendly ThreatIntelSet name displayed in all findings that are
-- generated by activity that involves IP addresses included in this
-- ThreatIntelSet.
getThreatIntelSetResponse_name :: Lens.Lens' GetThreatIntelSetResponse Prelude.Text
getThreatIntelSetResponse_name = Lens.lens (\GetThreatIntelSetResponse' {name} -> name) (\s@GetThreatIntelSetResponse' {} a -> s {name = a} :: GetThreatIntelSetResponse)

-- | The format of the threatIntelSet.
getThreatIntelSetResponse_format :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetFormat
getThreatIntelSetResponse_format = Lens.lens (\GetThreatIntelSetResponse' {format} -> format) (\s@GetThreatIntelSetResponse' {} a -> s {format = a} :: GetThreatIntelSetResponse)

-- | The URI of the file that contains the ThreatIntelSet.
getThreatIntelSetResponse_location :: Lens.Lens' GetThreatIntelSetResponse Prelude.Text
getThreatIntelSetResponse_location = Lens.lens (\GetThreatIntelSetResponse' {location} -> location) (\s@GetThreatIntelSetResponse' {} a -> s {location = a} :: GetThreatIntelSetResponse)

-- | The status of threatIntelSet file uploaded.
getThreatIntelSetResponse_status :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetStatus
getThreatIntelSetResponse_status = Lens.lens (\GetThreatIntelSetResponse' {status} -> status) (\s@GetThreatIntelSetResponse' {} a -> s {status = a} :: GetThreatIntelSetResponse)

instance Prelude.NFData GetThreatIntelSetResponse where
  rnf GetThreatIntelSetResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf status
