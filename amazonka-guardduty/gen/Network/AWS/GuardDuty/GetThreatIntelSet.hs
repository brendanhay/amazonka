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
-- Module      : Network.AWS.GuardDuty.GetThreatIntelSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.GetThreatIntelSet
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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetThreatIntelSet' smart constructor.
data GetThreatIntelSet = GetThreatIntelSet'
  { -- | The unique ID of the detector that the threatIntelSet is associated
    -- with.
    detectorId :: Core.Text,
    -- | The unique ID of the threatIntelSet that you want to get.
    threatIntelSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'threatIntelSetId'
  Core.Text ->
  GetThreatIntelSet
newGetThreatIntelSet pDetectorId_ pThreatIntelSetId_ =
  GetThreatIntelSet'
    { detectorId = pDetectorId_,
      threatIntelSetId = pThreatIntelSetId_
    }

-- | The unique ID of the detector that the threatIntelSet is associated
-- with.
getThreatIntelSet_detectorId :: Lens.Lens' GetThreatIntelSet Core.Text
getThreatIntelSet_detectorId = Lens.lens (\GetThreatIntelSet' {detectorId} -> detectorId) (\s@GetThreatIntelSet' {} a -> s {detectorId = a} :: GetThreatIntelSet)

-- | The unique ID of the threatIntelSet that you want to get.
getThreatIntelSet_threatIntelSetId :: Lens.Lens' GetThreatIntelSet Core.Text
getThreatIntelSet_threatIntelSetId = Lens.lens (\GetThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@GetThreatIntelSet' {} a -> s {threatIntelSetId = a} :: GetThreatIntelSet)

instance Core.AWSRequest GetThreatIntelSet where
  type
    AWSResponse GetThreatIntelSet =
      GetThreatIntelSetResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThreatIntelSetResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "name")
            Core.<*> (x Core..:> "format")
            Core.<*> (x Core..:> "location")
            Core.<*> (x Core..:> "status")
      )

instance Core.Hashable GetThreatIntelSet

instance Core.NFData GetThreatIntelSet

instance Core.ToHeaders GetThreatIntelSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetThreatIntelSet where
  toPath GetThreatIntelSet' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/threatintelset/",
        Core.toBS threatIntelSetId
      ]

instance Core.ToQuery GetThreatIntelSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetThreatIntelSetResponse' smart constructor.
data GetThreatIntelSetResponse = GetThreatIntelSetResponse'
  { -- | The tags of the threat list resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A user-friendly ThreatIntelSet name displayed in all findings that are
    -- generated by activity that involves IP addresses included in this
    -- ThreatIntelSet.
    name :: Core.Text,
    -- | The format of the threatIntelSet.
    format :: ThreatIntelSetFormat,
    -- | The URI of the file that contains the ThreatIntelSet. For example:
    -- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
    location :: Core.Text,
    -- | The status of threatIntelSet file uploaded.
    status :: ThreatIntelSetStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'location', 'getThreatIntelSetResponse_location' - The URI of the file that contains the ThreatIntelSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
--
-- 'status', 'getThreatIntelSetResponse_status' - The status of threatIntelSet file uploaded.
newGetThreatIntelSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'name'
  Core.Text ->
  -- | 'format'
  ThreatIntelSetFormat ->
  -- | 'location'
  Core.Text ->
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
      { tags = Core.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        status = pStatus_
      }

-- | The tags of the threat list resource.
getThreatIntelSetResponse_tags :: Lens.Lens' GetThreatIntelSetResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getThreatIntelSetResponse_tags = Lens.lens (\GetThreatIntelSetResponse' {tags} -> tags) (\s@GetThreatIntelSetResponse' {} a -> s {tags = a} :: GetThreatIntelSetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getThreatIntelSetResponse_httpStatus :: Lens.Lens' GetThreatIntelSetResponse Core.Int
getThreatIntelSetResponse_httpStatus = Lens.lens (\GetThreatIntelSetResponse' {httpStatus} -> httpStatus) (\s@GetThreatIntelSetResponse' {} a -> s {httpStatus = a} :: GetThreatIntelSetResponse)

-- | A user-friendly ThreatIntelSet name displayed in all findings that are
-- generated by activity that involves IP addresses included in this
-- ThreatIntelSet.
getThreatIntelSetResponse_name :: Lens.Lens' GetThreatIntelSetResponse Core.Text
getThreatIntelSetResponse_name = Lens.lens (\GetThreatIntelSetResponse' {name} -> name) (\s@GetThreatIntelSetResponse' {} a -> s {name = a} :: GetThreatIntelSetResponse)

-- | The format of the threatIntelSet.
getThreatIntelSetResponse_format :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetFormat
getThreatIntelSetResponse_format = Lens.lens (\GetThreatIntelSetResponse' {format} -> format) (\s@GetThreatIntelSetResponse' {} a -> s {format = a} :: GetThreatIntelSetResponse)

-- | The URI of the file that contains the ThreatIntelSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
getThreatIntelSetResponse_location :: Lens.Lens' GetThreatIntelSetResponse Core.Text
getThreatIntelSetResponse_location = Lens.lens (\GetThreatIntelSetResponse' {location} -> location) (\s@GetThreatIntelSetResponse' {} a -> s {location = a} :: GetThreatIntelSetResponse)

-- | The status of threatIntelSet file uploaded.
getThreatIntelSetResponse_status :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetStatus
getThreatIntelSetResponse_status = Lens.lens (\GetThreatIntelSetResponse' {status} -> status) (\s@GetThreatIntelSetResponse' {} a -> s {status = a} :: GetThreatIntelSetResponse)

instance Core.NFData GetThreatIntelSetResponse
