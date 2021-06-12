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
-- Module      : Network.AWS.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other
-- settings for a specific version of a segment that\'s associated with an
-- application.
module Network.AWS.Pinpoint.GetSegmentVersion
  ( -- * Creating a Request
    GetSegmentVersion (..),
    newGetSegmentVersion,

    -- * Request Lenses
    getSegmentVersion_segmentId,
    getSegmentVersion_version,
    getSegmentVersion_applicationId,

    -- * Destructuring the Response
    GetSegmentVersionResponse (..),
    newGetSegmentVersionResponse,

    -- * Response Lenses
    getSegmentVersionResponse_httpStatus,
    getSegmentVersionResponse_segmentResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique version number (Version property) for the campaign version.
    version :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'getSegmentVersion_segmentId' - The unique identifier for the segment.
--
-- 'version', 'getSegmentVersion_version' - The unique version number (Version property) for the campaign version.
--
-- 'applicationId', 'getSegmentVersion_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetSegmentVersion ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentVersion
newGetSegmentVersion
  pSegmentId_
  pVersion_
  pApplicationId_ =
    GetSegmentVersion'
      { segmentId = pSegmentId_,
        version = pVersion_,
        applicationId = pApplicationId_
      }

-- | The unique identifier for the segment.
getSegmentVersion_segmentId :: Lens.Lens' GetSegmentVersion Core.Text
getSegmentVersion_segmentId = Lens.lens (\GetSegmentVersion' {segmentId} -> segmentId) (\s@GetSegmentVersion' {} a -> s {segmentId = a} :: GetSegmentVersion)

-- | The unique version number (Version property) for the campaign version.
getSegmentVersion_version :: Lens.Lens' GetSegmentVersion Core.Text
getSegmentVersion_version = Lens.lens (\GetSegmentVersion' {version} -> version) (\s@GetSegmentVersion' {} a -> s {version = a} :: GetSegmentVersion)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentVersion_applicationId :: Lens.Lens' GetSegmentVersion Core.Text
getSegmentVersion_applicationId = Lens.lens (\GetSegmentVersion' {applicationId} -> applicationId) (\s@GetSegmentVersion' {} a -> s {applicationId = a} :: GetSegmentVersion)

instance Core.AWSRequest GetSegmentVersion where
  type
    AWSResponse GetSegmentVersion =
      GetSegmentVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetSegmentVersion

instance Core.NFData GetSegmentVersion

instance Core.ToHeaders GetSegmentVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSegmentVersion where
  toPath GetSegmentVersion' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery GetSegmentVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSegmentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentVersionResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'getSegmentVersionResponse_segmentResponse' - Undocumented member.
newGetSegmentVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  GetSegmentVersionResponse
newGetSegmentVersionResponse
  pHttpStatus_
  pSegmentResponse_ =
    GetSegmentVersionResponse'
      { httpStatus =
          pHttpStatus_,
        segmentResponse = pSegmentResponse_
      }

-- | The response's http status code.
getSegmentVersionResponse_httpStatus :: Lens.Lens' GetSegmentVersionResponse Core.Int
getSegmentVersionResponse_httpStatus = Lens.lens (\GetSegmentVersionResponse' {httpStatus} -> httpStatus) (\s@GetSegmentVersionResponse' {} a -> s {httpStatus = a} :: GetSegmentVersionResponse)

-- | Undocumented member.
getSegmentVersionResponse_segmentResponse :: Lens.Lens' GetSegmentVersionResponse SegmentResponse
getSegmentVersionResponse_segmentResponse = Lens.lens (\GetSegmentVersionResponse' {segmentResponse} -> segmentResponse) (\s@GetSegmentVersionResponse' {} a -> s {segmentResponse = a} :: GetSegmentVersionResponse)

instance Core.NFData GetSegmentVersionResponse
