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
-- Module      : Amazonka.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other
-- settings for a specific version of a segment that\'s associated with an
-- application.
module Amazonka.Pinpoint.GetSegmentVersion
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { -- | The unique identifier for the segment.
    segmentId :: Prelude.Text,
    -- | The unique version number (Version property) for the campaign version.
    version :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
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
getSegmentVersion_segmentId :: Lens.Lens' GetSegmentVersion Prelude.Text
getSegmentVersion_segmentId = Lens.lens (\GetSegmentVersion' {segmentId} -> segmentId) (\s@GetSegmentVersion' {} a -> s {segmentId = a} :: GetSegmentVersion)

-- | The unique version number (Version property) for the campaign version.
getSegmentVersion_version :: Lens.Lens' GetSegmentVersion Prelude.Text
getSegmentVersion_version = Lens.lens (\GetSegmentVersion' {version} -> version) (\s@GetSegmentVersion' {} a -> s {version = a} :: GetSegmentVersion)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getSegmentVersion_applicationId :: Lens.Lens' GetSegmentVersion Prelude.Text
getSegmentVersion_applicationId = Lens.lens (\GetSegmentVersion' {applicationId} -> applicationId) (\s@GetSegmentVersion' {} a -> s {applicationId = a} :: GetSegmentVersion)

instance Core.AWSRequest GetSegmentVersion where
  type
    AWSResponse GetSegmentVersion =
      GetSegmentVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetSegmentVersion where
  hashWithSalt _salt GetSegmentVersion' {..} =
    _salt `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetSegmentVersion where
  rnf GetSegmentVersion' {..} =
    Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders GetSegmentVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSegmentVersion where
  toPath GetSegmentVersion' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/segments/",
        Core.toBS segmentId,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery GetSegmentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getSegmentVersionResponse_httpStatus :: Lens.Lens' GetSegmentVersionResponse Prelude.Int
getSegmentVersionResponse_httpStatus = Lens.lens (\GetSegmentVersionResponse' {httpStatus} -> httpStatus) (\s@GetSegmentVersionResponse' {} a -> s {httpStatus = a} :: GetSegmentVersionResponse)

-- | Undocumented member.
getSegmentVersionResponse_segmentResponse :: Lens.Lens' GetSegmentVersionResponse SegmentResponse
getSegmentVersionResponse_segmentResponse = Lens.lens (\GetSegmentVersionResponse' {segmentResponse} -> segmentResponse) (\s@GetSegmentVersionResponse' {} a -> s {segmentResponse = a} :: GetSegmentVersionResponse)

instance Prelude.NFData GetSegmentVersionResponse where
  rnf GetSegmentVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segmentResponse
