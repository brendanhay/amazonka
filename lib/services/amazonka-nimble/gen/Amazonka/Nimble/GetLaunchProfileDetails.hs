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
-- Module      : Amazonka.Nimble.GetLaunchProfileDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launch profile details include the launch profile resource and summary
-- information of resources that are used by, or available to, the launch
-- profile. This includes the name and description of all studio components
-- used by the launch profiles, and the name and description of streaming
-- images that can be used with this launch profile.
module Amazonka.Nimble.GetLaunchProfileDetails
  ( -- * Creating a Request
    GetLaunchProfileDetails (..),
    newGetLaunchProfileDetails,

    -- * Request Lenses
    getLaunchProfileDetails_studioId,
    getLaunchProfileDetails_launchProfileId,

    -- * Destructuring the Response
    GetLaunchProfileDetailsResponse (..),
    newGetLaunchProfileDetailsResponse,

    -- * Response Lenses
    getLaunchProfileDetailsResponse_streamingImages,
    getLaunchProfileDetailsResponse_launchProfile,
    getLaunchProfileDetailsResponse_studioComponentSummaries,
    getLaunchProfileDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfileDetails' smart constructor.
data GetLaunchProfileDetails = GetLaunchProfileDetails'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'getLaunchProfileDetails_studioId' - The studio ID.
--
-- 'launchProfileId', 'getLaunchProfileDetails_launchProfileId' - The launch profile ID.
newGetLaunchProfileDetails ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  GetLaunchProfileDetails
newGetLaunchProfileDetails
  pStudioId_
  pLaunchProfileId_ =
    GetLaunchProfileDetails'
      { studioId = pStudioId_,
        launchProfileId = pLaunchProfileId_
      }

-- | The studio ID.
getLaunchProfileDetails_studioId :: Lens.Lens' GetLaunchProfileDetails Prelude.Text
getLaunchProfileDetails_studioId = Lens.lens (\GetLaunchProfileDetails' {studioId} -> studioId) (\s@GetLaunchProfileDetails' {} a -> s {studioId = a} :: GetLaunchProfileDetails)

-- | The launch profile ID.
getLaunchProfileDetails_launchProfileId :: Lens.Lens' GetLaunchProfileDetails Prelude.Text
getLaunchProfileDetails_launchProfileId = Lens.lens (\GetLaunchProfileDetails' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfileDetails' {} a -> s {launchProfileId = a} :: GetLaunchProfileDetails)

instance Core.AWSRequest GetLaunchProfileDetails where
  type
    AWSResponse GetLaunchProfileDetails =
      GetLaunchProfileDetailsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileDetailsResponse'
            Prelude.<$> ( x Core..?> "streamingImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "launchProfile")
            Prelude.<*> ( x Core..?> "studioComponentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfileDetails where
  hashWithSalt _salt GetLaunchProfileDetails' {..} =
    _salt `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` launchProfileId

instance Prelude.NFData GetLaunchProfileDetails where
  rnf GetLaunchProfileDetails' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf launchProfileId

instance Core.ToHeaders GetLaunchProfileDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLaunchProfileDetails where
  toPath GetLaunchProfileDetails' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId,
        "/details"
      ]

instance Core.ToQuery GetLaunchProfileDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchProfileDetailsResponse' smart constructor.
data GetLaunchProfileDetailsResponse = GetLaunchProfileDetailsResponse'
  { -- | A collection of streaming images.
    streamingImages :: Prelude.Maybe [StreamingImage],
    -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | A collection of studio component summaries.
    studioComponentSummaries :: Prelude.Maybe [StudioComponentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImages', 'getLaunchProfileDetailsResponse_streamingImages' - A collection of streaming images.
--
-- 'launchProfile', 'getLaunchProfileDetailsResponse_launchProfile' - The launch profile.
--
-- 'studioComponentSummaries', 'getLaunchProfileDetailsResponse_studioComponentSummaries' - A collection of studio component summaries.
--
-- 'httpStatus', 'getLaunchProfileDetailsResponse_httpStatus' - The response's http status code.
newGetLaunchProfileDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchProfileDetailsResponse
newGetLaunchProfileDetailsResponse pHttpStatus_ =
  GetLaunchProfileDetailsResponse'
    { streamingImages =
        Prelude.Nothing,
      launchProfile = Prelude.Nothing,
      studioComponentSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of streaming images.
getLaunchProfileDetailsResponse_streamingImages :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe [StreamingImage])
getLaunchProfileDetailsResponse_streamingImages = Lens.lens (\GetLaunchProfileDetailsResponse' {streamingImages} -> streamingImages) (\s@GetLaunchProfileDetailsResponse' {} a -> s {streamingImages = a} :: GetLaunchProfileDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The launch profile.
getLaunchProfileDetailsResponse_launchProfile :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe LaunchProfile)
getLaunchProfileDetailsResponse_launchProfile = Lens.lens (\GetLaunchProfileDetailsResponse' {launchProfile} -> launchProfile) (\s@GetLaunchProfileDetailsResponse' {} a -> s {launchProfile = a} :: GetLaunchProfileDetailsResponse)

-- | A collection of studio component summaries.
getLaunchProfileDetailsResponse_studioComponentSummaries :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe [StudioComponentSummary])
getLaunchProfileDetailsResponse_studioComponentSummaries = Lens.lens (\GetLaunchProfileDetailsResponse' {studioComponentSummaries} -> studioComponentSummaries) (\s@GetLaunchProfileDetailsResponse' {} a -> s {studioComponentSummaries = a} :: GetLaunchProfileDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLaunchProfileDetailsResponse_httpStatus :: Lens.Lens' GetLaunchProfileDetailsResponse Prelude.Int
getLaunchProfileDetailsResponse_httpStatus = Lens.lens (\GetLaunchProfileDetailsResponse' {httpStatus} -> httpStatus) (\s@GetLaunchProfileDetailsResponse' {} a -> s {httpStatus = a} :: GetLaunchProfileDetailsResponse)

instance
  Prelude.NFData
    GetLaunchProfileDetailsResponse
  where
  rnf GetLaunchProfileDetailsResponse' {..} =
    Prelude.rnf streamingImages
      `Prelude.seq` Prelude.rnf launchProfile
      `Prelude.seq` Prelude.rnf studioComponentSummaries
      `Prelude.seq` Prelude.rnf httpStatus
