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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getLaunchProfileDetails_launchProfileId,
    getLaunchProfileDetails_studioId,

    -- * Destructuring the Response
    GetLaunchProfileDetailsResponse (..),
    newGetLaunchProfileDetailsResponse,

    -- * Response Lenses
    getLaunchProfileDetailsResponse_launchProfile,
    getLaunchProfileDetailsResponse_studioComponentSummaries,
    getLaunchProfileDetailsResponse_streamingImages,
    getLaunchProfileDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfileDetails' smart constructor.
data GetLaunchProfileDetails = GetLaunchProfileDetails'
  { -- | The Launch Profile ID.
    launchProfileId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
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
-- 'launchProfileId', 'getLaunchProfileDetails_launchProfileId' - The Launch Profile ID.
--
-- 'studioId', 'getLaunchProfileDetails_studioId' - The studio ID.
newGetLaunchProfileDetails ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetLaunchProfileDetails
newGetLaunchProfileDetails
  pLaunchProfileId_
  pStudioId_ =
    GetLaunchProfileDetails'
      { launchProfileId =
          pLaunchProfileId_,
        studioId = pStudioId_
      }

-- | The Launch Profile ID.
getLaunchProfileDetails_launchProfileId :: Lens.Lens' GetLaunchProfileDetails Prelude.Text
getLaunchProfileDetails_launchProfileId = Lens.lens (\GetLaunchProfileDetails' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfileDetails' {} a -> s {launchProfileId = a} :: GetLaunchProfileDetails)

-- | The studio ID.
getLaunchProfileDetails_studioId :: Lens.Lens' GetLaunchProfileDetails Prelude.Text
getLaunchProfileDetails_studioId = Lens.lens (\GetLaunchProfileDetails' {studioId} -> studioId) (\s@GetLaunchProfileDetails' {} a -> s {studioId = a} :: GetLaunchProfileDetails)

instance Core.AWSRequest GetLaunchProfileDetails where
  type
    AWSResponse GetLaunchProfileDetails =
      GetLaunchProfileDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileDetailsResponse'
            Prelude.<$> (x Data..?> "launchProfile")
            Prelude.<*> ( x Data..?> "studioComponentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "streamingImages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfileDetails where
  hashWithSalt _salt GetLaunchProfileDetails' {..} =
    _salt `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetLaunchProfileDetails where
  rnf GetLaunchProfileDetails' {..} =
    Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetLaunchProfileDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLaunchProfileDetails where
  toPath GetLaunchProfileDetails' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/launch-profiles/",
        Data.toBS launchProfileId,
        "/details"
      ]

instance Data.ToQuery GetLaunchProfileDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchProfileDetailsResponse' smart constructor.
data GetLaunchProfileDetailsResponse = GetLaunchProfileDetailsResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | A collection of studio component summaries.
    studioComponentSummaries :: Prelude.Maybe [StudioComponentSummary],
    -- | A collection of streaming images.
    streamingImages :: Prelude.Maybe [StreamingImage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfile', 'getLaunchProfileDetailsResponse_launchProfile' - The launch profile.
--
-- 'studioComponentSummaries', 'getLaunchProfileDetailsResponse_studioComponentSummaries' - A collection of studio component summaries.
--
-- 'streamingImages', 'getLaunchProfileDetailsResponse_streamingImages' - A collection of streaming images.
--
-- 'httpStatus', 'getLaunchProfileDetailsResponse_httpStatus' - The response's http status code.
newGetLaunchProfileDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchProfileDetailsResponse
newGetLaunchProfileDetailsResponse pHttpStatus_ =
  GetLaunchProfileDetailsResponse'
    { launchProfile =
        Prelude.Nothing,
      studioComponentSummaries = Prelude.Nothing,
      streamingImages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The launch profile.
getLaunchProfileDetailsResponse_launchProfile :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe LaunchProfile)
getLaunchProfileDetailsResponse_launchProfile = Lens.lens (\GetLaunchProfileDetailsResponse' {launchProfile} -> launchProfile) (\s@GetLaunchProfileDetailsResponse' {} a -> s {launchProfile = a} :: GetLaunchProfileDetailsResponse)

-- | A collection of studio component summaries.
getLaunchProfileDetailsResponse_studioComponentSummaries :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe [StudioComponentSummary])
getLaunchProfileDetailsResponse_studioComponentSummaries = Lens.lens (\GetLaunchProfileDetailsResponse' {studioComponentSummaries} -> studioComponentSummaries) (\s@GetLaunchProfileDetailsResponse' {} a -> s {studioComponentSummaries = a} :: GetLaunchProfileDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A collection of streaming images.
getLaunchProfileDetailsResponse_streamingImages :: Lens.Lens' GetLaunchProfileDetailsResponse (Prelude.Maybe [StreamingImage])
getLaunchProfileDetailsResponse_streamingImages = Lens.lens (\GetLaunchProfileDetailsResponse' {streamingImages} -> streamingImages) (\s@GetLaunchProfileDetailsResponse' {} a -> s {streamingImages = a} :: GetLaunchProfileDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLaunchProfileDetailsResponse_httpStatus :: Lens.Lens' GetLaunchProfileDetailsResponse Prelude.Int
getLaunchProfileDetailsResponse_httpStatus = Lens.lens (\GetLaunchProfileDetailsResponse' {httpStatus} -> httpStatus) (\s@GetLaunchProfileDetailsResponse' {} a -> s {httpStatus = a} :: GetLaunchProfileDetailsResponse)

instance
  Prelude.NFData
    GetLaunchProfileDetailsResponse
  where
  rnf GetLaunchProfileDetailsResponse' {..} =
    Prelude.rnf launchProfile
      `Prelude.seq` Prelude.rnf studioComponentSummaries
      `Prelude.seq` Prelude.rnf streamingImages
      `Prelude.seq` Prelude.rnf httpStatus
