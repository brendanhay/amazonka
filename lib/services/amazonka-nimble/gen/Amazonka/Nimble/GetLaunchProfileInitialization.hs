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
-- Module      : Amazonka.Nimble.GetLaunchProfileInitialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a launch profile initialization.
module Amazonka.Nimble.GetLaunchProfileInitialization
  ( -- * Creating a Request
    GetLaunchProfileInitialization (..),
    newGetLaunchProfileInitialization,

    -- * Request Lenses
    getLaunchProfileInitialization_studioId,
    getLaunchProfileInitialization_launchProfileProtocolVersions,
    getLaunchProfileInitialization_launchPurpose,
    getLaunchProfileInitialization_launchProfileId,
    getLaunchProfileInitialization_platform,

    -- * Destructuring the Response
    GetLaunchProfileInitializationResponse (..),
    newGetLaunchProfileInitializationResponse,

    -- * Response Lenses
    getLaunchProfileInitializationResponse_launchProfileInitialization,
    getLaunchProfileInitializationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfileInitialization' smart constructor.
data GetLaunchProfileInitialization = GetLaunchProfileInitialization'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | A collection of launch profile protocol versions.
    launchProfileProtocolVersions :: [Prelude.Text],
    -- | The launch purpose.
    launchPurpose :: Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text,
    -- | The platform.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileInitialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'getLaunchProfileInitialization_studioId' - The studio ID.
--
-- 'launchProfileProtocolVersions', 'getLaunchProfileInitialization_launchProfileProtocolVersions' - A collection of launch profile protocol versions.
--
-- 'launchPurpose', 'getLaunchProfileInitialization_launchPurpose' - The launch purpose.
--
-- 'launchProfileId', 'getLaunchProfileInitialization_launchProfileId' - The launch profile ID.
--
-- 'platform', 'getLaunchProfileInitialization_platform' - The platform.
newGetLaunchProfileInitialization ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'launchPurpose'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'platform'
  Prelude.Text ->
  GetLaunchProfileInitialization
newGetLaunchProfileInitialization
  pStudioId_
  pLaunchPurpose_
  pLaunchProfileId_
  pPlatform_ =
    GetLaunchProfileInitialization'
      { studioId =
          pStudioId_,
        launchProfileProtocolVersions =
          Prelude.mempty,
        launchPurpose = pLaunchPurpose_,
        launchProfileId = pLaunchProfileId_,
        platform = pPlatform_
      }

-- | The studio ID.
getLaunchProfileInitialization_studioId :: Lens.Lens' GetLaunchProfileInitialization Prelude.Text
getLaunchProfileInitialization_studioId = Lens.lens (\GetLaunchProfileInitialization' {studioId} -> studioId) (\s@GetLaunchProfileInitialization' {} a -> s {studioId = a} :: GetLaunchProfileInitialization)

-- | A collection of launch profile protocol versions.
getLaunchProfileInitialization_launchProfileProtocolVersions :: Lens.Lens' GetLaunchProfileInitialization [Prelude.Text]
getLaunchProfileInitialization_launchProfileProtocolVersions = Lens.lens (\GetLaunchProfileInitialization' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@GetLaunchProfileInitialization' {} a -> s {launchProfileProtocolVersions = a} :: GetLaunchProfileInitialization) Prelude.. Lens.coerced

-- | The launch purpose.
getLaunchProfileInitialization_launchPurpose :: Lens.Lens' GetLaunchProfileInitialization Prelude.Text
getLaunchProfileInitialization_launchPurpose = Lens.lens (\GetLaunchProfileInitialization' {launchPurpose} -> launchPurpose) (\s@GetLaunchProfileInitialization' {} a -> s {launchPurpose = a} :: GetLaunchProfileInitialization)

-- | The launch profile ID.
getLaunchProfileInitialization_launchProfileId :: Lens.Lens' GetLaunchProfileInitialization Prelude.Text
getLaunchProfileInitialization_launchProfileId = Lens.lens (\GetLaunchProfileInitialization' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfileInitialization' {} a -> s {launchProfileId = a} :: GetLaunchProfileInitialization)

-- | The platform.
getLaunchProfileInitialization_platform :: Lens.Lens' GetLaunchProfileInitialization Prelude.Text
getLaunchProfileInitialization_platform = Lens.lens (\GetLaunchProfileInitialization' {platform} -> platform) (\s@GetLaunchProfileInitialization' {} a -> s {platform = a} :: GetLaunchProfileInitialization)

instance
  Core.AWSRequest
    GetLaunchProfileInitialization
  where
  type
    AWSResponse GetLaunchProfileInitialization =
      GetLaunchProfileInitializationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileInitializationResponse'
            Prelude.<$> (x Core..?> "launchProfileInitialization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetLaunchProfileInitialization
  where
  hashWithSalt
    _salt
    GetLaunchProfileInitialization' {..} =
      _salt `Prelude.hashWithSalt` studioId
        `Prelude.hashWithSalt` launchProfileProtocolVersions
        `Prelude.hashWithSalt` launchPurpose
        `Prelude.hashWithSalt` launchProfileId
        `Prelude.hashWithSalt` platform

instance
  Prelude.NFData
    GetLaunchProfileInitialization
  where
  rnf GetLaunchProfileInitialization' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf launchPurpose
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf platform

instance
  Core.ToHeaders
    GetLaunchProfileInitialization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLaunchProfileInitialization where
  toPath GetLaunchProfileInitialization' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId,
        "/init"
      ]

instance Core.ToQuery GetLaunchProfileInitialization where
  toQuery GetLaunchProfileInitialization' {..} =
    Prelude.mconcat
      [ "launchProfileProtocolVersions"
          Core.=: Core.toQueryList
            "member"
            launchProfileProtocolVersions,
        "launchPurpose" Core.=: launchPurpose,
        "platform" Core.=: platform
      ]

-- | /See:/ 'newGetLaunchProfileInitializationResponse' smart constructor.
data GetLaunchProfileInitializationResponse = GetLaunchProfileInitializationResponse'
  { -- | The launch profile initialization.
    launchProfileInitialization :: Prelude.Maybe LaunchProfileInitialization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileInitializationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfileInitialization', 'getLaunchProfileInitializationResponse_launchProfileInitialization' - The launch profile initialization.
--
-- 'httpStatus', 'getLaunchProfileInitializationResponse_httpStatus' - The response's http status code.
newGetLaunchProfileInitializationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchProfileInitializationResponse
newGetLaunchProfileInitializationResponse
  pHttpStatus_ =
    GetLaunchProfileInitializationResponse'
      { launchProfileInitialization =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The launch profile initialization.
getLaunchProfileInitializationResponse_launchProfileInitialization :: Lens.Lens' GetLaunchProfileInitializationResponse (Prelude.Maybe LaunchProfileInitialization)
getLaunchProfileInitializationResponse_launchProfileInitialization = Lens.lens (\GetLaunchProfileInitializationResponse' {launchProfileInitialization} -> launchProfileInitialization) (\s@GetLaunchProfileInitializationResponse' {} a -> s {launchProfileInitialization = a} :: GetLaunchProfileInitializationResponse)

-- | The response's http status code.
getLaunchProfileInitializationResponse_httpStatus :: Lens.Lens' GetLaunchProfileInitializationResponse Prelude.Int
getLaunchProfileInitializationResponse_httpStatus = Lens.lens (\GetLaunchProfileInitializationResponse' {httpStatus} -> httpStatus) (\s@GetLaunchProfileInitializationResponse' {} a -> s {httpStatus = a} :: GetLaunchProfileInitializationResponse)

instance
  Prelude.NFData
    GetLaunchProfileInitializationResponse
  where
  rnf GetLaunchProfileInitializationResponse' {..} =
    Prelude.rnf launchProfileInitialization
      `Prelude.seq` Prelude.rnf httpStatus
