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
-- Module      : Amazonka.Pinpoint.GetCampaignVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for a specific version of a campaign.
module Amazonka.Pinpoint.GetCampaignVersion
  ( -- * Creating a Request
    GetCampaignVersion (..),
    newGetCampaignVersion,

    -- * Request Lenses
    getCampaignVersion_version,
    getCampaignVersion_applicationId,
    getCampaignVersion_campaignId,

    -- * Destructuring the Response
    GetCampaignVersionResponse (..),
    newGetCampaignVersionResponse,

    -- * Response Lenses
    getCampaignVersionResponse_httpStatus,
    getCampaignVersionResponse_campaignResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { -- | The unique version number (Version property) for the campaign version.
    version :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getCampaignVersion_version' - The unique version number (Version property) for the campaign version.
--
-- 'applicationId', 'getCampaignVersion_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'campaignId', 'getCampaignVersion_campaignId' - The unique identifier for the campaign.
newGetCampaignVersion ::
  -- | 'version'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'campaignId'
  Prelude.Text ->
  GetCampaignVersion
newGetCampaignVersion
  pVersion_
  pApplicationId_
  pCampaignId_ =
    GetCampaignVersion'
      { version = pVersion_,
        applicationId = pApplicationId_,
        campaignId = pCampaignId_
      }

-- | The unique version number (Version property) for the campaign version.
getCampaignVersion_version :: Lens.Lens' GetCampaignVersion Prelude.Text
getCampaignVersion_version = Lens.lens (\GetCampaignVersion' {version} -> version) (\s@GetCampaignVersion' {} a -> s {version = a} :: GetCampaignVersion)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaignVersion_applicationId :: Lens.Lens' GetCampaignVersion Prelude.Text
getCampaignVersion_applicationId = Lens.lens (\GetCampaignVersion' {applicationId} -> applicationId) (\s@GetCampaignVersion' {} a -> s {applicationId = a} :: GetCampaignVersion)

-- | The unique identifier for the campaign.
getCampaignVersion_campaignId :: Lens.Lens' GetCampaignVersion Prelude.Text
getCampaignVersion_campaignId = Lens.lens (\GetCampaignVersion' {campaignId} -> campaignId) (\s@GetCampaignVersion' {} a -> s {campaignId = a} :: GetCampaignVersion)

instance Core.AWSRequest GetCampaignVersion where
  type
    AWSResponse GetCampaignVersion =
      GetCampaignVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetCampaignVersion where
  hashWithSalt _salt GetCampaignVersion' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` campaignId

instance Prelude.NFData GetCampaignVersion where
  rnf GetCampaignVersion' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf campaignId

instance Data.ToHeaders GetCampaignVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCampaignVersion where
  toPath GetCampaignVersion' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/campaigns/",
        Data.toBS campaignId,
        "/versions/",
        Data.toBS version
      ]

instance Data.ToQuery GetCampaignVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignVersionResponse_httpStatus' - The response's http status code.
--
-- 'campaignResponse', 'getCampaignVersionResponse_campaignResponse' - Undocumented member.
newGetCampaignVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  GetCampaignVersionResponse
newGetCampaignVersionResponse
  pHttpStatus_
  pCampaignResponse_ =
    GetCampaignVersionResponse'
      { httpStatus =
          pHttpStatus_,
        campaignResponse = pCampaignResponse_
      }

-- | The response's http status code.
getCampaignVersionResponse_httpStatus :: Lens.Lens' GetCampaignVersionResponse Prelude.Int
getCampaignVersionResponse_httpStatus = Lens.lens (\GetCampaignVersionResponse' {httpStatus} -> httpStatus) (\s@GetCampaignVersionResponse' {} a -> s {httpStatus = a} :: GetCampaignVersionResponse)

-- | Undocumented member.
getCampaignVersionResponse_campaignResponse :: Lens.Lens' GetCampaignVersionResponse CampaignResponse
getCampaignVersionResponse_campaignResponse = Lens.lens (\GetCampaignVersionResponse' {campaignResponse} -> campaignResponse) (\s@GetCampaignVersionResponse' {} a -> s {campaignResponse = a} :: GetCampaignVersionResponse)

instance Prelude.NFData GetCampaignVersionResponse where
  rnf GetCampaignVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf campaignResponse
