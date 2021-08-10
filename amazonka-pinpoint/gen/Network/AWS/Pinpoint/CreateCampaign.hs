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
-- Module      : Network.AWS.Pinpoint.CreateCampaign
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new campaign for an application or updates the settings of an
-- existing campaign for an application.
module Network.AWS.Pinpoint.CreateCampaign
  ( -- * Creating a Request
    CreateCampaign (..),
    newCreateCampaign,

    -- * Request Lenses
    createCampaign_applicationId,
    createCampaign_writeCampaignRequest,

    -- * Destructuring the Response
    CreateCampaignResponse (..),
    newCreateCampaignResponse,

    -- * Response Lenses
    createCampaignResponse_httpStatus,
    createCampaignResponse_campaignResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeCampaignRequest :: WriteCampaignRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createCampaign_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeCampaignRequest', 'createCampaign_writeCampaignRequest' - Undocumented member.
newCreateCampaign ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeCampaignRequest'
  WriteCampaignRequest ->
  CreateCampaign
newCreateCampaign
  pApplicationId_
  pWriteCampaignRequest_ =
    CreateCampaign'
      { applicationId = pApplicationId_,
        writeCampaignRequest = pWriteCampaignRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
createCampaign_applicationId :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_applicationId = Lens.lens (\CreateCampaign' {applicationId} -> applicationId) (\s@CreateCampaign' {} a -> s {applicationId = a} :: CreateCampaign)

-- | Undocumented member.
createCampaign_writeCampaignRequest :: Lens.Lens' CreateCampaign WriteCampaignRequest
createCampaign_writeCampaignRequest = Lens.lens (\CreateCampaign' {writeCampaignRequest} -> writeCampaignRequest) (\s@CreateCampaign' {} a -> s {writeCampaignRequest = a} :: CreateCampaign)

instance Core.AWSRequest CreateCampaign where
  type
    AWSResponse CreateCampaign =
      CreateCampaignResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCampaignResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateCampaign

instance Prelude.NFData CreateCampaign

instance Core.ToHeaders CreateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "WriteCampaignRequest"
                  Core..= writeCampaignRequest
              )
          ]
      )

instance Core.ToPath CreateCampaign where
  toPath CreateCampaign' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/campaigns"]

instance Core.ToQuery CreateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCampaignResponse_httpStatus' - The response's http status code.
--
-- 'campaignResponse', 'createCampaignResponse_campaignResponse' - Undocumented member.
newCreateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  CreateCampaignResponse
newCreateCampaignResponse
  pHttpStatus_
  pCampaignResponse_ =
    CreateCampaignResponse'
      { httpStatus = pHttpStatus_,
        campaignResponse = pCampaignResponse_
      }

-- | The response's http status code.
createCampaignResponse_httpStatus :: Lens.Lens' CreateCampaignResponse Prelude.Int
createCampaignResponse_httpStatus = Lens.lens (\CreateCampaignResponse' {httpStatus} -> httpStatus) (\s@CreateCampaignResponse' {} a -> s {httpStatus = a} :: CreateCampaignResponse)

-- | Undocumented member.
createCampaignResponse_campaignResponse :: Lens.Lens' CreateCampaignResponse CampaignResponse
createCampaignResponse_campaignResponse = Lens.lens (\CreateCampaignResponse' {campaignResponse} -> campaignResponse) (\s@CreateCampaignResponse' {} a -> s {campaignResponse = a} :: CreateCampaignResponse)

instance Prelude.NFData CreateCampaignResponse
