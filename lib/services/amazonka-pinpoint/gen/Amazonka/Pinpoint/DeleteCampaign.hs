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
-- Module      : Amazonka.Pinpoint.DeleteCampaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a campaign from an application.
module Amazonka.Pinpoint.DeleteCampaign
  ( -- * Creating a Request
    DeleteCampaign (..),
    newDeleteCampaign,

    -- * Request Lenses
    deleteCampaign_campaignId,
    deleteCampaign_applicationId,

    -- * Destructuring the Response
    DeleteCampaignResponse (..),
    newDeleteCampaignResponse,

    -- * Response Lenses
    deleteCampaignResponse_httpStatus,
    deleteCampaignResponse_campaignResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignId', 'deleteCampaign_campaignId' - The unique identifier for the campaign.
--
-- 'applicationId', 'deleteCampaign_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteCampaign ::
  -- | 'campaignId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  DeleteCampaign
newDeleteCampaign pCampaignId_ pApplicationId_ =
  DeleteCampaign'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
deleteCampaign_campaignId :: Lens.Lens' DeleteCampaign Prelude.Text
deleteCampaign_campaignId = Lens.lens (\DeleteCampaign' {campaignId} -> campaignId) (\s@DeleteCampaign' {} a -> s {campaignId = a} :: DeleteCampaign)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteCampaign_applicationId :: Lens.Lens' DeleteCampaign Prelude.Text
deleteCampaign_applicationId = Lens.lens (\DeleteCampaign' {applicationId} -> applicationId) (\s@DeleteCampaign' {} a -> s {applicationId = a} :: DeleteCampaign)

instance Core.AWSRequest DeleteCampaign where
  type
    AWSResponse DeleteCampaign =
      DeleteCampaignResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCampaignResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteCampaign where
  hashWithSalt _salt DeleteCampaign' {..} =
    _salt
      `Prelude.hashWithSalt` campaignId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteCampaign where
  rnf DeleteCampaign' {..} =
    Prelude.rnf campaignId `Prelude.seq`
      Prelude.rnf applicationId

instance Data.ToHeaders DeleteCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCampaign where
  toPath DeleteCampaign' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/campaigns/",
        Data.toBS campaignId
      ]

instance Data.ToQuery DeleteCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignResponse :: CampaignResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCampaignResponse_httpStatus' - The response's http status code.
--
-- 'campaignResponse', 'deleteCampaignResponse_campaignResponse' - Undocumented member.
newDeleteCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  DeleteCampaignResponse
newDeleteCampaignResponse
  pHttpStatus_
  pCampaignResponse_ =
    DeleteCampaignResponse'
      { httpStatus = pHttpStatus_,
        campaignResponse = pCampaignResponse_
      }

-- | The response's http status code.
deleteCampaignResponse_httpStatus :: Lens.Lens' DeleteCampaignResponse Prelude.Int
deleteCampaignResponse_httpStatus = Lens.lens (\DeleteCampaignResponse' {httpStatus} -> httpStatus) (\s@DeleteCampaignResponse' {} a -> s {httpStatus = a} :: DeleteCampaignResponse)

-- | Undocumented member.
deleteCampaignResponse_campaignResponse :: Lens.Lens' DeleteCampaignResponse CampaignResponse
deleteCampaignResponse_campaignResponse = Lens.lens (\DeleteCampaignResponse' {campaignResponse} -> campaignResponse) (\s@DeleteCampaignResponse' {} a -> s {campaignResponse = a} :: DeleteCampaignResponse)

instance Prelude.NFData DeleteCampaignResponse where
  rnf DeleteCampaignResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf campaignResponse
