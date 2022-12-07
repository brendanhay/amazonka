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
-- Module      : Amazonka.Personalize.UpdateCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a campaign by either deploying a new solution or changing the
-- value of the campaign\'s @minProvisionedTPS@ parameter.
--
-- To update a campaign, the campaign status must be ACTIVE or CREATE
-- FAILED. Check the campaign status using the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeCampaign.html DescribeCampaign>
-- operation.
--
-- You can still get recommendations from a campaign while an update is in
-- progress. The campaign will use the previous solution version and
-- campaign configuration to generate recommendations until the latest
-- campaign update status is @Active@.
--
-- For more information on campaigns, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
module Amazonka.Personalize.UpdateCampaign
  ( -- * Creating a Request
    UpdateCampaign (..),
    newUpdateCampaign,

    -- * Request Lenses
    updateCampaign_campaignConfig,
    updateCampaign_minProvisionedTPS,
    updateCampaign_solutionVersionArn,
    updateCampaign_campaignArn,

    -- * Destructuring the Response
    UpdateCampaignResponse (..),
    newUpdateCampaignResponse,

    -- * Response Lenses
    updateCampaignResponse_campaignArn,
    updateCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { -- | The configuration details of a campaign.
    campaignConfig :: Prelude.Maybe CampaignConfig,
    -- | Specifies the requested minimum provisioned transactions
    -- (recommendations) per second that Amazon Personalize will support.
    minProvisionedTPS :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of a new solution version to deploy.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the campaign.
    campaignArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignConfig', 'updateCampaign_campaignConfig' - The configuration details of a campaign.
--
-- 'minProvisionedTPS', 'updateCampaign_minProvisionedTPS' - Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
--
-- 'solutionVersionArn', 'updateCampaign_solutionVersionArn' - The ARN of a new solution version to deploy.
--
-- 'campaignArn', 'updateCampaign_campaignArn' - The Amazon Resource Name (ARN) of the campaign.
newUpdateCampaign ::
  -- | 'campaignArn'
  Prelude.Text ->
  UpdateCampaign
newUpdateCampaign pCampaignArn_ =
  UpdateCampaign'
    { campaignConfig = Prelude.Nothing,
      minProvisionedTPS = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      campaignArn = pCampaignArn_
    }

-- | The configuration details of a campaign.
updateCampaign_campaignConfig :: Lens.Lens' UpdateCampaign (Prelude.Maybe CampaignConfig)
updateCampaign_campaignConfig = Lens.lens (\UpdateCampaign' {campaignConfig} -> campaignConfig) (\s@UpdateCampaign' {} a -> s {campaignConfig = a} :: UpdateCampaign)

-- | Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
updateCampaign_minProvisionedTPS :: Lens.Lens' UpdateCampaign (Prelude.Maybe Prelude.Natural)
updateCampaign_minProvisionedTPS = Lens.lens (\UpdateCampaign' {minProvisionedTPS} -> minProvisionedTPS) (\s@UpdateCampaign' {} a -> s {minProvisionedTPS = a} :: UpdateCampaign)

-- | The ARN of a new solution version to deploy.
updateCampaign_solutionVersionArn :: Lens.Lens' UpdateCampaign (Prelude.Maybe Prelude.Text)
updateCampaign_solutionVersionArn = Lens.lens (\UpdateCampaign' {solutionVersionArn} -> solutionVersionArn) (\s@UpdateCampaign' {} a -> s {solutionVersionArn = a} :: UpdateCampaign)

-- | The Amazon Resource Name (ARN) of the campaign.
updateCampaign_campaignArn :: Lens.Lens' UpdateCampaign Prelude.Text
updateCampaign_campaignArn = Lens.lens (\UpdateCampaign' {campaignArn} -> campaignArn) (\s@UpdateCampaign' {} a -> s {campaignArn = a} :: UpdateCampaign)

instance Core.AWSRequest UpdateCampaign where
  type
    AWSResponse UpdateCampaign =
      UpdateCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCampaignResponse'
            Prelude.<$> (x Data..?> "campaignArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCampaign where
  hashWithSalt _salt UpdateCampaign' {..} =
    _salt `Prelude.hashWithSalt` campaignConfig
      `Prelude.hashWithSalt` minProvisionedTPS
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` campaignArn

instance Prelude.NFData UpdateCampaign where
  rnf UpdateCampaign' {..} =
    Prelude.rnf campaignConfig
      `Prelude.seq` Prelude.rnf minProvisionedTPS
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf campaignArn

instance Data.ToHeaders UpdateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.UpdateCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCampaign where
  toJSON UpdateCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("campaignConfig" Data..=)
              Prelude.<$> campaignConfig,
            ("minProvisionedTPS" Data..=)
              Prelude.<$> minProvisionedTPS,
            ("solutionVersionArn" Data..=)
              Prelude.<$> solutionVersionArn,
            Prelude.Just ("campaignArn" Data..= campaignArn)
          ]
      )

instance Data.ToPath UpdateCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { -- | The same campaign ARN as given in the request.
    campaignArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignArn', 'updateCampaignResponse_campaignArn' - The same campaign ARN as given in the request.
--
-- 'httpStatus', 'updateCampaignResponse_httpStatus' - The response's http status code.
newUpdateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCampaignResponse
newUpdateCampaignResponse pHttpStatus_ =
  UpdateCampaignResponse'
    { campaignArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The same campaign ARN as given in the request.
updateCampaignResponse_campaignArn :: Lens.Lens' UpdateCampaignResponse (Prelude.Maybe Prelude.Text)
updateCampaignResponse_campaignArn = Lens.lens (\UpdateCampaignResponse' {campaignArn} -> campaignArn) (\s@UpdateCampaignResponse' {} a -> s {campaignArn = a} :: UpdateCampaignResponse)

-- | The response's http status code.
updateCampaignResponse_httpStatus :: Lens.Lens' UpdateCampaignResponse Prelude.Int
updateCampaignResponse_httpStatus = Lens.lens (\UpdateCampaignResponse' {httpStatus} -> httpStatus) (\s@UpdateCampaignResponse' {} a -> s {httpStatus = a} :: UpdateCampaignResponse)

instance Prelude.NFData UpdateCampaignResponse where
  rnf UpdateCampaignResponse' {..} =
    Prelude.rnf campaignArn
      `Prelude.seq` Prelude.rnf httpStatus
