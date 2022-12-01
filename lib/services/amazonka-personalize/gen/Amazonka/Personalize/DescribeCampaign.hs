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
-- Module      : Amazonka.Personalize.DescribeCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given campaign, including its status.
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- When the @status@ is @CREATE FAILED@, the response includes the
-- @failureReason@ key, which describes why.
--
-- For more information on campaigns, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
module Amazonka.Personalize.DescribeCampaign
  ( -- * Creating a Request
    DescribeCampaign (..),
    newDescribeCampaign,

    -- * Request Lenses
    describeCampaign_campaignArn,

    -- * Destructuring the Response
    DescribeCampaignResponse (..),
    newDescribeCampaignResponse,

    -- * Response Lenses
    describeCampaignResponse_campaign,
    describeCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCampaign' smart constructor.
data DescribeCampaign = DescribeCampaign'
  { -- | The Amazon Resource Name (ARN) of the campaign.
    campaignArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCampaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignArn', 'describeCampaign_campaignArn' - The Amazon Resource Name (ARN) of the campaign.
newDescribeCampaign ::
  -- | 'campaignArn'
  Prelude.Text ->
  DescribeCampaign
newDescribeCampaign pCampaignArn_ =
  DescribeCampaign' {campaignArn = pCampaignArn_}

-- | The Amazon Resource Name (ARN) of the campaign.
describeCampaign_campaignArn :: Lens.Lens' DescribeCampaign Prelude.Text
describeCampaign_campaignArn = Lens.lens (\DescribeCampaign' {campaignArn} -> campaignArn) (\s@DescribeCampaign' {} a -> s {campaignArn = a} :: DescribeCampaign)

instance Core.AWSRequest DescribeCampaign where
  type
    AWSResponse DescribeCampaign =
      DescribeCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCampaignResponse'
            Prelude.<$> (x Core..?> "campaign")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCampaign where
  hashWithSalt _salt DescribeCampaign' {..} =
    _salt `Prelude.hashWithSalt` campaignArn

instance Prelude.NFData DescribeCampaign where
  rnf DescribeCampaign' {..} = Prelude.rnf campaignArn

instance Core.ToHeaders DescribeCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.DescribeCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCampaign where
  toJSON DescribeCampaign' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("campaignArn" Core..= campaignArn)]
      )

instance Core.ToPath DescribeCampaign where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCampaignResponse' smart constructor.
data DescribeCampaignResponse = DescribeCampaignResponse'
  { -- | The properties of the campaign.
    campaign :: Prelude.Maybe Campaign,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCampaignResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaign', 'describeCampaignResponse_campaign' - The properties of the campaign.
--
-- 'httpStatus', 'describeCampaignResponse_httpStatus' - The response's http status code.
newDescribeCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCampaignResponse
newDescribeCampaignResponse pHttpStatus_ =
  DescribeCampaignResponse'
    { campaign =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the campaign.
describeCampaignResponse_campaign :: Lens.Lens' DescribeCampaignResponse (Prelude.Maybe Campaign)
describeCampaignResponse_campaign = Lens.lens (\DescribeCampaignResponse' {campaign} -> campaign) (\s@DescribeCampaignResponse' {} a -> s {campaign = a} :: DescribeCampaignResponse)

-- | The response's http status code.
describeCampaignResponse_httpStatus :: Lens.Lens' DescribeCampaignResponse Prelude.Int
describeCampaignResponse_httpStatus = Lens.lens (\DescribeCampaignResponse' {httpStatus} -> httpStatus) (\s@DescribeCampaignResponse' {} a -> s {httpStatus = a} :: DescribeCampaignResponse)

instance Prelude.NFData DescribeCampaignResponse where
  rnf DescribeCampaignResponse' {..} =
    Prelude.rnf campaign
      `Prelude.seq` Prelude.rnf httpStatus
