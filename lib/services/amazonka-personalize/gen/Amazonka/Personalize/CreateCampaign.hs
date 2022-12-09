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
-- Module      : Amazonka.Personalize.CreateCampaign
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a campaign that deploys a solution version. When a client calls
-- the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- and
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetPersonalizedRanking.html GetPersonalizedRanking>
-- APIs, a campaign is specified in the request.
--
-- __Minimum Provisioned TPS and Auto-Scaling__
--
-- A transaction is a single @GetRecommendations@ or
-- @GetPersonalizedRanking@ call. Transactions per second (TPS) is the
-- throughput and unit of billing for Amazon Personalize. The minimum
-- provisioned TPS (@minProvisionedTPS@) specifies the baseline throughput
-- provisioned by Amazon Personalize, and thus, the minimum billing charge.
--
-- If your TPS increases beyond @minProvisionedTPS@, Amazon Personalize
-- auto-scales the provisioned capacity up and down, but never below
-- @minProvisionedTPS@. There\'s a short time delay while the capacity is
-- increased that might cause loss of transactions.
--
-- The actual TPS used is calculated as the average requests\/second within
-- a 5-minute window. You pay for maximum of either the minimum provisioned
-- TPS or the actual TPS. We recommend starting with a low
-- @minProvisionedTPS@, track your usage using Amazon CloudWatch metrics,
-- and then increase the @minProvisionedTPS@ as necessary.
--
-- __Status__
--
-- A campaign can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- To get the campaign status, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeCampaign.html DescribeCampaign>.
--
-- Wait until the @status@ of the campaign is @ACTIVE@ before asking the
-- campaign for recommendations.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListCampaigns.html ListCampaigns>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeCampaign.html DescribeCampaign>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_UpdateCampaign.html UpdateCampaign>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteCampaign.html DeleteCampaign>
module Amazonka.Personalize.CreateCampaign
  ( -- * Creating a Request
    CreateCampaign (..),
    newCreateCampaign,

    -- * Request Lenses
    createCampaign_campaignConfig,
    createCampaign_minProvisionedTPS,
    createCampaign_tags,
    createCampaign_name,
    createCampaign_solutionVersionArn,

    -- * Destructuring the Response
    CreateCampaignResponse (..),
    newCreateCampaignResponse,

    -- * Response Lenses
    createCampaignResponse_campaignArn,
    createCampaignResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { -- | The configuration details of a campaign.
    campaignConfig :: Prelude.Maybe CampaignConfig,
    -- | Specifies the requested minimum provisioned transactions
    -- (recommendations) per second that Amazon Personalize will support.
    minProvisionedTPS :: Prelude.Maybe Prelude.Natural,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the campaign.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the new campaign. The campaign name must be unique within
    -- your account.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution version to deploy.
    solutionVersionArn :: Prelude.Text
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
-- 'campaignConfig', 'createCampaign_campaignConfig' - The configuration details of a campaign.
--
-- 'minProvisionedTPS', 'createCampaign_minProvisionedTPS' - Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
--
-- 'tags', 'createCampaign_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the campaign.
--
-- 'name', 'createCampaign_name' - A name for the new campaign. The campaign name must be unique within
-- your account.
--
-- 'solutionVersionArn', 'createCampaign_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version to deploy.
newCreateCampaign ::
  -- | 'name'
  Prelude.Text ->
  -- | 'solutionVersionArn'
  Prelude.Text ->
  CreateCampaign
newCreateCampaign pName_ pSolutionVersionArn_ =
  CreateCampaign'
    { campaignConfig = Prelude.Nothing,
      minProvisionedTPS = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      solutionVersionArn = pSolutionVersionArn_
    }

-- | The configuration details of a campaign.
createCampaign_campaignConfig :: Lens.Lens' CreateCampaign (Prelude.Maybe CampaignConfig)
createCampaign_campaignConfig = Lens.lens (\CreateCampaign' {campaignConfig} -> campaignConfig) (\s@CreateCampaign' {} a -> s {campaignConfig = a} :: CreateCampaign)

-- | Specifies the requested minimum provisioned transactions
-- (recommendations) per second that Amazon Personalize will support.
createCampaign_minProvisionedTPS :: Lens.Lens' CreateCampaign (Prelude.Maybe Prelude.Natural)
createCampaign_minProvisionedTPS = Lens.lens (\CreateCampaign' {minProvisionedTPS} -> minProvisionedTPS) (\s@CreateCampaign' {} a -> s {minProvisionedTPS = a} :: CreateCampaign)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the campaign.
createCampaign_tags :: Lens.Lens' CreateCampaign (Prelude.Maybe [Tag])
createCampaign_tags = Lens.lens (\CreateCampaign' {tags} -> tags) (\s@CreateCampaign' {} a -> s {tags = a} :: CreateCampaign) Prelude.. Lens.mapping Lens.coerced

-- | A name for the new campaign. The campaign name must be unique within
-- your account.
createCampaign_name :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_name = Lens.lens (\CreateCampaign' {name} -> name) (\s@CreateCampaign' {} a -> s {name = a} :: CreateCampaign)

-- | The Amazon Resource Name (ARN) of the solution version to deploy.
createCampaign_solutionVersionArn :: Lens.Lens' CreateCampaign Prelude.Text
createCampaign_solutionVersionArn = Lens.lens (\CreateCampaign' {solutionVersionArn} -> solutionVersionArn) (\s@CreateCampaign' {} a -> s {solutionVersionArn = a} :: CreateCampaign)

instance Core.AWSRequest CreateCampaign where
  type
    AWSResponse CreateCampaign =
      CreateCampaignResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCampaignResponse'
            Prelude.<$> (x Data..?> "campaignArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCampaign where
  hashWithSalt _salt CreateCampaign' {..} =
    _salt `Prelude.hashWithSalt` campaignConfig
      `Prelude.hashWithSalt` minProvisionedTPS
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData CreateCampaign where
  rnf CreateCampaign' {..} =
    Prelude.rnf campaignConfig
      `Prelude.seq` Prelude.rnf minProvisionedTPS
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf solutionVersionArn

instance Data.ToHeaders CreateCampaign where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateCampaign" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("campaignConfig" Data..=)
              Prelude.<$> campaignConfig,
            ("minProvisionedTPS" Data..=)
              Prelude.<$> minProvisionedTPS,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("solutionVersionArn" Data..= solutionVersionArn)
          ]
      )

instance Data.ToPath CreateCampaign where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCampaign where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { -- | The Amazon Resource Name (ARN) of the campaign.
    campaignArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'campaignArn', 'createCampaignResponse_campaignArn' - The Amazon Resource Name (ARN) of the campaign.
--
-- 'httpStatus', 'createCampaignResponse_httpStatus' - The response's http status code.
newCreateCampaignResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCampaignResponse
newCreateCampaignResponse pHttpStatus_ =
  CreateCampaignResponse'
    { campaignArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the campaign.
createCampaignResponse_campaignArn :: Lens.Lens' CreateCampaignResponse (Prelude.Maybe Prelude.Text)
createCampaignResponse_campaignArn = Lens.lens (\CreateCampaignResponse' {campaignArn} -> campaignArn) (\s@CreateCampaignResponse' {} a -> s {campaignArn = a} :: CreateCampaignResponse)

-- | The response's http status code.
createCampaignResponse_httpStatus :: Lens.Lens' CreateCampaignResponse Prelude.Int
createCampaignResponse_httpStatus = Lens.lens (\CreateCampaignResponse' {httpStatus} -> httpStatus) (\s@CreateCampaignResponse' {} a -> s {httpStatus = a} :: CreateCampaignResponse)

instance Prelude.NFData CreateCampaignResponse where
  rnf CreateCampaignResponse' {..} =
    Prelude.rnf campaignArn
      `Prelude.seq` Prelude.rnf httpStatus
