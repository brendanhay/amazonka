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
-- Module      : Amazonka.ApplicationInsights.DescribeComponentConfigurationRecommendation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the recommended monitoring configuration of the component.
module Amazonka.ApplicationInsights.DescribeComponentConfigurationRecommendation
  ( -- * Creating a Request
    DescribeComponentConfigurationRecommendation (..),
    newDescribeComponentConfigurationRecommendation,

    -- * Request Lenses
    describeComponentConfigurationRecommendation_resourceGroupName,
    describeComponentConfigurationRecommendation_componentName,
    describeComponentConfigurationRecommendation_tier,

    -- * Destructuring the Response
    DescribeComponentConfigurationRecommendationResponse (..),
    newDescribeComponentConfigurationRecommendationResponse,

    -- * Response Lenses
    describeComponentConfigurationRecommendationResponse_componentConfiguration,
    describeComponentConfigurationRecommendationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeComponentConfigurationRecommendation' smart constructor.
data DescribeComponentConfigurationRecommendation = DescribeComponentConfigurationRecommendation'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Text,
    -- | The tier of the application component.
    tier :: Tier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponentConfigurationRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'describeComponentConfigurationRecommendation_resourceGroupName' - The name of the resource group.
--
-- 'componentName', 'describeComponentConfigurationRecommendation_componentName' - The name of the component.
--
-- 'tier', 'describeComponentConfigurationRecommendation_tier' - The tier of the application component.
newDescribeComponentConfigurationRecommendation ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'componentName'
  Prelude.Text ->
  -- | 'tier'
  Tier ->
  DescribeComponentConfigurationRecommendation
newDescribeComponentConfigurationRecommendation
  pResourceGroupName_
  pComponentName_
  pTier_ =
    DescribeComponentConfigurationRecommendation'
      { resourceGroupName =
          pResourceGroupName_,
        componentName =
          pComponentName_,
        tier = pTier_
      }

-- | The name of the resource group.
describeComponentConfigurationRecommendation_resourceGroupName :: Lens.Lens' DescribeComponentConfigurationRecommendation Prelude.Text
describeComponentConfigurationRecommendation_resourceGroupName = Lens.lens (\DescribeComponentConfigurationRecommendation' {resourceGroupName} -> resourceGroupName) (\s@DescribeComponentConfigurationRecommendation' {} a -> s {resourceGroupName = a} :: DescribeComponentConfigurationRecommendation)

-- | The name of the component.
describeComponentConfigurationRecommendation_componentName :: Lens.Lens' DescribeComponentConfigurationRecommendation Prelude.Text
describeComponentConfigurationRecommendation_componentName = Lens.lens (\DescribeComponentConfigurationRecommendation' {componentName} -> componentName) (\s@DescribeComponentConfigurationRecommendation' {} a -> s {componentName = a} :: DescribeComponentConfigurationRecommendation)

-- | The tier of the application component.
describeComponentConfigurationRecommendation_tier :: Lens.Lens' DescribeComponentConfigurationRecommendation Tier
describeComponentConfigurationRecommendation_tier = Lens.lens (\DescribeComponentConfigurationRecommendation' {tier} -> tier) (\s@DescribeComponentConfigurationRecommendation' {} a -> s {tier = a} :: DescribeComponentConfigurationRecommendation)

instance
  Core.AWSRequest
    DescribeComponentConfigurationRecommendation
  where
  type
    AWSResponse
      DescribeComponentConfigurationRecommendation =
      DescribeComponentConfigurationRecommendationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComponentConfigurationRecommendationResponse'
            Prelude.<$> (x Data..?> "ComponentConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeComponentConfigurationRecommendation
  where
  hashWithSalt
    _salt
    DescribeComponentConfigurationRecommendation' {..} =
      _salt `Prelude.hashWithSalt` resourceGroupName
        `Prelude.hashWithSalt` componentName
        `Prelude.hashWithSalt` tier

instance
  Prelude.NFData
    DescribeComponentConfigurationRecommendation
  where
  rnf DescribeComponentConfigurationRecommendation' {..} =
    Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf tier

instance
  Data.ToHeaders
    DescribeComponentConfigurationRecommendation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DescribeComponentConfigurationRecommendation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeComponentConfigurationRecommendation
  where
  toJSON
    DescribeComponentConfigurationRecommendation' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("ResourceGroupName" Data..= resourceGroupName),
              Prelude.Just ("ComponentName" Data..= componentName),
              Prelude.Just ("Tier" Data..= tier)
            ]
        )

instance
  Data.ToPath
    DescribeComponentConfigurationRecommendation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeComponentConfigurationRecommendation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeComponentConfigurationRecommendationResponse' smart constructor.
data DescribeComponentConfigurationRecommendationResponse = DescribeComponentConfigurationRecommendationResponse'
  { -- | The recommended configuration settings of the component. The value is
    -- the escaped JSON of the configuration.
    componentConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponentConfigurationRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentConfiguration', 'describeComponentConfigurationRecommendationResponse_componentConfiguration' - The recommended configuration settings of the component. The value is
-- the escaped JSON of the configuration.
--
-- 'httpStatus', 'describeComponentConfigurationRecommendationResponse_httpStatus' - The response's http status code.
newDescribeComponentConfigurationRecommendationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComponentConfigurationRecommendationResponse
newDescribeComponentConfigurationRecommendationResponse
  pHttpStatus_ =
    DescribeComponentConfigurationRecommendationResponse'
      { componentConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The recommended configuration settings of the component. The value is
-- the escaped JSON of the configuration.
describeComponentConfigurationRecommendationResponse_componentConfiguration :: Lens.Lens' DescribeComponentConfigurationRecommendationResponse (Prelude.Maybe Prelude.Text)
describeComponentConfigurationRecommendationResponse_componentConfiguration = Lens.lens (\DescribeComponentConfigurationRecommendationResponse' {componentConfiguration} -> componentConfiguration) (\s@DescribeComponentConfigurationRecommendationResponse' {} a -> s {componentConfiguration = a} :: DescribeComponentConfigurationRecommendationResponse)

-- | The response's http status code.
describeComponentConfigurationRecommendationResponse_httpStatus :: Lens.Lens' DescribeComponentConfigurationRecommendationResponse Prelude.Int
describeComponentConfigurationRecommendationResponse_httpStatus = Lens.lens (\DescribeComponentConfigurationRecommendationResponse' {httpStatus} -> httpStatus) (\s@DescribeComponentConfigurationRecommendationResponse' {} a -> s {httpStatus = a} :: DescribeComponentConfigurationRecommendationResponse)

instance
  Prelude.NFData
    DescribeComponentConfigurationRecommendationResponse
  where
  rnf
    DescribeComponentConfigurationRecommendationResponse' {..} =
      Prelude.rnf componentConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
