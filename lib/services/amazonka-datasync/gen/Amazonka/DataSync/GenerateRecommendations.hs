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
-- Module      : Amazonka.DataSync.GenerateRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations about where to migrate your data to in Amazon
-- Web Services. Recommendations are generated based on information that
-- DataSync Discovery collects about your on-premises storage system\'s
-- resources. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
--
-- Once generated, you can view your recommendations by using the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_DescribeStorageSystemResources.html DescribeStorageSystemResources>
-- operation.
--
-- If your
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table discovery job completes successfully>,
-- you don\'t need to use this operation. DataSync Discovery generates the
-- recommendations for you automatically.
module Amazonka.DataSync.GenerateRecommendations
  ( -- * Creating a Request
    GenerateRecommendations (..),
    newGenerateRecommendations,

    -- * Request Lenses
    generateRecommendations_discoveryJobArn,
    generateRecommendations_resourceIds,
    generateRecommendations_resourceType,

    -- * Destructuring the Response
    GenerateRecommendationsResponse (..),
    newGenerateRecommendationsResponse,

    -- * Response Lenses
    generateRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateRecommendations' smart constructor.
data GenerateRecommendations = GenerateRecommendations'
  { -- | Specifies the Amazon Resource Name (ARN) of the discovery job that
    -- collects information about your on-premises storage system.
    discoveryJobArn :: Prelude.Text,
    -- | Specifies the universally unique identifiers (UUIDs) of the resources in
    -- your storage system that you want recommendations on.
    resourceIds :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the type of resource in your storage system that you want
    -- recommendations on.
    resourceType :: DiscoveryResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'generateRecommendations_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that
-- collects information about your on-premises storage system.
--
-- 'resourceIds', 'generateRecommendations_resourceIds' - Specifies the universally unique identifiers (UUIDs) of the resources in
-- your storage system that you want recommendations on.
--
-- 'resourceType', 'generateRecommendations_resourceType' - Specifies the type of resource in your storage system that you want
-- recommendations on.
newGenerateRecommendations ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  -- | 'resourceIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'resourceType'
  DiscoveryResourceType ->
  GenerateRecommendations
newGenerateRecommendations
  pDiscoveryJobArn_
  pResourceIds_
  pResourceType_ =
    GenerateRecommendations'
      { discoveryJobArn =
          pDiscoveryJobArn_,
        resourceIds = Lens.coerced Lens.# pResourceIds_,
        resourceType = pResourceType_
      }

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that
-- collects information about your on-premises storage system.
generateRecommendations_discoveryJobArn :: Lens.Lens' GenerateRecommendations Prelude.Text
generateRecommendations_discoveryJobArn = Lens.lens (\GenerateRecommendations' {discoveryJobArn} -> discoveryJobArn) (\s@GenerateRecommendations' {} a -> s {discoveryJobArn = a} :: GenerateRecommendations)

-- | Specifies the universally unique identifiers (UUIDs) of the resources in
-- your storage system that you want recommendations on.
generateRecommendations_resourceIds :: Lens.Lens' GenerateRecommendations (Prelude.NonEmpty Prelude.Text)
generateRecommendations_resourceIds = Lens.lens (\GenerateRecommendations' {resourceIds} -> resourceIds) (\s@GenerateRecommendations' {} a -> s {resourceIds = a} :: GenerateRecommendations) Prelude.. Lens.coerced

-- | Specifies the type of resource in your storage system that you want
-- recommendations on.
generateRecommendations_resourceType :: Lens.Lens' GenerateRecommendations DiscoveryResourceType
generateRecommendations_resourceType = Lens.lens (\GenerateRecommendations' {resourceType} -> resourceType) (\s@GenerateRecommendations' {} a -> s {resourceType = a} :: GenerateRecommendations)

instance Core.AWSRequest GenerateRecommendations where
  type
    AWSResponse GenerateRecommendations =
      GenerateRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          GenerateRecommendationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateRecommendations where
  hashWithSalt _salt GenerateRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` discoveryJobArn
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GenerateRecommendations where
  rnf GenerateRecommendations' {..} =
    Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GenerateRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.GenerateRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateRecommendations where
  toJSON GenerateRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn),
            Prelude.Just ("ResourceIds" Data..= resourceIds),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath GenerateRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery GenerateRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateRecommendationsResponse' smart constructor.
data GenerateRecommendationsResponse = GenerateRecommendationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'generateRecommendationsResponse_httpStatus' - The response's http status code.
newGenerateRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateRecommendationsResponse
newGenerateRecommendationsResponse pHttpStatus_ =
  GenerateRecommendationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
generateRecommendationsResponse_httpStatus :: Lens.Lens' GenerateRecommendationsResponse Prelude.Int
generateRecommendationsResponse_httpStatus = Lens.lens (\GenerateRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GenerateRecommendationsResponse' {} a -> s {httpStatus = a} :: GenerateRecommendationsResponse)

instance
  Prelude.NFData
    GenerateRecommendationsResponse
  where
  rnf GenerateRecommendationsResponse' {..} =
    Prelude.rnf httpStatus
