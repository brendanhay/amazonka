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
-- Module      : Network.AWS.Pinpoint.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.CreateRecommenderConfiguration
  ( -- * Creating a Request
    CreateRecommenderConfiguration' (..),
    newCreateRecommenderConfiguration',

    -- * Request Lenses
    createRecommenderConfiguration'_createRecommenderConfiguration,

    -- * Destructuring the Response
    CreateRecommenderConfigurationResponse (..),
    newCreateRecommenderConfigurationResponse,

    -- * Response Lenses
    createRecommenderConfigurationResponse_httpStatus,
    createRecommenderConfigurationResponse_recommenderConfigurationResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRecommenderConfiguration'' smart constructor.
data CreateRecommenderConfiguration' = CreateRecommenderConfiguration''
  { createRecommenderConfiguration :: CreateRecommenderConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRecommenderConfiguration'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createRecommenderConfiguration', 'createRecommenderConfiguration'_createRecommenderConfiguration' - Undocumented member.
newCreateRecommenderConfiguration' ::
  -- | 'createRecommenderConfiguration'
  CreateRecommenderConfiguration ->
  CreateRecommenderConfiguration'
newCreateRecommenderConfiguration'
  pCreateRecommenderConfiguration_ =
    CreateRecommenderConfiguration''
      { createRecommenderConfiguration =
          pCreateRecommenderConfiguration_
      }

-- | Undocumented member.
createRecommenderConfiguration'_createRecommenderConfiguration :: Lens.Lens' CreateRecommenderConfiguration' CreateRecommenderConfiguration
createRecommenderConfiguration'_createRecommenderConfiguration = Lens.lens (\CreateRecommenderConfiguration'' {createRecommenderConfiguration} -> createRecommenderConfiguration) (\s@CreateRecommenderConfiguration'' {} a -> s {createRecommenderConfiguration = a} :: CreateRecommenderConfiguration')

instance
  Core.AWSRequest
    CreateRecommenderConfiguration'
  where
  type
    AWSResponse CreateRecommenderConfiguration' =
      CreateRecommenderConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecommenderConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance
  Core.Hashable
    CreateRecommenderConfiguration'

instance Core.NFData CreateRecommenderConfiguration'

instance
  Core.ToHeaders
    CreateRecommenderConfiguration'
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRecommenderConfiguration' where
  toJSON CreateRecommenderConfiguration'' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "CreateRecommenderConfiguration"
                  Core..= createRecommenderConfiguration
              )
          ]
      )

instance Core.ToPath CreateRecommenderConfiguration' where
  toPath = Core.const "/v1/recommenders"

instance Core.ToQuery CreateRecommenderConfiguration' where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRecommenderConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'recommenderConfigurationResponse', 'createRecommenderConfigurationResponse_recommenderConfigurationResponse' - Undocumented member.
newCreateRecommenderConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  CreateRecommenderConfigurationResponse
newCreateRecommenderConfigurationResponse
  pHttpStatus_
  pRecommenderConfigurationResponse_ =
    CreateRecommenderConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | The response's http status code.
createRecommenderConfigurationResponse_httpStatus :: Lens.Lens' CreateRecommenderConfigurationResponse Core.Int
createRecommenderConfigurationResponse_httpStatus = Lens.lens (\CreateRecommenderConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateRecommenderConfigurationResponse' {} a -> s {httpStatus = a} :: CreateRecommenderConfigurationResponse)

-- | Undocumented member.
createRecommenderConfigurationResponse_recommenderConfigurationResponse :: Lens.Lens' CreateRecommenderConfigurationResponse RecommenderConfigurationResponse
createRecommenderConfigurationResponse_recommenderConfigurationResponse = Lens.lens (\CreateRecommenderConfigurationResponse' {recommenderConfigurationResponse} -> recommenderConfigurationResponse) (\s@CreateRecommenderConfigurationResponse' {} a -> s {recommenderConfigurationResponse = a} :: CreateRecommenderConfigurationResponse)

instance
  Core.NFData
    CreateRecommenderConfigurationResponse
