{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRecommenderConfiguration'' smart constructor.
data CreateRecommenderConfiguration' = CreateRecommenderConfiguration''
  { createRecommenderConfiguration :: CreateRecommenderConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    CreateRecommenderConfiguration'
  where
  type
    Rs CreateRecommenderConfiguration' =
      CreateRecommenderConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecommenderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    CreateRecommenderConfiguration'

instance
  Prelude.NFData
    CreateRecommenderConfiguration'

instance
  Prelude.ToHeaders
    CreateRecommenderConfiguration'
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    CreateRecommenderConfiguration'
  where
  toJSON CreateRecommenderConfiguration'' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CreateRecommenderConfiguration"
                  Prelude..= createRecommenderConfiguration
              )
          ]
      )

instance
  Prelude.ToPath
    CreateRecommenderConfiguration'
  where
  toPath = Prelude.const "/v1/recommenders"

instance
  Prelude.ToQuery
    CreateRecommenderConfiguration'
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
createRecommenderConfigurationResponse_httpStatus :: Lens.Lens' CreateRecommenderConfigurationResponse Prelude.Int
createRecommenderConfigurationResponse_httpStatus = Lens.lens (\CreateRecommenderConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateRecommenderConfigurationResponse' {} a -> s {httpStatus = a} :: CreateRecommenderConfigurationResponse)

-- | Undocumented member.
createRecommenderConfigurationResponse_recommenderConfigurationResponse :: Lens.Lens' CreateRecommenderConfigurationResponse RecommenderConfigurationResponse
createRecommenderConfigurationResponse_recommenderConfigurationResponse = Lens.lens (\CreateRecommenderConfigurationResponse' {recommenderConfigurationResponse} -> recommenderConfigurationResponse) (\s@CreateRecommenderConfigurationResponse' {} a -> s {recommenderConfigurationResponse = a} :: CreateRecommenderConfigurationResponse)

instance
  Prelude.NFData
    CreateRecommenderConfigurationResponse
