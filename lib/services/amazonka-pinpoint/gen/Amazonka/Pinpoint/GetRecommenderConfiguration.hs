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
-- Module      : Amazonka.Pinpoint.GetRecommenderConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an Amazon Pinpoint configuration for a
-- recommender model.
module Amazonka.Pinpoint.GetRecommenderConfiguration
  ( -- * Creating a Request
    GetRecommenderConfiguration (..),
    newGetRecommenderConfiguration,

    -- * Request Lenses
    getRecommenderConfiguration_recommenderId,

    -- * Destructuring the Response
    GetRecommenderConfigurationResponse (..),
    newGetRecommenderConfigurationResponse,

    -- * Response Lenses
    getRecommenderConfigurationResponse_httpStatus,
    getRecommenderConfigurationResponse_recommenderConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecommenderConfiguration' smart constructor.
data GetRecommenderConfiguration = GetRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This
    -- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
    -- console.
    recommenderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommenderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderId', 'getRecommenderConfiguration_recommenderId' - The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
newGetRecommenderConfiguration ::
  -- | 'recommenderId'
  Prelude.Text ->
  GetRecommenderConfiguration
newGetRecommenderConfiguration pRecommenderId_ =
  GetRecommenderConfiguration'
    { recommenderId =
        pRecommenderId_
    }

-- | The unique identifier for the recommender model configuration. This
-- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
-- console.
getRecommenderConfiguration_recommenderId :: Lens.Lens' GetRecommenderConfiguration Prelude.Text
getRecommenderConfiguration_recommenderId = Lens.lens (\GetRecommenderConfiguration' {recommenderId} -> recommenderId) (\s@GetRecommenderConfiguration' {} a -> s {recommenderId = a} :: GetRecommenderConfiguration)

instance Core.AWSRequest GetRecommenderConfiguration where
  type
    AWSResponse GetRecommenderConfiguration =
      GetRecommenderConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetRecommenderConfiguration where
  hashWithSalt _salt GetRecommenderConfiguration' {..} =
    _salt `Prelude.hashWithSalt` recommenderId

instance Prelude.NFData GetRecommenderConfiguration where
  rnf GetRecommenderConfiguration' {..} =
    Prelude.rnf recommenderId

instance Data.ToHeaders GetRecommenderConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRecommenderConfiguration where
  toPath GetRecommenderConfiguration' {..} =
    Prelude.mconcat
      ["/v1/recommenders/", Data.toBS recommenderId]

instance Data.ToQuery GetRecommenderConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecommenderConfigurationResponse' smart constructor.
data GetRecommenderConfigurationResponse = GetRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRecommenderConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'recommenderConfigurationResponse', 'getRecommenderConfigurationResponse_recommenderConfigurationResponse' - Undocumented member.
newGetRecommenderConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recommenderConfigurationResponse'
  RecommenderConfigurationResponse ->
  GetRecommenderConfigurationResponse
newGetRecommenderConfigurationResponse
  pHttpStatus_
  pRecommenderConfigurationResponse_ =
    GetRecommenderConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        recommenderConfigurationResponse =
          pRecommenderConfigurationResponse_
      }

-- | The response's http status code.
getRecommenderConfigurationResponse_httpStatus :: Lens.Lens' GetRecommenderConfigurationResponse Prelude.Int
getRecommenderConfigurationResponse_httpStatus = Lens.lens (\GetRecommenderConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetRecommenderConfigurationResponse' {} a -> s {httpStatus = a} :: GetRecommenderConfigurationResponse)

-- | Undocumented member.
getRecommenderConfigurationResponse_recommenderConfigurationResponse :: Lens.Lens' GetRecommenderConfigurationResponse RecommenderConfigurationResponse
getRecommenderConfigurationResponse_recommenderConfigurationResponse = Lens.lens (\GetRecommenderConfigurationResponse' {recommenderConfigurationResponse} -> recommenderConfigurationResponse) (\s@GetRecommenderConfigurationResponse' {} a -> s {recommenderConfigurationResponse = a} :: GetRecommenderConfigurationResponse)

instance
  Prelude.NFData
    GetRecommenderConfigurationResponse
  where
  rnf GetRecommenderConfigurationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf recommenderConfigurationResponse
