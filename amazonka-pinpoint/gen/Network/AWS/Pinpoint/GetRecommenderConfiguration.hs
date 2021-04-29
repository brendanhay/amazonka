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
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an Amazon Pinpoint configuration for a
-- recommender model.
module Network.AWS.Pinpoint.GetRecommenderConfiguration
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRecommenderConfiguration' smart constructor.
data GetRecommenderConfiguration = GetRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This
    -- identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint
    -- console.
    recommenderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    GetRecommenderConfiguration
  where
  type
    Rs GetRecommenderConfiguration =
      GetRecommenderConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetRecommenderConfiguration

instance Prelude.NFData GetRecommenderConfiguration

instance
  Prelude.ToHeaders
    GetRecommenderConfiguration
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

instance Prelude.ToPath GetRecommenderConfiguration where
  toPath GetRecommenderConfiguration' {..} =
    Prelude.mconcat
      ["/v1/recommenders/", Prelude.toBS recommenderId]

instance Prelude.ToQuery GetRecommenderConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecommenderConfigurationResponse' smart constructor.
data GetRecommenderConfigurationResponse = GetRecommenderConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    recommenderConfigurationResponse :: RecommenderConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
