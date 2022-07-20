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
-- Module      : Amazonka.Pinpoint.GetRecommenderConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the recommender model configurations
-- that are associated with your Amazon Pinpoint account.
module Amazonka.Pinpoint.GetRecommenderConfigurations
  ( -- * Creating a Request
    GetRecommenderConfigurations (..),
    newGetRecommenderConfigurations,

    -- * Request Lenses
    getRecommenderConfigurations_pageSize,
    getRecommenderConfigurations_token,

    -- * Destructuring the Response
    GetRecommenderConfigurationsResponse (..),
    newGetRecommenderConfigurationsResponse,

    -- * Response Lenses
    getRecommenderConfigurationsResponse_httpStatus,
    getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecommenderConfigurations' smart constructor.
data GetRecommenderConfigurations = GetRecommenderConfigurations'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommenderConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getRecommenderConfigurations_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getRecommenderConfigurations_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
newGetRecommenderConfigurations ::
  GetRecommenderConfigurations
newGetRecommenderConfigurations =
  GetRecommenderConfigurations'
    { pageSize =
        Prelude.Nothing,
      token = Prelude.Nothing
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getRecommenderConfigurations_pageSize :: Lens.Lens' GetRecommenderConfigurations (Prelude.Maybe Prelude.Text)
getRecommenderConfigurations_pageSize = Lens.lens (\GetRecommenderConfigurations' {pageSize} -> pageSize) (\s@GetRecommenderConfigurations' {} a -> s {pageSize = a} :: GetRecommenderConfigurations)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getRecommenderConfigurations_token :: Lens.Lens' GetRecommenderConfigurations (Prelude.Maybe Prelude.Text)
getRecommenderConfigurations_token = Lens.lens (\GetRecommenderConfigurations' {token} -> token) (\s@GetRecommenderConfigurations' {} a -> s {token = a} :: GetRecommenderConfigurations)

instance Core.AWSRequest GetRecommenderConfigurations where
  type
    AWSResponse GetRecommenderConfigurations =
      GetRecommenderConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    GetRecommenderConfigurations
  where
  hashWithSalt _salt GetRecommenderConfigurations' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token

instance Prelude.NFData GetRecommenderConfigurations where
  rnf GetRecommenderConfigurations' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token

instance Core.ToHeaders GetRecommenderConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRecommenderConfigurations where
  toPath = Prelude.const "/v1/recommenders"

instance Core.ToQuery GetRecommenderConfigurations where
  toQuery GetRecommenderConfigurations' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetRecommenderConfigurationsResponse' smart constructor.
data GetRecommenderConfigurationsResponse = GetRecommenderConfigurationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    listRecommenderConfigurationsResponse :: ListRecommenderConfigurationsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommenderConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRecommenderConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'listRecommenderConfigurationsResponse', 'getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse' - Undocumented member.
newGetRecommenderConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'listRecommenderConfigurationsResponse'
  ListRecommenderConfigurationsResponse ->
  GetRecommenderConfigurationsResponse
newGetRecommenderConfigurationsResponse
  pHttpStatus_
  pListRecommenderConfigurationsResponse_ =
    GetRecommenderConfigurationsResponse'
      { httpStatus =
          pHttpStatus_,
        listRecommenderConfigurationsResponse =
          pListRecommenderConfigurationsResponse_
      }

-- | The response's http status code.
getRecommenderConfigurationsResponse_httpStatus :: Lens.Lens' GetRecommenderConfigurationsResponse Prelude.Int
getRecommenderConfigurationsResponse_httpStatus = Lens.lens (\GetRecommenderConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetRecommenderConfigurationsResponse' {} a -> s {httpStatus = a} :: GetRecommenderConfigurationsResponse)

-- | Undocumented member.
getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse :: Lens.Lens' GetRecommenderConfigurationsResponse ListRecommenderConfigurationsResponse
getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse = Lens.lens (\GetRecommenderConfigurationsResponse' {listRecommenderConfigurationsResponse} -> listRecommenderConfigurationsResponse) (\s@GetRecommenderConfigurationsResponse' {} a -> s {listRecommenderConfigurationsResponse = a} :: GetRecommenderConfigurationsResponse)

instance
  Prelude.NFData
    GetRecommenderConfigurationsResponse
  where
  rnf GetRecommenderConfigurationsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf listRecommenderConfigurationsResponse
