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
-- Module      : Network.AWS.Glue.GetSecurityConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all security configurations.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetSecurityConfigurations
  ( -- * Creating a Request
    GetSecurityConfigurations (..),
    newGetSecurityConfigurations,

    -- * Request Lenses
    getSecurityConfigurations_nextToken,
    getSecurityConfigurations_maxResults,

    -- * Destructuring the Response
    GetSecurityConfigurationsResponse (..),
    newGetSecurityConfigurationsResponse,

    -- * Response Lenses
    getSecurityConfigurationsResponse_nextToken,
    getSecurityConfigurationsResponse_securityConfigurations,
    getSecurityConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSecurityConfigurations' smart constructor.
data GetSecurityConfigurations = GetSecurityConfigurations'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSecurityConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSecurityConfigurations_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getSecurityConfigurations_maxResults' - The maximum number of results to return.
newGetSecurityConfigurations ::
  GetSecurityConfigurations
newGetSecurityConfigurations =
  GetSecurityConfigurations'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getSecurityConfigurations_nextToken :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Core.Text)
getSecurityConfigurations_nextToken = Lens.lens (\GetSecurityConfigurations' {nextToken} -> nextToken) (\s@GetSecurityConfigurations' {} a -> s {nextToken = a} :: GetSecurityConfigurations)

-- | The maximum number of results to return.
getSecurityConfigurations_maxResults :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Core.Natural)
getSecurityConfigurations_maxResults = Lens.lens (\GetSecurityConfigurations' {maxResults} -> maxResults) (\s@GetSecurityConfigurations' {} a -> s {maxResults = a} :: GetSecurityConfigurations)

instance Core.AWSPager GetSecurityConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSecurityConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getSecurityConfigurationsResponse_securityConfigurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getSecurityConfigurations_nextToken
          Lens..~ rs
          Lens.^? getSecurityConfigurationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetSecurityConfigurations where
  type
    AWSResponse GetSecurityConfigurations =
      GetSecurityConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "SecurityConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSecurityConfigurations

instance Core.NFData GetSecurityConfigurations

instance Core.ToHeaders GetSecurityConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetSecurityConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSecurityConfigurations where
  toJSON GetSecurityConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath GetSecurityConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery GetSecurityConfigurations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSecurityConfigurationsResponse' smart constructor.
data GetSecurityConfigurationsResponse = GetSecurityConfigurationsResponse'
  { -- | A continuation token, if there are more security configurations to
    -- return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of security configurations.
    securityConfigurations :: Core.Maybe [SecurityConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSecurityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSecurityConfigurationsResponse_nextToken' - A continuation token, if there are more security configurations to
-- return.
--
-- 'securityConfigurations', 'getSecurityConfigurationsResponse_securityConfigurations' - A list of security configurations.
--
-- 'httpStatus', 'getSecurityConfigurationsResponse_httpStatus' - The response's http status code.
newGetSecurityConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSecurityConfigurationsResponse
newGetSecurityConfigurationsResponse pHttpStatus_ =
  GetSecurityConfigurationsResponse'
    { nextToken =
        Core.Nothing,
      securityConfigurations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if there are more security configurations to
-- return.
getSecurityConfigurationsResponse_nextToken :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe Core.Text)
getSecurityConfigurationsResponse_nextToken = Lens.lens (\GetSecurityConfigurationsResponse' {nextToken} -> nextToken) (\s@GetSecurityConfigurationsResponse' {} a -> s {nextToken = a} :: GetSecurityConfigurationsResponse)

-- | A list of security configurations.
getSecurityConfigurationsResponse_securityConfigurations :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe [SecurityConfiguration])
getSecurityConfigurationsResponse_securityConfigurations = Lens.lens (\GetSecurityConfigurationsResponse' {securityConfigurations} -> securityConfigurations) (\s@GetSecurityConfigurationsResponse' {} a -> s {securityConfigurations = a} :: GetSecurityConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSecurityConfigurationsResponse_httpStatus :: Lens.Lens' GetSecurityConfigurationsResponse Core.Int
getSecurityConfigurationsResponse_httpStatus = Lens.lens (\GetSecurityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetSecurityConfigurationsResponse' {} a -> s {httpStatus = a} :: GetSecurityConfigurationsResponse)

instance
  Core.NFData
    GetSecurityConfigurationsResponse
