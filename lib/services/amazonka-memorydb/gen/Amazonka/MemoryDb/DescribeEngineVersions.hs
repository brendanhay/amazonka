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
-- Module      : Amazonka.MemoryDb.DescribeEngineVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available Redis engine versions.
module Amazonka.MemoryDb.DescribeEngineVersions
  ( -- * Creating a Request
    DescribeEngineVersions (..),
    newDescribeEngineVersions,

    -- * Request Lenses
    describeEngineVersions_engineVersion,
    describeEngineVersions_defaultOnly,
    describeEngineVersions_nextToken,
    describeEngineVersions_parameterGroupFamily,
    describeEngineVersions_maxResults,

    -- * Destructuring the Response
    DescribeEngineVersionsResponse (..),
    newDescribeEngineVersionsResponse,

    -- * Response Lenses
    describeEngineVersionsResponse_nextToken,
    describeEngineVersionsResponse_engineVersions,
    describeEngineVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEngineVersions' smart constructor.
data DescribeEngineVersions = DescribeEngineVersions'
  { -- | The Redis engine version
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | If true, specifies that only the default version of the specified engine
    -- or engine and major version combination is to be returned.
    defaultOnly :: Prelude.Maybe Prelude.Bool,
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a specific parameter group family to return details for.
    parameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'describeEngineVersions_engineVersion' - The Redis engine version
--
-- 'defaultOnly', 'describeEngineVersions_defaultOnly' - If true, specifies that only the default version of the specified engine
-- or engine and major version combination is to be returned.
--
-- 'nextToken', 'describeEngineVersions_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'parameterGroupFamily', 'describeEngineVersions_parameterGroupFamily' - The name of a specific parameter group family to return details for.
--
-- 'maxResults', 'describeEngineVersions_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
newDescribeEngineVersions ::
  DescribeEngineVersions
newDescribeEngineVersions =
  DescribeEngineVersions'
    { engineVersion =
        Prelude.Nothing,
      defaultOnly = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parameterGroupFamily = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The Redis engine version
describeEngineVersions_engineVersion :: Lens.Lens' DescribeEngineVersions (Prelude.Maybe Prelude.Text)
describeEngineVersions_engineVersion = Lens.lens (\DescribeEngineVersions' {engineVersion} -> engineVersion) (\s@DescribeEngineVersions' {} a -> s {engineVersion = a} :: DescribeEngineVersions)

-- | If true, specifies that only the default version of the specified engine
-- or engine and major version combination is to be returned.
describeEngineVersions_defaultOnly :: Lens.Lens' DescribeEngineVersions (Prelude.Maybe Prelude.Bool)
describeEngineVersions_defaultOnly = Lens.lens (\DescribeEngineVersions' {defaultOnly} -> defaultOnly) (\s@DescribeEngineVersions' {} a -> s {defaultOnly = a} :: DescribeEngineVersions)

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeEngineVersions_nextToken :: Lens.Lens' DescribeEngineVersions (Prelude.Maybe Prelude.Text)
describeEngineVersions_nextToken = Lens.lens (\DescribeEngineVersions' {nextToken} -> nextToken) (\s@DescribeEngineVersions' {} a -> s {nextToken = a} :: DescribeEngineVersions)

-- | The name of a specific parameter group family to return details for.
describeEngineVersions_parameterGroupFamily :: Lens.Lens' DescribeEngineVersions (Prelude.Maybe Prelude.Text)
describeEngineVersions_parameterGroupFamily = Lens.lens (\DescribeEngineVersions' {parameterGroupFamily} -> parameterGroupFamily) (\s@DescribeEngineVersions' {} a -> s {parameterGroupFamily = a} :: DescribeEngineVersions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeEngineVersions_maxResults :: Lens.Lens' DescribeEngineVersions (Prelude.Maybe Prelude.Int)
describeEngineVersions_maxResults = Lens.lens (\DescribeEngineVersions' {maxResults} -> maxResults) (\s@DescribeEngineVersions' {} a -> s {maxResults = a} :: DescribeEngineVersions)

instance Core.AWSRequest DescribeEngineVersions where
  type
    AWSResponse DescribeEngineVersions =
      DescribeEngineVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEngineVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "EngineVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEngineVersions where
  hashWithSalt _salt DescribeEngineVersions' {..} =
    _salt `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` defaultOnly
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parameterGroupFamily
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeEngineVersions where
  rnf DescribeEngineVersions' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf defaultOnly
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameterGroupFamily
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeEngineVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.DescribeEngineVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEngineVersions where
  toJSON DescribeEngineVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("DefaultOnly" Core..=) Prelude.<$> defaultOnly,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ParameterGroupFamily" Core..=)
              Prelude.<$> parameterGroupFamily,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeEngineVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEngineVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEngineVersionsResponse' smart constructor.
data DescribeEngineVersionsResponse = DescribeEngineVersionsResponse'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of engine version details. Each element in the list contains
    -- detailed information about one engine version.
    engineVersions :: Prelude.Maybe [EngineVersionInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngineVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEngineVersionsResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'engineVersions', 'describeEngineVersionsResponse_engineVersions' - A list of engine version details. Each element in the list contains
-- detailed information about one engine version.
--
-- 'httpStatus', 'describeEngineVersionsResponse_httpStatus' - The response's http status code.
newDescribeEngineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEngineVersionsResponse
newDescribeEngineVersionsResponse pHttpStatus_ =
  DescribeEngineVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      engineVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeEngineVersionsResponse_nextToken :: Lens.Lens' DescribeEngineVersionsResponse (Prelude.Maybe Prelude.Text)
describeEngineVersionsResponse_nextToken = Lens.lens (\DescribeEngineVersionsResponse' {nextToken} -> nextToken) (\s@DescribeEngineVersionsResponse' {} a -> s {nextToken = a} :: DescribeEngineVersionsResponse)

-- | A list of engine version details. Each element in the list contains
-- detailed information about one engine version.
describeEngineVersionsResponse_engineVersions :: Lens.Lens' DescribeEngineVersionsResponse (Prelude.Maybe [EngineVersionInfo])
describeEngineVersionsResponse_engineVersions = Lens.lens (\DescribeEngineVersionsResponse' {engineVersions} -> engineVersions) (\s@DescribeEngineVersionsResponse' {} a -> s {engineVersions = a} :: DescribeEngineVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEngineVersionsResponse_httpStatus :: Lens.Lens' DescribeEngineVersionsResponse Prelude.Int
describeEngineVersionsResponse_httpStatus = Lens.lens (\DescribeEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeEngineVersionsResponse' {} a -> s {httpStatus = a} :: DescribeEngineVersionsResponse)

instance
  Prelude.NFData
    DescribeEngineVersionsResponse
  where
  rnf DescribeEngineVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf engineVersions
      `Prelude.seq` Prelude.rnf httpStatus
