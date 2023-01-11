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
-- Module      : Amazonka.SSM.GetParametersByPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about one or more parameters in a specific
-- hierarchy.
--
-- Request results are returned on a best-effort basis. If you specify
-- @MaxResults@ in the request, the response includes information up to the
-- limit specified. The number of items returned, however, can be between
-- zero and the value of @MaxResults@. If the service reaches an internal
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point and a @NextToken@. You can specify
-- the @NextToken@ in a subsequent call to get the next set of results.
--
-- This operation returns paginated results.
module Amazonka.SSM.GetParametersByPath
  ( -- * Creating a Request
    GetParametersByPath (..),
    newGetParametersByPath,

    -- * Request Lenses
    getParametersByPath_maxResults,
    getParametersByPath_nextToken,
    getParametersByPath_parameterFilters,
    getParametersByPath_recursive,
    getParametersByPath_withDecryption,
    getParametersByPath_path,

    -- * Destructuring the Response
    GetParametersByPathResponse (..),
    newGetParametersByPathResponse,

    -- * Response Lenses
    getParametersByPathResponse_nextToken,
    getParametersByPathResponse_parameters,
    getParametersByPathResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetParametersByPath' smart constructor.
data GetParametersByPath = GetParametersByPath'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters to limit the request results.
    --
    -- The following @Key@ values are supported for @GetParametersByPath@:
    -- @Type@, @KeyId@, and @Label@.
    --
    -- The following @Key@ values aren\'t supported for @GetParametersByPath@:
    -- @tag@, @DataType@, @Name@, @Path@, and @Tier@.
    parameterFilters :: Prelude.Maybe [ParameterStringFilter],
    -- | Retrieve all parameters within a hierarchy.
    --
    -- If a user has access to a path, then the user can access all levels of
    -- that path. For example, if a user has permission to access path @\/a@,
    -- then the user can also access @\/a\/b@. Even if a user has explicitly
    -- been denied access in IAM for parameter @\/a\/b@, they can still call
    -- the GetParametersByPath API operation recursively for @\/a@ and view
    -- @\/a\/b@.
    recursive :: Prelude.Maybe Prelude.Bool,
    -- | Retrieve all parameters in a hierarchy with their value decrypted.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | The hierarchy for the parameter. Hierarchies start with a forward slash
    -- (\/). The hierarchy is the parameter name except the last part of the
    -- parameter. For the API call to succeed, the last part of the parameter
    -- name can\'t be in the path. A parameter name hierarchy can have a
    -- maximum of 15 levels. Here is an example of a hierarchy:
    -- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersByPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getParametersByPath_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'getParametersByPath_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'parameterFilters', 'getParametersByPath_parameterFilters' - Filters to limit the request results.
--
-- The following @Key@ values are supported for @GetParametersByPath@:
-- @Type@, @KeyId@, and @Label@.
--
-- The following @Key@ values aren\'t supported for @GetParametersByPath@:
-- @tag@, @DataType@, @Name@, @Path@, and @Tier@.
--
-- 'recursive', 'getParametersByPath_recursive' - Retrieve all parameters within a hierarchy.
--
-- If a user has access to a path, then the user can access all levels of
-- that path. For example, if a user has permission to access path @\/a@,
-- then the user can also access @\/a\/b@. Even if a user has explicitly
-- been denied access in IAM for parameter @\/a\/b@, they can still call
-- the GetParametersByPath API operation recursively for @\/a@ and view
-- @\/a\/b@.
--
-- 'withDecryption', 'getParametersByPath_withDecryption' - Retrieve all parameters in a hierarchy with their value decrypted.
--
-- 'path', 'getParametersByPath_path' - The hierarchy for the parameter. Hierarchies start with a forward slash
-- (\/). The hierarchy is the parameter name except the last part of the
-- parameter. For the API call to succeed, the last part of the parameter
-- name can\'t be in the path. A parameter name hierarchy can have a
-- maximum of 15 levels. Here is an example of a hierarchy:
-- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
newGetParametersByPath ::
  -- | 'path'
  Prelude.Text ->
  GetParametersByPath
newGetParametersByPath pPath_ =
  GetParametersByPath'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parameterFilters = Prelude.Nothing,
      recursive = Prelude.Nothing,
      withDecryption = Prelude.Nothing,
      path = pPath_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getParametersByPath_maxResults :: Lens.Lens' GetParametersByPath (Prelude.Maybe Prelude.Natural)
getParametersByPath_maxResults = Lens.lens (\GetParametersByPath' {maxResults} -> maxResults) (\s@GetParametersByPath' {} a -> s {maxResults = a} :: GetParametersByPath)

-- | A token to start the list. Use this token to get the next set of
-- results.
getParametersByPath_nextToken :: Lens.Lens' GetParametersByPath (Prelude.Maybe Prelude.Text)
getParametersByPath_nextToken = Lens.lens (\GetParametersByPath' {nextToken} -> nextToken) (\s@GetParametersByPath' {} a -> s {nextToken = a} :: GetParametersByPath)

-- | Filters to limit the request results.
--
-- The following @Key@ values are supported for @GetParametersByPath@:
-- @Type@, @KeyId@, and @Label@.
--
-- The following @Key@ values aren\'t supported for @GetParametersByPath@:
-- @tag@, @DataType@, @Name@, @Path@, and @Tier@.
getParametersByPath_parameterFilters :: Lens.Lens' GetParametersByPath (Prelude.Maybe [ParameterStringFilter])
getParametersByPath_parameterFilters = Lens.lens (\GetParametersByPath' {parameterFilters} -> parameterFilters) (\s@GetParametersByPath' {} a -> s {parameterFilters = a} :: GetParametersByPath) Prelude.. Lens.mapping Lens.coerced

-- | Retrieve all parameters within a hierarchy.
--
-- If a user has access to a path, then the user can access all levels of
-- that path. For example, if a user has permission to access path @\/a@,
-- then the user can also access @\/a\/b@. Even if a user has explicitly
-- been denied access in IAM for parameter @\/a\/b@, they can still call
-- the GetParametersByPath API operation recursively for @\/a@ and view
-- @\/a\/b@.
getParametersByPath_recursive :: Lens.Lens' GetParametersByPath (Prelude.Maybe Prelude.Bool)
getParametersByPath_recursive = Lens.lens (\GetParametersByPath' {recursive} -> recursive) (\s@GetParametersByPath' {} a -> s {recursive = a} :: GetParametersByPath)

-- | Retrieve all parameters in a hierarchy with their value decrypted.
getParametersByPath_withDecryption :: Lens.Lens' GetParametersByPath (Prelude.Maybe Prelude.Bool)
getParametersByPath_withDecryption = Lens.lens (\GetParametersByPath' {withDecryption} -> withDecryption) (\s@GetParametersByPath' {} a -> s {withDecryption = a} :: GetParametersByPath)

-- | The hierarchy for the parameter. Hierarchies start with a forward slash
-- (\/). The hierarchy is the parameter name except the last part of the
-- parameter. For the API call to succeed, the last part of the parameter
-- name can\'t be in the path. A parameter name hierarchy can have a
-- maximum of 15 levels. Here is an example of a hierarchy:
-- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
getParametersByPath_path :: Lens.Lens' GetParametersByPath Prelude.Text
getParametersByPath_path = Lens.lens (\GetParametersByPath' {path} -> path) (\s@GetParametersByPath' {} a -> s {path = a} :: GetParametersByPath)

instance Core.AWSPager GetParametersByPath where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getParametersByPathResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getParametersByPathResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getParametersByPath_nextToken
          Lens..~ rs
          Lens.^? getParametersByPathResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetParametersByPath where
  type
    AWSResponse GetParametersByPath =
      GetParametersByPathResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersByPathResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParametersByPath where
  hashWithSalt _salt GetParametersByPath' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parameterFilters
      `Prelude.hashWithSalt` recursive
      `Prelude.hashWithSalt` withDecryption
      `Prelude.hashWithSalt` path

instance Prelude.NFData GetParametersByPath where
  rnf GetParametersByPath' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameterFilters
      `Prelude.seq` Prelude.rnf recursive
      `Prelude.seq` Prelude.rnf withDecryption
      `Prelude.seq` Prelude.rnf path

instance Data.ToHeaders GetParametersByPath where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetParametersByPath" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParametersByPath where
  toJSON GetParametersByPath' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ParameterFilters" Data..=)
              Prelude.<$> parameterFilters,
            ("Recursive" Data..=) Prelude.<$> recursive,
            ("WithDecryption" Data..=)
              Prelude.<$> withDecryption,
            Prelude.Just ("Path" Data..= path)
          ]
      )

instance Data.ToPath GetParametersByPath where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParametersByPath where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersByPathResponse' smart constructor.
data GetParametersByPathResponse = GetParametersByPathResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of parameters found in the specified hierarchy.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersByPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getParametersByPathResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'parameters', 'getParametersByPathResponse_parameters' - A list of parameters found in the specified hierarchy.
--
-- 'httpStatus', 'getParametersByPathResponse_httpStatus' - The response's http status code.
newGetParametersByPathResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParametersByPathResponse
newGetParametersByPathResponse pHttpStatus_ =
  GetParametersByPathResponse'
    { nextToken =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
getParametersByPathResponse_nextToken :: Lens.Lens' GetParametersByPathResponse (Prelude.Maybe Prelude.Text)
getParametersByPathResponse_nextToken = Lens.lens (\GetParametersByPathResponse' {nextToken} -> nextToken) (\s@GetParametersByPathResponse' {} a -> s {nextToken = a} :: GetParametersByPathResponse)

-- | A list of parameters found in the specified hierarchy.
getParametersByPathResponse_parameters :: Lens.Lens' GetParametersByPathResponse (Prelude.Maybe [Parameter])
getParametersByPathResponse_parameters = Lens.lens (\GetParametersByPathResponse' {parameters} -> parameters) (\s@GetParametersByPathResponse' {} a -> s {parameters = a} :: GetParametersByPathResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getParametersByPathResponse_httpStatus :: Lens.Lens' GetParametersByPathResponse Prelude.Int
getParametersByPathResponse_httpStatus = Lens.lens (\GetParametersByPathResponse' {httpStatus} -> httpStatus) (\s@GetParametersByPathResponse' {} a -> s {httpStatus = a} :: GetParametersByPathResponse)

instance Prelude.NFData GetParametersByPathResponse where
  rnf GetParametersByPathResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
