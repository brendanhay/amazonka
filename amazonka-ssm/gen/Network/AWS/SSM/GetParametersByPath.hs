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
-- Module      : Network.AWS.SSM.GetParametersByPath
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SSM.GetParametersByPath
  ( -- * Creating a Request
    GetParametersByPath (..),
    newGetParametersByPath,

    -- * Request Lenses
    getParametersByPath_withDecryption,
    getParametersByPath_nextToken,
    getParametersByPath_maxResults,
    getParametersByPath_recursive,
    getParametersByPath_parameterFilters,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParametersByPath' smart constructor.
data GetParametersByPath = GetParametersByPath'
  { -- | Retrieve all parameters in a hierarchy with their value decrypted.
    withDecryption :: Core.Maybe Core.Bool,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Retrieve all parameters within a hierarchy.
    --
    -- If a user has access to a path, then the user can access all levels of
    -- that path. For example, if a user has permission to access path @\/a@,
    -- then the user can also access @\/a\/b@. Even if a user has explicitly
    -- been denied access in IAM for parameter @\/a\/b@, they can still call
    -- the GetParametersByPath API action recursively for @\/a@ and view
    -- @\/a\/b@.
    recursive :: Core.Maybe Core.Bool,
    -- | Filters to limit the request results.
    --
    -- For @GetParametersByPath@, the following filter @Key@ names are
    -- supported: @Type@, @KeyId@, @Label@, and @DataType@.
    --
    -- The following @Key@ values are not supported for @GetParametersByPath@:
    -- @tag@, @Name@, @Path@, and @Tier@.
    parameterFilters :: Core.Maybe [ParameterStringFilter],
    -- | The hierarchy for the parameter. Hierarchies start with a forward slash
    -- (\/). The hierachy is the parameter name except the last part of the
    -- parameter. For the API call to succeeed, the last part of the parameter
    -- name cannot be in the path. A parameter name hierarchy can have a
    -- maximum of 15 levels. Here is an example of a hierarchy:
    -- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
    path :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetParametersByPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withDecryption', 'getParametersByPath_withDecryption' - Retrieve all parameters in a hierarchy with their value decrypted.
--
-- 'nextToken', 'getParametersByPath_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'getParametersByPath_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'recursive', 'getParametersByPath_recursive' - Retrieve all parameters within a hierarchy.
--
-- If a user has access to a path, then the user can access all levels of
-- that path. For example, if a user has permission to access path @\/a@,
-- then the user can also access @\/a\/b@. Even if a user has explicitly
-- been denied access in IAM for parameter @\/a\/b@, they can still call
-- the GetParametersByPath API action recursively for @\/a@ and view
-- @\/a\/b@.
--
-- 'parameterFilters', 'getParametersByPath_parameterFilters' - Filters to limit the request results.
--
-- For @GetParametersByPath@, the following filter @Key@ names are
-- supported: @Type@, @KeyId@, @Label@, and @DataType@.
--
-- The following @Key@ values are not supported for @GetParametersByPath@:
-- @tag@, @Name@, @Path@, and @Tier@.
--
-- 'path', 'getParametersByPath_path' - The hierarchy for the parameter. Hierarchies start with a forward slash
-- (\/). The hierachy is the parameter name except the last part of the
-- parameter. For the API call to succeeed, the last part of the parameter
-- name cannot be in the path. A parameter name hierarchy can have a
-- maximum of 15 levels. Here is an example of a hierarchy:
-- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
newGetParametersByPath ::
  -- | 'path'
  Core.Text ->
  GetParametersByPath
newGetParametersByPath pPath_ =
  GetParametersByPath'
    { withDecryption = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      recursive = Core.Nothing,
      parameterFilters = Core.Nothing,
      path = pPath_
    }

-- | Retrieve all parameters in a hierarchy with their value decrypted.
getParametersByPath_withDecryption :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Bool)
getParametersByPath_withDecryption = Lens.lens (\GetParametersByPath' {withDecryption} -> withDecryption) (\s@GetParametersByPath' {} a -> s {withDecryption = a} :: GetParametersByPath)

-- | A token to start the list. Use this token to get the next set of
-- results.
getParametersByPath_nextToken :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Text)
getParametersByPath_nextToken = Lens.lens (\GetParametersByPath' {nextToken} -> nextToken) (\s@GetParametersByPath' {} a -> s {nextToken = a} :: GetParametersByPath)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getParametersByPath_maxResults :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Natural)
getParametersByPath_maxResults = Lens.lens (\GetParametersByPath' {maxResults} -> maxResults) (\s@GetParametersByPath' {} a -> s {maxResults = a} :: GetParametersByPath)

-- | Retrieve all parameters within a hierarchy.
--
-- If a user has access to a path, then the user can access all levels of
-- that path. For example, if a user has permission to access path @\/a@,
-- then the user can also access @\/a\/b@. Even if a user has explicitly
-- been denied access in IAM for parameter @\/a\/b@, they can still call
-- the GetParametersByPath API action recursively for @\/a@ and view
-- @\/a\/b@.
getParametersByPath_recursive :: Lens.Lens' GetParametersByPath (Core.Maybe Core.Bool)
getParametersByPath_recursive = Lens.lens (\GetParametersByPath' {recursive} -> recursive) (\s@GetParametersByPath' {} a -> s {recursive = a} :: GetParametersByPath)

-- | Filters to limit the request results.
--
-- For @GetParametersByPath@, the following filter @Key@ names are
-- supported: @Type@, @KeyId@, @Label@, and @DataType@.
--
-- The following @Key@ values are not supported for @GetParametersByPath@:
-- @tag@, @Name@, @Path@, and @Tier@.
getParametersByPath_parameterFilters :: Lens.Lens' GetParametersByPath (Core.Maybe [ParameterStringFilter])
getParametersByPath_parameterFilters = Lens.lens (\GetParametersByPath' {parameterFilters} -> parameterFilters) (\s@GetParametersByPath' {} a -> s {parameterFilters = a} :: GetParametersByPath) Core.. Lens.mapping Lens._Coerce

-- | The hierarchy for the parameter. Hierarchies start with a forward slash
-- (\/). The hierachy is the parameter name except the last part of the
-- parameter. For the API call to succeeed, the last part of the parameter
-- name cannot be in the path. A parameter name hierarchy can have a
-- maximum of 15 levels. Here is an example of a hierarchy:
-- @\/Finance\/Prod\/IAD\/WinServ2016\/license33 @
getParametersByPath_path :: Lens.Lens' GetParametersByPath Core.Text
getParametersByPath_path = Lens.lens (\GetParametersByPath' {path} -> path) (\s@GetParametersByPath' {} a -> s {path = a} :: GetParametersByPath)

instance Core.AWSPager GetParametersByPath where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getParametersByPathResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getParametersByPathResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getParametersByPath_nextToken
          Lens..~ rs
          Lens.^? getParametersByPathResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetParametersByPath where
  type
    AWSResponse GetParametersByPath =
      GetParametersByPathResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersByPathResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetParametersByPath

instance Core.NFData GetParametersByPath

instance Core.ToHeaders GetParametersByPath where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParametersByPath" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetParametersByPath where
  toJSON GetParametersByPath' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WithDecryption" Core..=) Core.<$> withDecryption,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Recursive" Core..=) Core.<$> recursive,
            ("ParameterFilters" Core..=)
              Core.<$> parameterFilters,
            Core.Just ("Path" Core..= path)
          ]
      )

instance Core.ToPath GetParametersByPath where
  toPath = Core.const "/"

instance Core.ToQuery GetParametersByPath where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetParametersByPathResponse' smart constructor.
data GetParametersByPathResponse = GetParametersByPathResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of parameters found in the specified hierarchy.
    parameters :: Core.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetParametersByPathResponse
newGetParametersByPathResponse pHttpStatus_ =
  GetParametersByPathResponse'
    { nextToken =
        Core.Nothing,
      parameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
getParametersByPathResponse_nextToken :: Lens.Lens' GetParametersByPathResponse (Core.Maybe Core.Text)
getParametersByPathResponse_nextToken = Lens.lens (\GetParametersByPathResponse' {nextToken} -> nextToken) (\s@GetParametersByPathResponse' {} a -> s {nextToken = a} :: GetParametersByPathResponse)

-- | A list of parameters found in the specified hierarchy.
getParametersByPathResponse_parameters :: Lens.Lens' GetParametersByPathResponse (Core.Maybe [Parameter])
getParametersByPathResponse_parameters = Lens.lens (\GetParametersByPathResponse' {parameters} -> parameters) (\s@GetParametersByPathResponse' {} a -> s {parameters = a} :: GetParametersByPathResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getParametersByPathResponse_httpStatus :: Lens.Lens' GetParametersByPathResponse Core.Int
getParametersByPathResponse_httpStatus = Lens.lens (\GetParametersByPathResponse' {httpStatus} -> httpStatus) (\s@GetParametersByPathResponse' {} a -> s {httpStatus = a} :: GetParametersByPathResponse)

instance Core.NFData GetParametersByPathResponse
