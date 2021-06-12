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
-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.ListIdentities
  ( -- * Creating a Request
    ListIdentities (..),
    newListIdentities,

    -- * Request Lenses
    listIdentities_nextToken,
    listIdentities_hideDisabled,
    listIdentities_identityPoolId,
    listIdentities_maxResults,

    -- * Destructuring the Response
    ListIdentitiesResponse (..),
    newListIdentitiesResponse,

    -- * Response Lenses
    listIdentitiesResponse_identityPoolId,
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_identities,
    listIdentitiesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the ListIdentities action.
--
-- /See:/ 'newListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | An optional boolean parameter that allows you to hide disabled
    -- identities. If omitted, the ListIdentities API will include disabled
    -- identities in the response.
    hideDisabled :: Core.Maybe Core.Bool,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Text,
    -- | The maximum number of identities to return.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentities_nextToken' - A pagination token.
--
-- 'hideDisabled', 'listIdentities_hideDisabled' - An optional boolean parameter that allows you to hide disabled
-- identities. If omitted, the ListIdentities API will include disabled
-- identities in the response.
--
-- 'identityPoolId', 'listIdentities_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'maxResults', 'listIdentities_maxResults' - The maximum number of identities to return.
newListIdentities ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'maxResults'
  Core.Natural ->
  ListIdentities
newListIdentities pIdentityPoolId_ pMaxResults_ =
  ListIdentities'
    { nextToken = Core.Nothing,
      hideDisabled = Core.Nothing,
      identityPoolId = pIdentityPoolId_,
      maxResults = pMaxResults_
    }

-- | A pagination token.
listIdentities_nextToken :: Lens.Lens' ListIdentities (Core.Maybe Core.Text)
listIdentities_nextToken = Lens.lens (\ListIdentities' {nextToken} -> nextToken) (\s@ListIdentities' {} a -> s {nextToken = a} :: ListIdentities)

-- | An optional boolean parameter that allows you to hide disabled
-- identities. If omitted, the ListIdentities API will include disabled
-- identities in the response.
listIdentities_hideDisabled :: Lens.Lens' ListIdentities (Core.Maybe Core.Bool)
listIdentities_hideDisabled = Lens.lens (\ListIdentities' {hideDisabled} -> hideDisabled) (\s@ListIdentities' {} a -> s {hideDisabled = a} :: ListIdentities)

-- | An identity pool ID in the format REGION:GUID.
listIdentities_identityPoolId :: Lens.Lens' ListIdentities Core.Text
listIdentities_identityPoolId = Lens.lens (\ListIdentities' {identityPoolId} -> identityPoolId) (\s@ListIdentities' {} a -> s {identityPoolId = a} :: ListIdentities)

-- | The maximum number of identities to return.
listIdentities_maxResults :: Lens.Lens' ListIdentities Core.Natural
listIdentities_maxResults = Lens.lens (\ListIdentities' {maxResults} -> maxResults) (\s@ListIdentities' {} a -> s {maxResults = a} :: ListIdentities)

instance Core.AWSRequest ListIdentities where
  type
    AWSResponse ListIdentities =
      ListIdentitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentitiesResponse'
            Core.<$> (x Core..?> "IdentityPoolId")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Identities" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListIdentities

instance Core.NFData ListIdentities

instance Core.ToHeaders ListIdentities where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.ListIdentities" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListIdentities where
  toJSON ListIdentities' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("HideDisabled" Core..=) Core.<$> hideDisabled,
            Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListIdentities where
  toPath = Core.const "/"

instance Core.ToQuery ListIdentities where
  toQuery = Core.const Core.mempty

-- | The response to a ListIdentities request.
--
-- /See:/ 'newListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Maybe Core.Text,
    -- | A pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | An object containing a set of identities and associated mappings.
    identities :: Core.Maybe [IdentityDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'listIdentitiesResponse_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'nextToken', 'listIdentitiesResponse_nextToken' - A pagination token.
--
-- 'identities', 'listIdentitiesResponse_identities' - An object containing a set of identities and associated mappings.
--
-- 'httpStatus', 'listIdentitiesResponse_httpStatus' - The response's http status code.
newListIdentitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIdentitiesResponse
newListIdentitiesResponse pHttpStatus_ =
  ListIdentitiesResponse'
    { identityPoolId =
        Core.Nothing,
      nextToken = Core.Nothing,
      identities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identity pool ID in the format REGION:GUID.
listIdentitiesResponse_identityPoolId :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Core.Text)
listIdentitiesResponse_identityPoolId = Lens.lens (\ListIdentitiesResponse' {identityPoolId} -> identityPoolId) (\s@ListIdentitiesResponse' {} a -> s {identityPoolId = a} :: ListIdentitiesResponse)

-- | A pagination token.
listIdentitiesResponse_nextToken :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Core.Text)
listIdentitiesResponse_nextToken = Lens.lens (\ListIdentitiesResponse' {nextToken} -> nextToken) (\s@ListIdentitiesResponse' {} a -> s {nextToken = a} :: ListIdentitiesResponse)

-- | An object containing a set of identities and associated mappings.
listIdentitiesResponse_identities :: Lens.Lens' ListIdentitiesResponse (Core.Maybe [IdentityDescription])
listIdentitiesResponse_identities = Lens.lens (\ListIdentitiesResponse' {identities} -> identities) (\s@ListIdentitiesResponse' {} a -> s {identities = a} :: ListIdentitiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIdentitiesResponse_httpStatus :: Lens.Lens' ListIdentitiesResponse Core.Int
listIdentitiesResponse_httpStatus = Lens.lens (\ListIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListIdentitiesResponse' {} a -> s {httpStatus = a} :: ListIdentitiesResponse)

instance Core.NFData ListIdentitiesResponse
