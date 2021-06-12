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
-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the identities (email addresses and
-- domains) for your AWS account in the current AWS Region, regardless of
-- verification status.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListIdentities
  ( -- * Creating a Request
    ListIdentities (..),
    newListIdentities,

    -- * Request Lenses
    listIdentities_nextToken,
    listIdentities_identityType,
    listIdentities_maxItems,

    -- * Destructuring the Response
    ListIdentitiesResponse (..),
    newListIdentitiesResponse,

    -- * Response Lenses
    listIdentitiesResponse_nextToken,
    listIdentitiesResponse_httpStatus,
    listIdentitiesResponse_identities,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return a list of all identities (email addresses
-- and domains) that you have attempted to verify under your AWS account,
-- regardless of verification status.
--
-- /See:/ 'newListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { -- | The token to use for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of the identities to list. Possible values are \"EmailAddress\"
    -- and \"Domain\". If this parameter is omitted, then all identities will
    -- be listed.
    identityType :: Core.Maybe IdentityType,
    -- | The maximum number of identities per page. Possible values are 1-1000
    -- inclusive.
    maxItems :: Core.Maybe Core.Int
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
-- 'nextToken', 'listIdentities_nextToken' - The token to use for pagination.
--
-- 'identityType', 'listIdentities_identityType' - The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
--
-- 'maxItems', 'listIdentities_maxItems' - The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
newListIdentities ::
  ListIdentities
newListIdentities =
  ListIdentities'
    { nextToken = Core.Nothing,
      identityType = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The token to use for pagination.
listIdentities_nextToken :: Lens.Lens' ListIdentities (Core.Maybe Core.Text)
listIdentities_nextToken = Lens.lens (\ListIdentities' {nextToken} -> nextToken) (\s@ListIdentities' {} a -> s {nextToken = a} :: ListIdentities)

-- | The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
listIdentities_identityType :: Lens.Lens' ListIdentities (Core.Maybe IdentityType)
listIdentities_identityType = Lens.lens (\ListIdentities' {identityType} -> identityType) (\s@ListIdentities' {} a -> s {identityType = a} :: ListIdentities)

-- | The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
listIdentities_maxItems :: Lens.Lens' ListIdentities (Core.Maybe Core.Int)
listIdentities_maxItems = Lens.lens (\ListIdentities' {maxItems} -> maxItems) (\s@ListIdentities' {} a -> s {maxItems = a} :: ListIdentities)

instance Core.AWSPager ListIdentities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentitiesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listIdentitiesResponse_identities) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listIdentities_nextToken
          Lens..~ rs
          Lens.^? listIdentitiesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListIdentities where
  type
    AWSResponse ListIdentities =
      ListIdentitiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListIdentitiesResult"
      ( \s h x ->
          ListIdentitiesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Identities" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable ListIdentities

instance Core.NFData ListIdentities

instance Core.ToHeaders ListIdentities where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListIdentities where
  toPath = Core.const "/"

instance Core.ToQuery ListIdentities where
  toQuery ListIdentities' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListIdentities" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "IdentityType" Core.=: identityType,
        "MaxItems" Core.=: maxItems
      ]

-- | A list of all identities that you have attempted to verify under your
-- AWS account, regardless of verification status.
--
-- /See:/ 'newListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { -- | The token used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of identities.
    identities :: [Core.Text]
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
-- 'nextToken', 'listIdentitiesResponse_nextToken' - The token used for pagination.
--
-- 'httpStatus', 'listIdentitiesResponse_httpStatus' - The response's http status code.
--
-- 'identities', 'listIdentitiesResponse_identities' - A list of identities.
newListIdentitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListIdentitiesResponse
newListIdentitiesResponse pHttpStatus_ =
  ListIdentitiesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      identities = Core.mempty
    }

-- | The token used for pagination.
listIdentitiesResponse_nextToken :: Lens.Lens' ListIdentitiesResponse (Core.Maybe Core.Text)
listIdentitiesResponse_nextToken = Lens.lens (\ListIdentitiesResponse' {nextToken} -> nextToken) (\s@ListIdentitiesResponse' {} a -> s {nextToken = a} :: ListIdentitiesResponse)

-- | The response's http status code.
listIdentitiesResponse_httpStatus :: Lens.Lens' ListIdentitiesResponse Core.Int
listIdentitiesResponse_httpStatus = Lens.lens (\ListIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListIdentitiesResponse' {} a -> s {httpStatus = a} :: ListIdentitiesResponse)

-- | A list of identities.
listIdentitiesResponse_identities :: Lens.Lens' ListIdentitiesResponse [Core.Text]
listIdentitiesResponse_identities = Lens.lens (\ListIdentitiesResponse' {identities} -> identities) (\s@ListIdentitiesResponse' {} a -> s {identities = a} :: ListIdentitiesResponse) Core.. Lens._Coerce

instance Core.NFData ListIdentitiesResponse
