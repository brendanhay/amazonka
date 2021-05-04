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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of the identities to list. Possible values are \"EmailAddress\"
    -- and \"Domain\". If this parameter is omitted, then all identities will
    -- be listed.
    identityType :: Prelude.Maybe IdentityType,
    -- | The maximum number of identities per page. Possible values are 1-1000
    -- inclusive.
    maxItems :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      identityType = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | The token to use for pagination.
listIdentities_nextToken :: Lens.Lens' ListIdentities (Prelude.Maybe Prelude.Text)
listIdentities_nextToken = Lens.lens (\ListIdentities' {nextToken} -> nextToken) (\s@ListIdentities' {} a -> s {nextToken = a} :: ListIdentities)

-- | The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
listIdentities_identityType :: Lens.Lens' ListIdentities (Prelude.Maybe IdentityType)
listIdentities_identityType = Lens.lens (\ListIdentities' {identityType} -> identityType) (\s@ListIdentities' {} a -> s {identityType = a} :: ListIdentities)

-- | The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
listIdentities_maxItems :: Lens.Lens' ListIdentities (Prelude.Maybe Prelude.Int)
listIdentities_maxItems = Lens.lens (\ListIdentities' {maxItems} -> maxItems) (\s@ListIdentities' {} a -> s {maxItems = a} :: ListIdentities)

instance Pager.AWSPager ListIdentities where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listIdentitiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listIdentitiesResponse_identities) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listIdentities_nextToken
          Lens..~ rs
          Lens.^? listIdentitiesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListIdentities where
  type Rs ListIdentities = ListIdentitiesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListIdentitiesResult"
      ( \s h x ->
          ListIdentitiesResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "Identities"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListIdentities

instance Prelude.NFData ListIdentities

instance Prelude.ToHeaders ListIdentities where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListIdentities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListIdentities where
  toQuery ListIdentities' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListIdentities" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "IdentityType" Prelude.=: identityType,
        "MaxItems" Prelude.=: maxItems
      ]

-- | A list of all identities that you have attempted to verify under your
-- AWS account, regardless of verification status.
--
-- /See:/ 'newListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { -- | The token used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of identities.
    identities :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListIdentitiesResponse
newListIdentitiesResponse pHttpStatus_ =
  ListIdentitiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      identities = Prelude.mempty
    }

-- | The token used for pagination.
listIdentitiesResponse_nextToken :: Lens.Lens' ListIdentitiesResponse (Prelude.Maybe Prelude.Text)
listIdentitiesResponse_nextToken = Lens.lens (\ListIdentitiesResponse' {nextToken} -> nextToken) (\s@ListIdentitiesResponse' {} a -> s {nextToken = a} :: ListIdentitiesResponse)

-- | The response's http status code.
listIdentitiesResponse_httpStatus :: Lens.Lens' ListIdentitiesResponse Prelude.Int
listIdentitiesResponse_httpStatus = Lens.lens (\ListIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListIdentitiesResponse' {} a -> s {httpStatus = a} :: ListIdentitiesResponse)

-- | A list of identities.
listIdentitiesResponse_identities :: Lens.Lens' ListIdentitiesResponse [Prelude.Text]
listIdentitiesResponse_identities = Lens.lens (\ListIdentitiesResponse' {identities} -> identities) (\s@ListIdentitiesResponse' {} a -> s {identities = a} :: ListIdentitiesResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListIdentitiesResponse
