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
-- Module      : Amazonka.CloudFront.ListPublicKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all public keys that have been added to CloudFront for this
-- account.
module Amazonka.CloudFront.ListPublicKeys
  ( -- * Creating a Request
    ListPublicKeys (..),
    newListPublicKeys,

    -- * Request Lenses
    listPublicKeys_marker,
    listPublicKeys_maxItems,

    -- * Destructuring the Response
    ListPublicKeysResponse (..),
    newListPublicKeysResponse,

    -- * Response Lenses
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Use this when paginating results to indicate where to begin in your list
    -- of public keys. The results include public keys in the list that occur
    -- after the marker. To get the next page of results, set the @Marker@ to
    -- the value of the @NextMarker@ from the current page\'s response (which
    -- is also the ID of the last public key on that page).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of public keys you want in the response body.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublicKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPublicKeys_marker' - Use this when paginating results to indicate where to begin in your list
-- of public keys. The results include public keys in the list that occur
-- after the marker. To get the next page of results, set the @Marker@ to
-- the value of the @NextMarker@ from the current page\'s response (which
-- is also the ID of the last public key on that page).
--
-- 'maxItems', 'listPublicKeys_maxItems' - The maximum number of public keys you want in the response body.
newListPublicKeys ::
  ListPublicKeys
newListPublicKeys =
  ListPublicKeys'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of public keys. The results include public keys in the list that occur
-- after the marker. To get the next page of results, set the @Marker@ to
-- the value of the @NextMarker@ from the current page\'s response (which
-- is also the ID of the last public key on that page).
listPublicKeys_marker :: Lens.Lens' ListPublicKeys (Prelude.Maybe Prelude.Text)
listPublicKeys_marker = Lens.lens (\ListPublicKeys' {marker} -> marker) (\s@ListPublicKeys' {} a -> s {marker = a} :: ListPublicKeys)

-- | The maximum number of public keys you want in the response body.
listPublicKeys_maxItems :: Lens.Lens' ListPublicKeys (Prelude.Maybe Prelude.Text)
listPublicKeys_maxItems = Lens.lens (\ListPublicKeys' {maxItems} -> maxItems) (\s@ListPublicKeys' {} a -> s {maxItems = a} :: ListPublicKeys)

instance Core.AWSRequest ListPublicKeys where
  type
    AWSResponse ListPublicKeys =
      ListPublicKeysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListPublicKeysResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPublicKeys where
  hashWithSalt _salt ListPublicKeys' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListPublicKeys where
  rnf ListPublicKeys' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance Core.ToHeaders ListPublicKeys where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPublicKeys where
  toPath = Prelude.const "/2020-05-31/public-key"

instance Core.ToQuery ListPublicKeys where
  toQuery ListPublicKeys' {..} =
    Prelude.mconcat
      [ "Marker" Core.=: marker,
        "MaxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Returns a list of all public keys that have been added to CloudFront for
    -- this account.
    publicKeyList :: Prelude.Maybe PublicKeyList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPublicKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKeyList', 'listPublicKeysResponse_publicKeyList' - Returns a list of all public keys that have been added to CloudFront for
-- this account.
--
-- 'httpStatus', 'listPublicKeysResponse_httpStatus' - The response's http status code.
newListPublicKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPublicKeysResponse
newListPublicKeysResponse pHttpStatus_ =
  ListPublicKeysResponse'
    { publicKeyList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of all public keys that have been added to CloudFront for
-- this account.
listPublicKeysResponse_publicKeyList :: Lens.Lens' ListPublicKeysResponse (Prelude.Maybe PublicKeyList)
listPublicKeysResponse_publicKeyList = Lens.lens (\ListPublicKeysResponse' {publicKeyList} -> publicKeyList) (\s@ListPublicKeysResponse' {} a -> s {publicKeyList = a} :: ListPublicKeysResponse)

-- | The response's http status code.
listPublicKeysResponse_httpStatus :: Lens.Lens' ListPublicKeysResponse Prelude.Int
listPublicKeysResponse_httpStatus = Lens.lens (\ListPublicKeysResponse' {httpStatus} -> httpStatus) (\s@ListPublicKeysResponse' {} a -> s {httpStatus = a} :: ListPublicKeysResponse)

instance Prelude.NFData ListPublicKeysResponse where
  rnf ListPublicKeysResponse' {..} =
    Prelude.rnf publicKeyList
      `Prelude.seq` Prelude.rnf httpStatus
