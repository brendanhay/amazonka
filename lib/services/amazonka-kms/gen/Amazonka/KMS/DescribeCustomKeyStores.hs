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
-- Module      : Amazonka.KMS.DescribeCustomKeyStores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- in the account and Region.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store feature>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a single-tenant key store.
--
-- By default, this operation returns information about all custom key
-- stores in the account and Region. To get only information about a
-- particular custom key store, use either the @CustomKeyStoreName@ or
-- @CustomKeyStoreId@ parameter (but not both).
--
-- To determine whether the custom key store is connected to its CloudHSM
-- cluster, use the @ConnectionState@ element in the response. If an
-- attempt to connect the custom key store failed, the @ConnectionState@
-- value is @FAILED@ and the @ConnectionErrorCode@ element in the response
-- indicates the cause of the failure. For help interpreting the
-- @ConnectionErrorCode@, see CustomKeyStoresListEntry.
--
-- Custom key stores have a @DISCONNECTED@ connection state if the key
-- store has never been connected or you use the DisconnectCustomKeyStore
-- operation to disconnect it. If your custom key store state is
-- @CONNECTED@ but you are having trouble using it, make sure that its
-- associated CloudHSM cluster is active and contains the minimum number of
-- HSMs required for the operation, if any.
--
-- For help repairing your custom key store, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting Custom Key Stores>
-- topic in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DescribeCustomKeyStores>
-- (IAM policy)
--
-- __Related operations:__
--
-- -   ConnectCustomKeyStore
--
-- -   CreateCustomKeyStore
--
-- -   DeleteCustomKeyStore
--
-- -   DisconnectCustomKeyStore
--
-- -   UpdateCustomKeyStore
--
-- This operation returns paginated results.
module Amazonka.KMS.DescribeCustomKeyStores
  ( -- * Creating a Request
    DescribeCustomKeyStores (..),
    newDescribeCustomKeyStores,

    -- * Request Lenses
    describeCustomKeyStores_marker,
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_customKeyStoreName,

    -- * Destructuring the Response
    DescribeCustomKeyStoresResponse (..),
    newDescribeCustomKeyStoresResponse,

    -- * Response Lenses
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Gets only information about the specified custom key store. Enter the
    -- key store ID.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and Region. To limit the output to a particular custom
    -- key store, you can use either the @CustomKeyStoreId@ or
    -- @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Gets only information about the specified custom key store. Enter the
    -- friendly name of the custom key store.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and Region. To limit the output to a particular custom
    -- key store, you can use either the @CustomKeyStoreId@ or
    -- @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomKeyStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCustomKeyStores_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
--
-- 'customKeyStoreId', 'describeCustomKeyStores_customKeyStoreId' - Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
--
-- 'limit', 'describeCustomKeyStores_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- 'customKeyStoreName', 'describeCustomKeyStores_customKeyStoreName' - Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
newDescribeCustomKeyStores ::
  DescribeCustomKeyStores
newDescribeCustomKeyStores =
  DescribeCustomKeyStores'
    { marker = Prelude.Nothing,
      customKeyStoreId = Prelude.Nothing,
      limit = Prelude.Nothing,
      customKeyStoreName = Prelude.Nothing
    }

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
describeCustomKeyStores_marker :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_marker = Lens.lens (\DescribeCustomKeyStores' {marker} -> marker) (\s@DescribeCustomKeyStores' {} a -> s {marker = a} :: DescribeCustomKeyStores)

-- | Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
describeCustomKeyStores_customKeyStoreId :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_customKeyStoreId = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreId} -> customKeyStoreId) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreId = a} :: DescribeCustomKeyStores)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
describeCustomKeyStores_limit :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Natural)
describeCustomKeyStores_limit = Lens.lens (\DescribeCustomKeyStores' {limit} -> limit) (\s@DescribeCustomKeyStores' {} a -> s {limit = a} :: DescribeCustomKeyStores)

-- | Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
describeCustomKeyStores_customKeyStoreName :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_customKeyStoreName = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreName} -> customKeyStoreName) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreName = a} :: DescribeCustomKeyStores)

instance Core.AWSPager DescribeCustomKeyStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCustomKeyStoresResponse_truncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? describeCustomKeyStoresResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCustomKeyStores_marker
          Lens..~ rs
          Lens.^? describeCustomKeyStoresResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCustomKeyStores where
  type
    AWSResponse DescribeCustomKeyStores =
      DescribeCustomKeyStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomKeyStoresResponse'
            Prelude.<$> (x Core..?> "Truncated")
            Prelude.<*> ( x Core..?> "CustomKeyStores"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomKeyStores where
  hashWithSalt _salt DescribeCustomKeyStores' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` customKeyStoreName

instance Prelude.NFData DescribeCustomKeyStores where
  rnf DescribeCustomKeyStores' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf customKeyStoreName

instance Core.ToHeaders DescribeCustomKeyStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.DescribeCustomKeyStores" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCustomKeyStores where
  toJSON DescribeCustomKeyStores' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("CustomKeyStoreId" Core..=)
              Prelude.<$> customKeyStoreId,
            ("Limit" Core..=) Prelude.<$> limit,
            ("CustomKeyStoreName" Core..=)
              Prelude.<$> customKeyStoreName
          ]
      )

instance Core.ToPath DescribeCustomKeyStores where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCustomKeyStores where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | Contains metadata about each custom key store.
    customKeyStores :: Prelude.Maybe [CustomKeyStoresListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomKeyStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'describeCustomKeyStoresResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'customKeyStores', 'describeCustomKeyStoresResponse_customKeyStores' - Contains metadata about each custom key store.
--
-- 'nextMarker', 'describeCustomKeyStoresResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'describeCustomKeyStoresResponse_httpStatus' - The response's http status code.
newDescribeCustomKeyStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomKeyStoresResponse
newDescribeCustomKeyStoresResponse pHttpStatus_ =
  DescribeCustomKeyStoresResponse'
    { truncated =
        Prelude.Nothing,
      customKeyStores = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_truncated :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe Prelude.Bool)
describeCustomKeyStoresResponse_truncated = Lens.lens (\DescribeCustomKeyStoresResponse' {truncated} -> truncated) (\s@DescribeCustomKeyStoresResponse' {} a -> s {truncated = a} :: DescribeCustomKeyStoresResponse)

-- | Contains metadata about each custom key store.
describeCustomKeyStoresResponse_customKeyStores :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe [CustomKeyStoresListEntry])
describeCustomKeyStoresResponse_customKeyStores = Lens.lens (\DescribeCustomKeyStoresResponse' {customKeyStores} -> customKeyStores) (\s@DescribeCustomKeyStoresResponse' {} a -> s {customKeyStores = a} :: DescribeCustomKeyStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_nextMarker :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe Prelude.Text)
describeCustomKeyStoresResponse_nextMarker = Lens.lens (\DescribeCustomKeyStoresResponse' {nextMarker} -> nextMarker) (\s@DescribeCustomKeyStoresResponse' {} a -> s {nextMarker = a} :: DescribeCustomKeyStoresResponse)

-- | The response's http status code.
describeCustomKeyStoresResponse_httpStatus :: Lens.Lens' DescribeCustomKeyStoresResponse Prelude.Int
describeCustomKeyStoresResponse_httpStatus = Lens.lens (\DescribeCustomKeyStoresResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomKeyStoresResponse' {} a -> s {httpStatus = a} :: DescribeCustomKeyStoresResponse)

instance
  Prelude.NFData
    DescribeCustomKeyStoresResponse
  where
  rnf DescribeCustomKeyStoresResponse' {..} =
    Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf customKeyStores
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
