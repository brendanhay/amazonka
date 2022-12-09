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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a key store that you own and
-- manage.
--
-- By default, this operation returns information about all custom key
-- stores in the account and Region. To get only information about a
-- particular custom key store, use either the @CustomKeyStoreName@ or
-- @CustomKeyStoreId@ parameter (but not both).
--
-- To determine whether the custom key store is connected to its CloudHSM
-- cluster or external key store proxy, use the @ConnectionState@ element
-- in the response. If an attempt to connect the custom key store failed,
-- the @ConnectionState@ value is @FAILED@ and the @ConnectionErrorCode@
-- element in the response indicates the cause of the failure. For help
-- interpreting the @ConnectionErrorCode@, see CustomKeyStoresListEntry.
--
-- Custom key stores have a @DISCONNECTED@ connection state if the key
-- store has never been connected or you used the DisconnectCustomKeyStore
-- operation to disconnect it. Otherwise, the connection state is
-- CONNECTED. If your custom key store connection state is @CONNECTED@ but
-- you are having trouble using it, verify that the backing store is active
-- and available. For an CloudHSM key store, verify that the associated
-- CloudHSM cluster is active and contains the minimum number of HSMs
-- required for the operation, if any. For an external key store, verify
-- that the external key store proxy and its associated external key
-- manager are reachable and enabled.
--
-- For help repairing your CloudHSM key store, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting CloudHSM key stores>.
-- For help repairing your external key store, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/xks-troubleshooting.html Troubleshooting external key stores>.
-- Both topics are in the /Key Management Service Developer Guide/.
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
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_marker,

    -- * Destructuring the Response
    DescribeCustomKeyStoresResponse (..),
    newDescribeCustomKeyStoresResponse,

    -- * Response Lenses
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { -- | Gets only information about the specified custom key store. Enter the
    -- key store ID.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and Region. To limit the output to a particular custom
    -- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
    -- parameter, but not both.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | Gets only information about the specified custom key store. Enter the
    -- friendly name of the custom key store.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and Region. To limit the output to a particular custom
    -- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
    -- parameter, but not both.
    customKeyStoreName :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, KMS does not return more than the specified
    -- number of items, but it might return fewer.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'customKeyStoreId', 'describeCustomKeyStores_customKeyStoreId' - Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
-- parameter, but not both.
--
-- 'customKeyStoreName', 'describeCustomKeyStores_customKeyStoreName' - Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
-- parameter, but not both.
--
-- 'limit', 'describeCustomKeyStores_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
--
-- 'marker', 'describeCustomKeyStores_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
newDescribeCustomKeyStores ::
  DescribeCustomKeyStores
newDescribeCustomKeyStores =
  DescribeCustomKeyStores'
    { customKeyStoreId =
        Prelude.Nothing,
      customKeyStoreName = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
-- parameter, but not both.
describeCustomKeyStores_customKeyStoreId :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_customKeyStoreId = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreId} -> customKeyStoreId) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreId = a} :: DescribeCustomKeyStores)

-- | Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and Region. To limit the output to a particular custom
-- key store, provide either the @CustomKeyStoreId@ or @CustomKeyStoreName@
-- parameter, but not both.
describeCustomKeyStores_customKeyStoreName :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_customKeyStoreName = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreName} -> customKeyStoreName) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreName = a} :: DescribeCustomKeyStores)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, KMS does not return more than the specified
-- number of items, but it might return fewer.
describeCustomKeyStores_limit :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Natural)
describeCustomKeyStores_limit = Lens.lens (\DescribeCustomKeyStores' {limit} -> limit) (\s@DescribeCustomKeyStores' {} a -> s {limit = a} :: DescribeCustomKeyStores)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
describeCustomKeyStores_marker :: Lens.Lens' DescribeCustomKeyStores (Prelude.Maybe Prelude.Text)
describeCustomKeyStores_marker = Lens.lens (\DescribeCustomKeyStores' {marker} -> marker) (\s@DescribeCustomKeyStores' {} a -> s {marker = a} :: DescribeCustomKeyStores)

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
            Prelude.<$> ( x Data..?> "CustomKeyStores"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "Truncated")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomKeyStores where
  hashWithSalt _salt DescribeCustomKeyStores' {..} =
    _salt `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` customKeyStoreName
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker

instance Prelude.NFData DescribeCustomKeyStores where
  rnf DescribeCustomKeyStores' {..} =
    Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf customKeyStoreName
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker

instance Data.ToHeaders DescribeCustomKeyStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.DescribeCustomKeyStores" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCustomKeyStores where
  toJSON DescribeCustomKeyStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomKeyStoreId" Data..=)
              Prelude.<$> customKeyStoreId,
            ("CustomKeyStoreName" Data..=)
              Prelude.<$> customKeyStoreName,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker
          ]
      )

instance Data.ToPath DescribeCustomKeyStores where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCustomKeyStores where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { -- | Contains metadata about each custom key store.
    customKeyStores :: Prelude.Maybe [CustomKeyStoresListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomKeyStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStores', 'describeCustomKeyStoresResponse_customKeyStores' - Contains metadata about each custom key store.
--
-- 'nextMarker', 'describeCustomKeyStoresResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'truncated', 'describeCustomKeyStoresResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'httpStatus', 'describeCustomKeyStoresResponse_httpStatus' - The response's http status code.
newDescribeCustomKeyStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomKeyStoresResponse
newDescribeCustomKeyStoresResponse pHttpStatus_ =
  DescribeCustomKeyStoresResponse'
    { customKeyStores =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      truncated = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains metadata about each custom key store.
describeCustomKeyStoresResponse_customKeyStores :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe [CustomKeyStoresListEntry])
describeCustomKeyStoresResponse_customKeyStores = Lens.lens (\DescribeCustomKeyStoresResponse' {customKeyStores} -> customKeyStores) (\s@DescribeCustomKeyStoresResponse' {} a -> s {customKeyStores = a} :: DescribeCustomKeyStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_nextMarker :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe Prelude.Text)
describeCustomKeyStoresResponse_nextMarker = Lens.lens (\DescribeCustomKeyStoresResponse' {nextMarker} -> nextMarker) (\s@DescribeCustomKeyStoresResponse' {} a -> s {nextMarker = a} :: DescribeCustomKeyStoresResponse)

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_truncated :: Lens.Lens' DescribeCustomKeyStoresResponse (Prelude.Maybe Prelude.Bool)
describeCustomKeyStoresResponse_truncated = Lens.lens (\DescribeCustomKeyStoresResponse' {truncated} -> truncated) (\s@DescribeCustomKeyStoresResponse' {} a -> s {truncated = a} :: DescribeCustomKeyStoresResponse)

-- | The response's http status code.
describeCustomKeyStoresResponse_httpStatus :: Lens.Lens' DescribeCustomKeyStoresResponse Prelude.Int
describeCustomKeyStoresResponse_httpStatus = Lens.lens (\DescribeCustomKeyStoresResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomKeyStoresResponse' {} a -> s {httpStatus = a} :: DescribeCustomKeyStoresResponse)

instance
  Prelude.NFData
    DescribeCustomKeyStoresResponse
  where
  rnf DescribeCustomKeyStoresResponse' {..} =
    Prelude.rnf customKeyStores
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf httpStatus
