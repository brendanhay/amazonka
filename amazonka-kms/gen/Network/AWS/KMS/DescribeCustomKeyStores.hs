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
-- Module      : Network.AWS.KMS.DescribeCustomKeyStores
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- in the account and region.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html Custom Key Store feature>
-- feature in AWS KMS, which combines the convenience and extensive
-- integration of AWS KMS with the isolation and control of a single-tenant
-- key store.
--
-- By default, this operation returns information about all custom key
-- stores in the account and region. To get only information about a
-- particular custom key store, use either the @CustomKeyStoreName@ or
-- @CustomKeyStoreId@ parameter (but not both).
--
-- To determine whether the custom key store is connected to its AWS
-- CloudHSM cluster, use the @ConnectionState@ element in the response. If
-- an attempt to connect the custom key store failed, the @ConnectionState@
-- value is @FAILED@ and the @ConnectionErrorCode@ element in the response
-- indicates the cause of the failure. For help interpreting the
-- @ConnectionErrorCode@, see CustomKeyStoresListEntry.
--
-- Custom key stores have a @DISCONNECTED@ connection state if the key
-- store has never been connected or you use the DisconnectCustomKeyStore
-- operation to disconnect it. If your custom key store state is
-- @CONNECTED@ but you are having trouble using it, make sure that its
-- associated AWS CloudHSM cluster is active and contains the minimum
-- number of HSMs required for the operation, if any.
--
-- For help repairing your custom key store, see the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting Custom Key Stores>
-- topic in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different AWS account.
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
module Network.AWS.KMS.DescribeCustomKeyStores
  ( -- * Creating a Request
    DescribeCustomKeyStores (..),
    newDescribeCustomKeyStores,

    -- * Request Lenses
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStores_customKeyStoreId,
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

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCustomKeyStores' smart constructor.
data DescribeCustomKeyStores = DescribeCustomKeyStores'
  { -- | Gets only information about the specified custom key store. Enter the
    -- friendly name of the custom key store.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and region. To limit the output to a particular custom
    -- key store, you can use either the @CustomKeyStoreId@ or
    -- @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreName :: Core.Maybe Core.Text,
    -- | Gets only information about the specified custom key store. Enter the
    -- key store ID.
    --
    -- By default, this operation gets information about all custom key stores
    -- in the account and region. To limit the output to a particular custom
    -- key store, you can use either the @CustomKeyStoreId@ or
    -- @CustomKeyStoreName@ parameter, but not both.
    customKeyStoreId :: Core.Maybe Core.Text,
    -- | Use this parameter to specify the maximum number of items to return.
    -- When this value is present, AWS KMS does not return more than the
    -- specified number of items, but it might return fewer.
    limit :: Core.Maybe Core.Natural,
    -- | Use this parameter in a subsequent request after you receive a response
    -- with truncated results. Set it to the value of @NextMarker@ from the
    -- truncated response you just received.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomKeyStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreName', 'describeCustomKeyStores_customKeyStoreName' - Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
--
-- 'customKeyStoreId', 'describeCustomKeyStores_customKeyStoreId' - Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
--
-- 'limit', 'describeCustomKeyStores_limit' - Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
--
-- 'marker', 'describeCustomKeyStores_marker' - Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
newDescribeCustomKeyStores ::
  DescribeCustomKeyStores
newDescribeCustomKeyStores =
  DescribeCustomKeyStores'
    { customKeyStoreName =
        Core.Nothing,
      customKeyStoreId = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Gets only information about the specified custom key store. Enter the
-- friendly name of the custom key store.
--
-- By default, this operation gets information about all custom key stores
-- in the account and region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
describeCustomKeyStores_customKeyStoreName :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Core.Text)
describeCustomKeyStores_customKeyStoreName = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreName} -> customKeyStoreName) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreName = a} :: DescribeCustomKeyStores)

-- | Gets only information about the specified custom key store. Enter the
-- key store ID.
--
-- By default, this operation gets information about all custom key stores
-- in the account and region. To limit the output to a particular custom
-- key store, you can use either the @CustomKeyStoreId@ or
-- @CustomKeyStoreName@ parameter, but not both.
describeCustomKeyStores_customKeyStoreId :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Core.Text)
describeCustomKeyStores_customKeyStoreId = Lens.lens (\DescribeCustomKeyStores' {customKeyStoreId} -> customKeyStoreId) (\s@DescribeCustomKeyStores' {} a -> s {customKeyStoreId = a} :: DescribeCustomKeyStores)

-- | Use this parameter to specify the maximum number of items to return.
-- When this value is present, AWS KMS does not return more than the
-- specified number of items, but it might return fewer.
describeCustomKeyStores_limit :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Core.Natural)
describeCustomKeyStores_limit = Lens.lens (\DescribeCustomKeyStores' {limit} -> limit) (\s@DescribeCustomKeyStores' {} a -> s {limit = a} :: DescribeCustomKeyStores)

-- | Use this parameter in a subsequent request after you receive a response
-- with truncated results. Set it to the value of @NextMarker@ from the
-- truncated response you just received.
describeCustomKeyStores_marker :: Lens.Lens' DescribeCustomKeyStores (Core.Maybe Core.Text)
describeCustomKeyStores_marker = Lens.lens (\DescribeCustomKeyStores' {marker} -> marker) (\s@DescribeCustomKeyStores' {} a -> s {marker = a} :: DescribeCustomKeyStores)

instance Core.AWSRequest DescribeCustomKeyStores where
  type
    AWSResponse DescribeCustomKeyStores =
      DescribeCustomKeyStoresResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomKeyStoresResponse'
            Core.<$> (x Core..?> "CustomKeyStores" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "Truncated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCustomKeyStores

instance Core.NFData DescribeCustomKeyStores

instance Core.ToHeaders DescribeCustomKeyStores where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.DescribeCustomKeyStores" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCustomKeyStores where
  toJSON DescribeCustomKeyStores' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomKeyStoreName" Core..=)
              Core.<$> customKeyStoreName,
            ("CustomKeyStoreId" Core..=)
              Core.<$> customKeyStoreId,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath DescribeCustomKeyStores where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCustomKeyStores where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCustomKeyStoresResponse' smart constructor.
data DescribeCustomKeyStoresResponse = DescribeCustomKeyStoresResponse'
  { -- | Contains metadata about each custom key store.
    customKeyStores :: Core.Maybe [CustomKeyStoresListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Core.Maybe Core.Text,
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeCustomKeyStoresResponse
newDescribeCustomKeyStoresResponse pHttpStatus_ =
  DescribeCustomKeyStoresResponse'
    { customKeyStores =
        Core.Nothing,
      nextMarker = Core.Nothing,
      truncated = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains metadata about each custom key store.
describeCustomKeyStoresResponse_customKeyStores :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe [CustomKeyStoresListEntry])
describeCustomKeyStoresResponse_customKeyStores = Lens.lens (\DescribeCustomKeyStoresResponse' {customKeyStores} -> customKeyStores) (\s@DescribeCustomKeyStoresResponse' {} a -> s {customKeyStores = a} :: DescribeCustomKeyStoresResponse) Core.. Lens.mapping Lens._Coerce

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_nextMarker :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe Core.Text)
describeCustomKeyStoresResponse_nextMarker = Lens.lens (\DescribeCustomKeyStoresResponse' {nextMarker} -> nextMarker) (\s@DescribeCustomKeyStoresResponse' {} a -> s {nextMarker = a} :: DescribeCustomKeyStoresResponse)

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
describeCustomKeyStoresResponse_truncated :: Lens.Lens' DescribeCustomKeyStoresResponse (Core.Maybe Core.Bool)
describeCustomKeyStoresResponse_truncated = Lens.lens (\DescribeCustomKeyStoresResponse' {truncated} -> truncated) (\s@DescribeCustomKeyStoresResponse' {} a -> s {truncated = a} :: DescribeCustomKeyStoresResponse)

-- | The response's http status code.
describeCustomKeyStoresResponse_httpStatus :: Lens.Lens' DescribeCustomKeyStoresResponse Core.Int
describeCustomKeyStoresResponse_httpStatus = Lens.lens (\DescribeCustomKeyStoresResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomKeyStoresResponse' {} a -> s {httpStatus = a} :: DescribeCustomKeyStoresResponse)

instance Core.NFData DescribeCustomKeyStoresResponse
