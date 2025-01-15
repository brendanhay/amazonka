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
-- Module      : Amazonka.KMS.UpdateCustomKeyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the properties of a custom key store. You can use this operation
-- to change the properties of an CloudHSM key store or an external key
-- store.
--
-- Use the required @CustomKeyStoreId@ parameter to identify the custom key
-- store. Use the remaining optional parameters to change its properties.
-- This operation does not return any property values. To verify the
-- updated property values, use the DescribeCustomKeyStores operation.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a key store that you own and
-- manage.
--
-- When updating the properties of an external key store, verify that the
-- updated settings connect your key store, via the external key store
-- proxy, to the same external key manager as the previous settings, or to
-- a backup or snapshot of the external key manager with the same
-- cryptographic keys. If the updated connection settings fail, you can fix
-- them and retry, although an extended delay might disrupt Amazon Web
-- Services services. However, if KMS permanently loses its access to
-- cryptographic keys, ciphertext encrypted under those keys is
-- unrecoverable.
--
-- For external key stores:
--
-- Some external key managers provide a simpler method for updating an
-- external key store. For details, see your external key manager
-- documentation.
--
-- When updating an external key store in the KMS console, you can upload a
-- JSON-based proxy configuration file with the desired values. You cannot
-- upload the proxy configuration file to the @UpdateCustomKeyStore@
-- operation. However, you can use the file to help you determine the
-- correct values for the @UpdateCustomKeyStore@ parameters.
--
-- For an CloudHSM key store, you can use this operation to change the
-- custom key store friendly name (@NewCustomKeyStoreName@), to tell KMS
-- about a change to the @kmsuser@ crypto user password
-- (@KeyStorePassword@), or to associate the custom key store with a
-- different, but related, CloudHSM cluster (@CloudHsmClusterId@). To
-- update any property of an CloudHSM key store, the @ConnectionState@ of
-- the CloudHSM key store must be @DISCONNECTED@.
--
-- For an external key store, you can use this operation to change the
-- custom key store friendly name (@NewCustomKeyStoreName@), or to tell KMS
-- about a change to the external key store proxy authentication
-- credentials (@XksProxyAuthenticationCredential@), connection method
-- (@XksProxyConnectivity@), external proxy endpoint
-- (@XksProxyUriEndpoint@) and path (@XksProxyUriPath@). For external key
-- stores with an @XksProxyConnectivity@ of @VPC_ENDPOINT_SERVICE@, you can
-- also update the Amazon VPC endpoint service name
-- (@XksProxyVpcEndpointServiceName@). To update most properties of an
-- external key store, the @ConnectionState@ of the external key store must
-- be @DISCONNECTED@. However, you can update the @CustomKeyStoreName@,
-- @XksProxyAuthenticationCredential@, and @XksProxyUriPath@ of an external
-- key store when it is in the CONNECTED or DISCONNECTED state.
--
-- If your update requires a @DISCONNECTED@ state, before using
-- @UpdateCustomKeyStore@, use the DisconnectCustomKeyStore operation to
-- disconnect the custom key store. After the @UpdateCustomKeyStore@
-- operation completes, use the ConnectCustomKeyStore to reconnect the
-- custom key store. To find the @ConnectionState@ of the custom key store,
-- use the DescribeCustomKeyStores operation.
--
-- Before updating the custom key store, verify that the new values allow
-- KMS to connect the custom key store to its backing key store. For
-- example, before you change the @XksProxyUriPath@ value, verify that the
-- external key store proxy is reachable at the new path.
--
-- If the operation succeeds, it returns a JSON object with no properties.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateCustomKeyStore>
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
-- -   DescribeCustomKeyStores
--
-- -   DisconnectCustomKeyStore
module Amazonka.KMS.UpdateCustomKeyStore
  ( -- * Creating a Request
    UpdateCustomKeyStore (..),
    newUpdateCustomKeyStore,

    -- * Request Lenses
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_xksProxyAuthenticationCredential,
    updateCustomKeyStore_xksProxyConnectivity,
    updateCustomKeyStore_xksProxyUriEndpoint,
    updateCustomKeyStore_xksProxyUriPath,
    updateCustomKeyStore_xksProxyVpcEndpointServiceName,
    updateCustomKeyStore_customKeyStoreId,

    -- * Destructuring the Response
    UpdateCustomKeyStoreResponse (..),
    newUpdateCustomKeyStoreResponse,

    -- * Response Lenses
    updateCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCustomKeyStore' smart constructor.
data UpdateCustomKeyStore = UpdateCustomKeyStore'
  { -- | Associates the custom key store with a related CloudHSM cluster. This
    -- parameter is valid only for custom key stores with a
    -- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
    --
    -- Enter the cluster ID of the cluster that you used to create the custom
    -- key store or a cluster that shares a backup history and has the same
    -- cluster certificate as the original cluster. You cannot use this
    -- parameter to associate a custom key store with an unrelated cluster. In
    -- addition, the replacement cluster must
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
    -- for a cluster associated with a custom key store. To view the cluster
    -- certificate of a cluster, use the
    -- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
    -- operation.
    --
    -- To change this value, the CloudHSM key store must be disconnected.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Enter the current password of the @kmsuser@ crypto user (CU) in the
    -- CloudHSM cluster that is associated with the custom key store. This
    -- parameter is valid only for custom key stores with a
    -- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
    --
    -- This parameter tells KMS the current password of the @kmsuser@ crypto
    -- user (CU). It does not set or change the password of any users in the
    -- CloudHSM cluster.
    --
    -- To change this value, the CloudHSM key store must be disconnected.
    keyStorePassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Changes the friendly name of the custom key store to the value that you
    -- specify. The custom key store name must be unique in the Amazon Web
    -- Services account.
    --
    -- To change this value, an CloudHSM key store must be disconnected. An
    -- external key store can be connected or disconnected.
    newCustomKeyStoreName' :: Prelude.Maybe Prelude.Text,
    -- | Changes the credentials that KMS uses to sign requests to the external
    -- key store proxy (XKS proxy). This parameter is valid only for custom key
    -- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- You must specify both the @AccessKeyId@ and @SecretAccessKey@ value in
    -- the authentication credential, even if you are only updating one value.
    --
    -- This parameter doesn\'t establish or change your authentication
    -- credentials on the proxy. It just tells KMS the credential that you
    -- established with your external key store proxy. For example, if you
    -- rotate the credential on your external key store proxy, you can use this
    -- parameter to update the credential in KMS.
    --
    -- You can change this value when the external key store is connected or
    -- disconnected.
    xksProxyAuthenticationCredential :: Prelude.Maybe XksProxyAuthenticationCredentialType,
    -- | Changes the connectivity setting for the external key store. To indicate
    -- that the external key store proxy uses a Amazon VPC endpoint service to
    -- communicate with KMS, specify @VPC_ENDPOINT_SERVICE@. Otherwise, specify
    -- @PUBLIC_ENDPOINT@.
    --
    -- If you change the @XksProxyConnectivity@ to @VPC_ENDPOINT_SERVICE@, you
    -- must also change the @XksProxyUriEndpoint@ and add an
    -- @XksProxyVpcEndpointServiceName@ value.
    --
    -- If you change the @XksProxyConnectivity@ to @PUBLIC_ENDPOINT@, you must
    -- also change the @XksProxyUriEndpoint@ and specify a null or empty string
    -- for the @XksProxyVpcEndpointServiceName@ value.
    --
    -- To change this value, the external key store must be disconnected.
    xksProxyConnectivity :: Prelude.Maybe XksProxyConnectivityType,
    -- | Changes the URI endpoint that KMS uses to connect to your external key
    -- store proxy (XKS proxy). This parameter is valid only for custom key
    -- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- For external key stores with an @XksProxyConnectivity@ value of
    -- @PUBLIC_ENDPOINT@, the protocol must be HTTPS.
    --
    -- For external key stores with an @XksProxyConnectivity@ value of
    -- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
    -- name associated with the VPC endpoint service. Each external key store
    -- must use a different private DNS name.
    --
    -- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
    -- unique in the Amazon Web Services account and Region.
    --
    -- To change this value, the external key store must be disconnected.
    xksProxyUriEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Changes the base path to the proxy APIs for this external key store. To
    -- find this value, see the documentation for your external key manager and
    -- external key store proxy (XKS proxy). This parameter is valid only for
    -- custom key stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- The value must start with @\/@ and must end with @\/kms\/xks\/v1@, where
    -- @v1@ represents the version of the KMS external key store proxy API. You
    -- can include an optional prefix between the required elements such as
    -- @\/@/@example@/@\/kms\/xks\/v1@.
    --
    -- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
    -- unique in the Amazon Web Services account and Region.
    --
    -- You can change this value when the external key store is connected or
    -- disconnected.
    xksProxyUriPath :: Prelude.Maybe Prelude.Text,
    -- | Changes the name that KMS uses to identify the Amazon VPC endpoint
    -- service for your external key store proxy (XKS proxy). This parameter is
    -- valid when the @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the
    -- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
    --
    -- To change this value, the external key store must be disconnected.
    xksProxyVpcEndpointServiceName :: Prelude.Maybe Prelude.Text,
    -- | Identifies the custom key store that you want to update. Enter the ID of
    -- the custom key store. To find the ID of a custom key store, use the
    -- DescribeCustomKeyStores operation.
    customKeyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudHsmClusterId', 'updateCustomKeyStore_cloudHsmClusterId' - Associates the custom key store with a related CloudHSM cluster. This
-- parameter is valid only for custom key stores with a
-- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
--
-- Enter the cluster ID of the cluster that you used to create the custom
-- key store or a cluster that shares a backup history and has the same
-- cluster certificate as the original cluster. You cannot use this
-- parameter to associate a custom key store with an unrelated cluster. In
-- addition, the replacement cluster must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
-- for a cluster associated with a custom key store. To view the cluster
-- certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- To change this value, the CloudHSM key store must be disconnected.
--
-- 'keyStorePassword', 'updateCustomKeyStore_keyStorePassword' - Enter the current password of the @kmsuser@ crypto user (CU) in the
-- CloudHSM cluster that is associated with the custom key store. This
-- parameter is valid only for custom key stores with a
-- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
--
-- This parameter tells KMS the current password of the @kmsuser@ crypto
-- user (CU). It does not set or change the password of any users in the
-- CloudHSM cluster.
--
-- To change this value, the CloudHSM key store must be disconnected.
--
-- 'newCustomKeyStoreName'', 'updateCustomKeyStore_newCustomKeyStoreName' - Changes the friendly name of the custom key store to the value that you
-- specify. The custom key store name must be unique in the Amazon Web
-- Services account.
--
-- To change this value, an CloudHSM key store must be disconnected. An
-- external key store can be connected or disconnected.
--
-- 'xksProxyAuthenticationCredential', 'updateCustomKeyStore_xksProxyAuthenticationCredential' - Changes the credentials that KMS uses to sign requests to the external
-- key store proxy (XKS proxy). This parameter is valid only for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- You must specify both the @AccessKeyId@ and @SecretAccessKey@ value in
-- the authentication credential, even if you are only updating one value.
--
-- This parameter doesn\'t establish or change your authentication
-- credentials on the proxy. It just tells KMS the credential that you
-- established with your external key store proxy. For example, if you
-- rotate the credential on your external key store proxy, you can use this
-- parameter to update the credential in KMS.
--
-- You can change this value when the external key store is connected or
-- disconnected.
--
-- 'xksProxyConnectivity', 'updateCustomKeyStore_xksProxyConnectivity' - Changes the connectivity setting for the external key store. To indicate
-- that the external key store proxy uses a Amazon VPC endpoint service to
-- communicate with KMS, specify @VPC_ENDPOINT_SERVICE@. Otherwise, specify
-- @PUBLIC_ENDPOINT@.
--
-- If you change the @XksProxyConnectivity@ to @VPC_ENDPOINT_SERVICE@, you
-- must also change the @XksProxyUriEndpoint@ and add an
-- @XksProxyVpcEndpointServiceName@ value.
--
-- If you change the @XksProxyConnectivity@ to @PUBLIC_ENDPOINT@, you must
-- also change the @XksProxyUriEndpoint@ and specify a null or empty string
-- for the @XksProxyVpcEndpointServiceName@ value.
--
-- To change this value, the external key store must be disconnected.
--
-- 'xksProxyUriEndpoint', 'updateCustomKeyStore_xksProxyUriEndpoint' - Changes the URI endpoint that KMS uses to connect to your external key
-- store proxy (XKS proxy). This parameter is valid only for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- For external key stores with an @XksProxyConnectivity@ value of
-- @PUBLIC_ENDPOINT@, the protocol must be HTTPS.
--
-- For external key stores with an @XksProxyConnectivity@ value of
-- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
-- name associated with the VPC endpoint service. Each external key store
-- must use a different private DNS name.
--
-- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
-- unique in the Amazon Web Services account and Region.
--
-- To change this value, the external key store must be disconnected.
--
-- 'xksProxyUriPath', 'updateCustomKeyStore_xksProxyUriPath' - Changes the base path to the proxy APIs for this external key store. To
-- find this value, see the documentation for your external key manager and
-- external key store proxy (XKS proxy). This parameter is valid only for
-- custom key stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The value must start with @\/@ and must end with @\/kms\/xks\/v1@, where
-- @v1@ represents the version of the KMS external key store proxy API. You
-- can include an optional prefix between the required elements such as
-- @\/@/@example@/@\/kms\/xks\/v1@.
--
-- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
-- unique in the Amazon Web Services account and Region.
--
-- You can change this value when the external key store is connected or
-- disconnected.
--
-- 'xksProxyVpcEndpointServiceName', 'updateCustomKeyStore_xksProxyVpcEndpointServiceName' - Changes the name that KMS uses to identify the Amazon VPC endpoint
-- service for your external key store proxy (XKS proxy). This parameter is
-- valid when the @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the
-- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
--
-- To change this value, the external key store must be disconnected.
--
-- 'customKeyStoreId', 'updateCustomKeyStore_customKeyStoreId' - Identifies the custom key store that you want to update. Enter the ID of
-- the custom key store. To find the ID of a custom key store, use the
-- DescribeCustomKeyStores operation.
newUpdateCustomKeyStore ::
  -- | 'customKeyStoreId'
  Prelude.Text ->
  UpdateCustomKeyStore
newUpdateCustomKeyStore pCustomKeyStoreId_ =
  UpdateCustomKeyStore'
    { cloudHsmClusterId =
        Prelude.Nothing,
      keyStorePassword = Prelude.Nothing,
      newCustomKeyStoreName' = Prelude.Nothing,
      xksProxyAuthenticationCredential = Prelude.Nothing,
      xksProxyConnectivity = Prelude.Nothing,
      xksProxyUriEndpoint = Prelude.Nothing,
      xksProxyUriPath = Prelude.Nothing,
      xksProxyVpcEndpointServiceName = Prelude.Nothing,
      customKeyStoreId = pCustomKeyStoreId_
    }

-- | Associates the custom key store with a related CloudHSM cluster. This
-- parameter is valid only for custom key stores with a
-- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
--
-- Enter the cluster ID of the cluster that you used to create the custom
-- key store or a cluster that shares a backup history and has the same
-- cluster certificate as the original cluster. You cannot use this
-- parameter to associate a custom key store with an unrelated cluster. In
-- addition, the replacement cluster must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore fulfill the requirements>
-- for a cluster associated with a custom key store. To view the cluster
-- certificate of a cluster, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- To change this value, the CloudHSM key store must be disconnected.
updateCustomKeyStore_cloudHsmClusterId :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_cloudHsmClusterId = Lens.lens (\UpdateCustomKeyStore' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@UpdateCustomKeyStore' {} a -> s {cloudHsmClusterId = a} :: UpdateCustomKeyStore)

-- | Enter the current password of the @kmsuser@ crypto user (CU) in the
-- CloudHSM cluster that is associated with the custom key store. This
-- parameter is valid only for custom key stores with a
-- @CustomKeyStoreType@ of @AWS_CLOUDHSM@.
--
-- This parameter tells KMS the current password of the @kmsuser@ crypto
-- user (CU). It does not set or change the password of any users in the
-- CloudHSM cluster.
--
-- To change this value, the CloudHSM key store must be disconnected.
updateCustomKeyStore_keyStorePassword :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_keyStorePassword = Lens.lens (\UpdateCustomKeyStore' {keyStorePassword} -> keyStorePassword) (\s@UpdateCustomKeyStore' {} a -> s {keyStorePassword = a} :: UpdateCustomKeyStore) Prelude.. Lens.mapping Data._Sensitive

-- | Changes the friendly name of the custom key store to the value that you
-- specify. The custom key store name must be unique in the Amazon Web
-- Services account.
--
-- To change this value, an CloudHSM key store must be disconnected. An
-- external key store can be connected or disconnected.
updateCustomKeyStore_newCustomKeyStoreName :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_newCustomKeyStoreName = Lens.lens (\UpdateCustomKeyStore' {newCustomKeyStoreName'} -> newCustomKeyStoreName') (\s@UpdateCustomKeyStore' {} a -> s {newCustomKeyStoreName' = a} :: UpdateCustomKeyStore)

-- | Changes the credentials that KMS uses to sign requests to the external
-- key store proxy (XKS proxy). This parameter is valid only for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- You must specify both the @AccessKeyId@ and @SecretAccessKey@ value in
-- the authentication credential, even if you are only updating one value.
--
-- This parameter doesn\'t establish or change your authentication
-- credentials on the proxy. It just tells KMS the credential that you
-- established with your external key store proxy. For example, if you
-- rotate the credential on your external key store proxy, you can use this
-- parameter to update the credential in KMS.
--
-- You can change this value when the external key store is connected or
-- disconnected.
updateCustomKeyStore_xksProxyAuthenticationCredential :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe XksProxyAuthenticationCredentialType)
updateCustomKeyStore_xksProxyAuthenticationCredential = Lens.lens (\UpdateCustomKeyStore' {xksProxyAuthenticationCredential} -> xksProxyAuthenticationCredential) (\s@UpdateCustomKeyStore' {} a -> s {xksProxyAuthenticationCredential = a} :: UpdateCustomKeyStore)

-- | Changes the connectivity setting for the external key store. To indicate
-- that the external key store proxy uses a Amazon VPC endpoint service to
-- communicate with KMS, specify @VPC_ENDPOINT_SERVICE@. Otherwise, specify
-- @PUBLIC_ENDPOINT@.
--
-- If you change the @XksProxyConnectivity@ to @VPC_ENDPOINT_SERVICE@, you
-- must also change the @XksProxyUriEndpoint@ and add an
-- @XksProxyVpcEndpointServiceName@ value.
--
-- If you change the @XksProxyConnectivity@ to @PUBLIC_ENDPOINT@, you must
-- also change the @XksProxyUriEndpoint@ and specify a null or empty string
-- for the @XksProxyVpcEndpointServiceName@ value.
--
-- To change this value, the external key store must be disconnected.
updateCustomKeyStore_xksProxyConnectivity :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe XksProxyConnectivityType)
updateCustomKeyStore_xksProxyConnectivity = Lens.lens (\UpdateCustomKeyStore' {xksProxyConnectivity} -> xksProxyConnectivity) (\s@UpdateCustomKeyStore' {} a -> s {xksProxyConnectivity = a} :: UpdateCustomKeyStore)

-- | Changes the URI endpoint that KMS uses to connect to your external key
-- store proxy (XKS proxy). This parameter is valid only for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- For external key stores with an @XksProxyConnectivity@ value of
-- @PUBLIC_ENDPOINT@, the protocol must be HTTPS.
--
-- For external key stores with an @XksProxyConnectivity@ value of
-- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
-- name associated with the VPC endpoint service. Each external key store
-- must use a different private DNS name.
--
-- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
-- unique in the Amazon Web Services account and Region.
--
-- To change this value, the external key store must be disconnected.
updateCustomKeyStore_xksProxyUriEndpoint :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_xksProxyUriEndpoint = Lens.lens (\UpdateCustomKeyStore' {xksProxyUriEndpoint} -> xksProxyUriEndpoint) (\s@UpdateCustomKeyStore' {} a -> s {xksProxyUriEndpoint = a} :: UpdateCustomKeyStore)

-- | Changes the base path to the proxy APIs for this external key store. To
-- find this value, see the documentation for your external key manager and
-- external key store proxy (XKS proxy). This parameter is valid only for
-- custom key stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The value must start with @\/@ and must end with @\/kms\/xks\/v1@, where
-- @v1@ represents the version of the KMS external key store proxy API. You
-- can include an optional prefix between the required elements such as
-- @\/@/@example@/@\/kms\/xks\/v1@.
--
-- The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must be
-- unique in the Amazon Web Services account and Region.
--
-- You can change this value when the external key store is connected or
-- disconnected.
updateCustomKeyStore_xksProxyUriPath :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_xksProxyUriPath = Lens.lens (\UpdateCustomKeyStore' {xksProxyUriPath} -> xksProxyUriPath) (\s@UpdateCustomKeyStore' {} a -> s {xksProxyUriPath = a} :: UpdateCustomKeyStore)

-- | Changes the name that KMS uses to identify the Amazon VPC endpoint
-- service for your external key store proxy (XKS proxy). This parameter is
-- valid when the @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the
-- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
--
-- To change this value, the external key store must be disconnected.
updateCustomKeyStore_xksProxyVpcEndpointServiceName :: Lens.Lens' UpdateCustomKeyStore (Prelude.Maybe Prelude.Text)
updateCustomKeyStore_xksProxyVpcEndpointServiceName = Lens.lens (\UpdateCustomKeyStore' {xksProxyVpcEndpointServiceName} -> xksProxyVpcEndpointServiceName) (\s@UpdateCustomKeyStore' {} a -> s {xksProxyVpcEndpointServiceName = a} :: UpdateCustomKeyStore)

-- | Identifies the custom key store that you want to update. Enter the ID of
-- the custom key store. To find the ID of a custom key store, use the
-- DescribeCustomKeyStores operation.
updateCustomKeyStore_customKeyStoreId :: Lens.Lens' UpdateCustomKeyStore Prelude.Text
updateCustomKeyStore_customKeyStoreId = Lens.lens (\UpdateCustomKeyStore' {customKeyStoreId} -> customKeyStoreId) (\s@UpdateCustomKeyStore' {} a -> s {customKeyStoreId = a} :: UpdateCustomKeyStore)

instance Core.AWSRequest UpdateCustomKeyStore where
  type
    AWSResponse UpdateCustomKeyStore =
      UpdateCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCustomKeyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCustomKeyStore where
  hashWithSalt _salt UpdateCustomKeyStore' {..} =
    _salt
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` keyStorePassword
      `Prelude.hashWithSalt` newCustomKeyStoreName'
      `Prelude.hashWithSalt` xksProxyAuthenticationCredential
      `Prelude.hashWithSalt` xksProxyConnectivity
      `Prelude.hashWithSalt` xksProxyUriEndpoint
      `Prelude.hashWithSalt` xksProxyUriPath
      `Prelude.hashWithSalt` xksProxyVpcEndpointServiceName
      `Prelude.hashWithSalt` customKeyStoreId

instance Prelude.NFData UpdateCustomKeyStore where
  rnf UpdateCustomKeyStore' {..} =
    Prelude.rnf cloudHsmClusterId `Prelude.seq`
      Prelude.rnf keyStorePassword `Prelude.seq`
        Prelude.rnf newCustomKeyStoreName' `Prelude.seq`
          Prelude.rnf xksProxyAuthenticationCredential `Prelude.seq`
            Prelude.rnf xksProxyConnectivity `Prelude.seq`
              Prelude.rnf xksProxyUriEndpoint `Prelude.seq`
                Prelude.rnf xksProxyUriPath `Prelude.seq`
                  Prelude.rnf xksProxyVpcEndpointServiceName `Prelude.seq`
                    Prelude.rnf customKeyStoreId

instance Data.ToHeaders UpdateCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.UpdateCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCustomKeyStore where
  toJSON UpdateCustomKeyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudHsmClusterId" Data..=)
              Prelude.<$> cloudHsmClusterId,
            ("KeyStorePassword" Data..=)
              Prelude.<$> keyStorePassword,
            ("NewCustomKeyStoreName" Data..=)
              Prelude.<$> newCustomKeyStoreName',
            ("XksProxyAuthenticationCredential" Data..=)
              Prelude.<$> xksProxyAuthenticationCredential,
            ("XksProxyConnectivity" Data..=)
              Prelude.<$> xksProxyConnectivity,
            ("XksProxyUriEndpoint" Data..=)
              Prelude.<$> xksProxyUriEndpoint,
            ("XksProxyUriPath" Data..=)
              Prelude.<$> xksProxyUriPath,
            ("XksProxyVpcEndpointServiceName" Data..=)
              Prelude.<$> xksProxyVpcEndpointServiceName,
            Prelude.Just
              ("CustomKeyStoreId" Data..= customKeyStoreId)
          ]
      )

instance Data.ToPath UpdateCustomKeyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomKeyStoreResponse' smart constructor.
data UpdateCustomKeyStoreResponse = UpdateCustomKeyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCustomKeyStoreResponse_httpStatus' - The response's http status code.
newUpdateCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomKeyStoreResponse
newUpdateCustomKeyStoreResponse pHttpStatus_ =
  UpdateCustomKeyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCustomKeyStoreResponse_httpStatus :: Lens.Lens' UpdateCustomKeyStoreResponse Prelude.Int
updateCustomKeyStoreResponse_httpStatus = Lens.lens (\UpdateCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: UpdateCustomKeyStoreResponse)

instance Prelude.NFData UpdateCustomKeyStoreResponse where
  rnf UpdateCustomKeyStoreResponse' {..} =
    Prelude.rnf httpStatus
