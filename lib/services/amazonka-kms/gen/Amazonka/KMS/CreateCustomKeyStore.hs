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
-- Module      : Amazonka.KMS.CreateCustomKeyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>
-- backed by a key store that you own and manage. When you use a KMS key in
-- a custom key store for a cryptographic operation, the cryptographic
-- operation is actually performed in your key store using your keys. KMS
-- supports
-- <https://docs.aws.amazon.com/kms/latest/developerguide/keystore-cloudhsm.html CloudHSM key stores>
-- backed by an
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/clusters.html CloudHSM cluster>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/keystore-external.html external key stores>
-- backed by an external key store proxy and external key manager outside
-- of Amazon Web Services.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key stores>
-- feature in KMS, which combines the convenience and extensive integration
-- of KMS with the isolation and control of a key store that you own and
-- manage.
--
-- Before you create the custom key store, the required elements must be in
-- place and operational. We recommend that you use the test tools that KMS
-- provides to verify the configuration your external key store proxy. For
-- details about the required elements and verification tests, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keystore.html#before-keystore Assemble the prerequisites (for CloudHSM key stores)>
-- or
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-xks-keystore.html#xks-requirements Assemble the prerequisites (for external key stores)>
-- in the /Key Management Service Developer Guide/.
--
-- To create a custom key store, use the following parameters.
--
-- -   To create an CloudHSM key store, specify the @CustomKeyStoreName@,
--     @CloudHsmClusterId@, @KeyStorePassword@, and
--     @TrustAnchorCertificate@. The @CustomKeyStoreType@ parameter is
--     optional for CloudHSM key stores. If you include it, set it to the
--     default value, @AWS_CLOUDHSM@. For help with failures, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting an CloudHSM key store>
--     in the /Key Management Service Developer Guide/.
--
-- -   To create an external key store, specify the @CustomKeyStoreName@
--     and a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@. Also, specify
--     values for @XksProxyConnectivity@,
--     @XksProxyAuthenticationCredential@, @XksProxyUriEndpoint@, and
--     @XksProxyUriPath@. If your @XksProxyConnectivity@ value is
--     @VPC_ENDPOINT_SERVICE@, specify the @XksProxyVpcEndpointServiceName@
--     parameter. For help with failures, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/xks-troubleshooting.html Troubleshooting an external key store>
--     in the /Key Management Service Developer Guide/.
--
-- For external key stores:
--
-- Some external key managers provide a simpler method for creating an
-- external key store. For details, see your external key manager
-- documentation.
--
-- When creating an external key store in the KMS console, you can upload a
-- JSON-based proxy configuration file with the desired values. You cannot
-- use a proxy configuration with the @CreateCustomKeyStore@ operation.
-- However, you can use the values in the file to help you determine the
-- correct values for the @CreateCustomKeyStore@ parameters.
--
-- When the operation completes successfully, it returns the ID of the new
-- custom key store. Before you can use your new custom key store, you need
-- to use the ConnectCustomKeyStore operation to connect a new CloudHSM key
-- store to its CloudHSM cluster, or to connect a new external key store to
-- the external key store proxy for your external key manager. Even if you
-- are not going to use your custom key store immediately, you might want
-- to connect it to verify that all settings are correct and then
-- disconnect it until you are ready to use it.
--
-- For help with failures, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a custom key store>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a custom
-- key store in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateCustomKeyStore>
-- (IAM policy).
--
-- __Related operations:__
--
-- -   ConnectCustomKeyStore
--
-- -   DeleteCustomKeyStore
--
-- -   DescribeCustomKeyStores
--
-- -   DisconnectCustomKeyStore
--
-- -   UpdateCustomKeyStore
module Amazonka.KMS.CreateCustomKeyStore
  ( -- * Creating a Request
    CreateCustomKeyStore (..),
    newCreateCustomKeyStore,

    -- * Request Lenses
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_customKeyStoreType,
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_xksProxyAuthenticationCredential,
    createCustomKeyStore_xksProxyConnectivity,
    createCustomKeyStore_xksProxyUriEndpoint,
    createCustomKeyStore_xksProxyUriPath,
    createCustomKeyStore_xksProxyVpcEndpointServiceName,
    createCustomKeyStore_customKeyStoreName,

    -- * Destructuring the Response
    CreateCustomKeyStoreResponse (..),
    newCreateCustomKeyStoreResponse,

    -- * Response Lenses
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomKeyStore' smart constructor.
data CreateCustomKeyStore = CreateCustomKeyStore'
  { -- | Identifies the CloudHSM cluster for an CloudHSM key store. This
    -- parameter is required for custom key stores with @CustomKeyStoreType@ of
    -- @AWS_CLOUDHSM@.
    --
    -- Enter the cluster ID of any active CloudHSM cluster that is not already
    -- associated with a custom key store. To find the cluster ID, use the
    -- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
    -- operation.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of custom key store. The default value is
    -- @AWS_CLOUDHSM@.
    --
    -- For a custom key store backed by an CloudHSM cluster, omit the parameter
    -- or enter @AWS_CLOUDHSM@. For a custom key store backed by an external
    -- key manager outside of Amazon Web Services, enter @EXTERNAL_KEY_STORE@.
    -- You cannot change this property after the key store is created.
    customKeyStoreType :: Prelude.Maybe CustomKeyStoreType,
    -- | Specifies the @kmsuser@ password for an CloudHSM key store. This
    -- parameter is required for custom key stores with a @CustomKeyStoreType@
    -- of @AWS_CLOUDHSM@.
    --
    -- Enter the password of the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
    -- in the specified CloudHSM cluster. KMS logs into the cluster as this
    -- user to manage key material on your behalf.
    --
    -- The password must be a string of 7 to 32 characters. Its value is case
    -- sensitive.
    --
    -- This parameter tells KMS the @kmsuser@ account password; it does not
    -- change the password in the CloudHSM cluster.
    keyStorePassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the certificate for an CloudHSM key store. This parameter is
    -- required for custom key stores with a @CustomKeyStoreType@ of
    -- @AWS_CLOUDHSM@.
    --
    -- Enter the content of the trust anchor certificate for the CloudHSM
    -- cluster. This is the content of the @customerCA.crt@ file that you
    -- created when you
    -- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
    trustAnchorCertificate :: Prelude.Maybe Prelude.Text,
    -- | Specifies an authentication credential for the external key store proxy
    -- (XKS proxy). This parameter is required for all custom key stores with a
    -- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- The @XksProxyAuthenticationCredential@ has two required elements:
    -- @RawSecretAccessKey@, a secret key, and @AccessKeyId@, a unique
    -- identifier for the @RawSecretAccessKey@. For character requirements, see
    -- <kms/latest/APIReference/API_XksProxyAuthenticationCredentialType.html XksProxyAuthenticationCredentialType>.
    --
    -- KMS uses this authentication credential to sign requests to the external
    -- key store proxy on your behalf. This credential is unrelated to Identity
    -- and Access Management (IAM) and Amazon Web Services credentials.
    --
    -- This parameter doesn\'t set or change the authentication credentials on
    -- the XKS proxy. It just tells KMS the credential that you established on
    -- your external key store proxy. If you rotate your proxy authentication
    -- credential, use the UpdateCustomKeyStore operation to provide the new
    -- credential to KMS.
    xksProxyAuthenticationCredential :: Prelude.Maybe XksProxyAuthenticationCredentialType,
    -- | Indicates how KMS communicates with the external key store proxy. This
    -- parameter is required for custom key stores with a @CustomKeyStoreType@
    -- of @EXTERNAL_KEY_STORE@.
    --
    -- If the external key store proxy uses a public endpoint, specify
    -- @PUBLIC_ENDPOINT@. If the external key store proxy uses a Amazon VPC
    -- endpoint service for communication with KMS, specify
    -- @VPC_ENDPOINT_SERVICE@. For help making this choice, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/plan-xks-keystore.html#choose-xks-connectivity Choosing a connectivity option>
    -- in the /Key Management Service Developer Guide/.
    --
    -- An Amazon VPC endpoint service keeps your communication with KMS in a
    -- private address space entirely within Amazon Web Services, but it
    -- requires more configuration, including establishing a Amazon VPC with
    -- multiple subnets, a VPC endpoint service, a network load balancer, and a
    -- verified private DNS name. A public endpoint is simpler to set up, but
    -- it might be slower and might not fulfill your security requirements. You
    -- might consider testing with a public endpoint, and then establishing a
    -- VPC endpoint service for production tasks. Note that this choice does
    -- not determine the location of the external key store proxy. Even if you
    -- choose a VPC endpoint service, the proxy can be hosted within the VPC or
    -- outside of Amazon Web Services such as in your corporate data center.
    xksProxyConnectivity :: Prelude.Maybe XksProxyConnectivityType,
    -- | Specifies the endpoint that KMS uses to send requests to the external
    -- key store proxy (XKS proxy). This parameter is required for custom key
    -- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- The protocol must be HTTPS. KMS communicates on port 443. Do not specify
    -- the port in the @XksProxyUriEndpoint@ value.
    --
    -- For external key stores with @XksProxyConnectivity@ value of
    -- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
    -- name of the VPC endpoint service.
    --
    -- For external key stores with @PUBLIC_ENDPOINT@ connectivity, this
    -- endpoint must be reachable before you create the custom key store. KMS
    -- connects to the external key store proxy while creating the custom key
    -- store. For external key stores with @VPC_ENDPOINT_SERVICE@ connectivity,
    -- KMS connects when you call the ConnectCustomKeyStore operation.
    --
    -- The value of this parameter must begin with @https:\/\/@. The remainder
    -- can contain upper and lower case letters (A-Z and a-z), numbers (0-9),
    -- dots (@.@), and hyphens (@-@). Additional slashes (@\/@ and @\\@) are
    -- not permitted.
    --
    -- __Uniqueness requirements:__
    --
    -- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
    --     be unique in the Amazon Web Services account and Region.
    --
    -- -   An external key store with @PUBLIC_ENDPOINT@ connectivity cannot use
    --     the same @XksProxyUriEndpoint@ value as an external key store with
    --     @VPC_ENDPOINT_SERVICE@ connectivity in the same Amazon Web Services
    --     Region.
    --
    -- -   Each external key store with @VPC_ENDPOINT_SERVICE@ connectivity
    --     must have its own private DNS name. The @XksProxyUriEndpoint@ value
    --     for external key stores with @VPC_ENDPOINT_SERVICE@ connectivity
    --     (private DNS name) must be unique in the Amazon Web Services account
    --     and Region.
    xksProxyUriEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Specifies the base path to the proxy APIs for this external key store.
    -- To find this value, see the documentation for your external key store
    -- proxy. This parameter is required for all custom key stores with a
    -- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
    --
    -- The value must start with @\/@ and must end with @\/kms\/xks\/v1@ where
    -- @v1@ represents the version of the KMS external key store proxy API.
    -- This path can include an optional prefix between the required elements
    -- such as @\/@/@prefix@/@\/kms\/xks\/v1@.
    --
    -- __Uniqueness requirements:__
    --
    -- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
    --     be unique in the Amazon Web Services account and Region.
    xksProxyUriPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Amazon VPC endpoint service for interface
    -- endpoints that is used to communicate with your external key store proxy
    -- (XKS proxy). This parameter is required when the value of
    -- @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the value of
    -- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
    --
    -- The Amazon VPC endpoint service must
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-xks-keystore.html#xks-requirements fulfill all requirements>
    -- for use with an external key store.
    --
    -- __Uniqueness requirements:__
    --
    -- -   External key stores with @VPC_ENDPOINT_SERVICE@ connectivity can
    --     share an Amazon VPC, but each external key store must have its own
    --     VPC endpoint service and private DNS name.
    xksProxyVpcEndpointServiceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies a friendly name for the custom key store. The name must be
    -- unique in your Amazon Web Services account and Region. This parameter is
    -- required for all custom key stores.
    customKeyStoreName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomKeyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudHsmClusterId', 'createCustomKeyStore_cloudHsmClusterId' - Identifies the CloudHSM cluster for an CloudHSM key store. This
-- parameter is required for custom key stores with @CustomKeyStoreType@ of
-- @AWS_CLOUDHSM@.
--
-- Enter the cluster ID of any active CloudHSM cluster that is not already
-- associated with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
--
-- 'customKeyStoreType', 'createCustomKeyStore_customKeyStoreType' - Specifies the type of custom key store. The default value is
-- @AWS_CLOUDHSM@.
--
-- For a custom key store backed by an CloudHSM cluster, omit the parameter
-- or enter @AWS_CLOUDHSM@. For a custom key store backed by an external
-- key manager outside of Amazon Web Services, enter @EXTERNAL_KEY_STORE@.
-- You cannot change this property after the key store is created.
--
-- 'keyStorePassword', 'createCustomKeyStore_keyStorePassword' - Specifies the @kmsuser@ password for an CloudHSM key store. This
-- parameter is required for custom key stores with a @CustomKeyStoreType@
-- of @AWS_CLOUDHSM@.
--
-- Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified CloudHSM cluster. KMS logs into the cluster as this
-- user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells KMS the @kmsuser@ account password; it does not
-- change the password in the CloudHSM cluster.
--
-- 'trustAnchorCertificate', 'createCustomKeyStore_trustAnchorCertificate' - Specifies the certificate for an CloudHSM key store. This parameter is
-- required for custom key stores with a @CustomKeyStoreType@ of
-- @AWS_CLOUDHSM@.
--
-- Enter the content of the trust anchor certificate for the CloudHSM
-- cluster. This is the content of the @customerCA.crt@ file that you
-- created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
--
-- 'xksProxyAuthenticationCredential', 'createCustomKeyStore_xksProxyAuthenticationCredential' - Specifies an authentication credential for the external key store proxy
-- (XKS proxy). This parameter is required for all custom key stores with a
-- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The @XksProxyAuthenticationCredential@ has two required elements:
-- @RawSecretAccessKey@, a secret key, and @AccessKeyId@, a unique
-- identifier for the @RawSecretAccessKey@. For character requirements, see
-- <kms/latest/APIReference/API_XksProxyAuthenticationCredentialType.html XksProxyAuthenticationCredentialType>.
--
-- KMS uses this authentication credential to sign requests to the external
-- key store proxy on your behalf. This credential is unrelated to Identity
-- and Access Management (IAM) and Amazon Web Services credentials.
--
-- This parameter doesn\'t set or change the authentication credentials on
-- the XKS proxy. It just tells KMS the credential that you established on
-- your external key store proxy. If you rotate your proxy authentication
-- credential, use the UpdateCustomKeyStore operation to provide the new
-- credential to KMS.
--
-- 'xksProxyConnectivity', 'createCustomKeyStore_xksProxyConnectivity' - Indicates how KMS communicates with the external key store proxy. This
-- parameter is required for custom key stores with a @CustomKeyStoreType@
-- of @EXTERNAL_KEY_STORE@.
--
-- If the external key store proxy uses a public endpoint, specify
-- @PUBLIC_ENDPOINT@. If the external key store proxy uses a Amazon VPC
-- endpoint service for communication with KMS, specify
-- @VPC_ENDPOINT_SERVICE@. For help making this choice, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/plan-xks-keystore.html#choose-xks-connectivity Choosing a connectivity option>
-- in the /Key Management Service Developer Guide/.
--
-- An Amazon VPC endpoint service keeps your communication with KMS in a
-- private address space entirely within Amazon Web Services, but it
-- requires more configuration, including establishing a Amazon VPC with
-- multiple subnets, a VPC endpoint service, a network load balancer, and a
-- verified private DNS name. A public endpoint is simpler to set up, but
-- it might be slower and might not fulfill your security requirements. You
-- might consider testing with a public endpoint, and then establishing a
-- VPC endpoint service for production tasks. Note that this choice does
-- not determine the location of the external key store proxy. Even if you
-- choose a VPC endpoint service, the proxy can be hosted within the VPC or
-- outside of Amazon Web Services such as in your corporate data center.
--
-- 'xksProxyUriEndpoint', 'createCustomKeyStore_xksProxyUriEndpoint' - Specifies the endpoint that KMS uses to send requests to the external
-- key store proxy (XKS proxy). This parameter is required for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The protocol must be HTTPS. KMS communicates on port 443. Do not specify
-- the port in the @XksProxyUriEndpoint@ value.
--
-- For external key stores with @XksProxyConnectivity@ value of
-- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
-- name of the VPC endpoint service.
--
-- For external key stores with @PUBLIC_ENDPOINT@ connectivity, this
-- endpoint must be reachable before you create the custom key store. KMS
-- connects to the external key store proxy while creating the custom key
-- store. For external key stores with @VPC_ENDPOINT_SERVICE@ connectivity,
-- KMS connects when you call the ConnectCustomKeyStore operation.
--
-- The value of this parameter must begin with @https:\/\/@. The remainder
-- can contain upper and lower case letters (A-Z and a-z), numbers (0-9),
-- dots (@.@), and hyphens (@-@). Additional slashes (@\/@ and @\\@) are
-- not permitted.
--
-- __Uniqueness requirements:__
--
-- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
--     be unique in the Amazon Web Services account and Region.
--
-- -   An external key store with @PUBLIC_ENDPOINT@ connectivity cannot use
--     the same @XksProxyUriEndpoint@ value as an external key store with
--     @VPC_ENDPOINT_SERVICE@ connectivity in the same Amazon Web Services
--     Region.
--
-- -   Each external key store with @VPC_ENDPOINT_SERVICE@ connectivity
--     must have its own private DNS name. The @XksProxyUriEndpoint@ value
--     for external key stores with @VPC_ENDPOINT_SERVICE@ connectivity
--     (private DNS name) must be unique in the Amazon Web Services account
--     and Region.
--
-- 'xksProxyUriPath', 'createCustomKeyStore_xksProxyUriPath' - Specifies the base path to the proxy APIs for this external key store.
-- To find this value, see the documentation for your external key store
-- proxy. This parameter is required for all custom key stores with a
-- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The value must start with @\/@ and must end with @\/kms\/xks\/v1@ where
-- @v1@ represents the version of the KMS external key store proxy API.
-- This path can include an optional prefix between the required elements
-- such as @\/@/@prefix@/@\/kms\/xks\/v1@.
--
-- __Uniqueness requirements:__
--
-- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
--     be unique in the Amazon Web Services account and Region.
--
-- 'xksProxyVpcEndpointServiceName', 'createCustomKeyStore_xksProxyVpcEndpointServiceName' - Specifies the name of the Amazon VPC endpoint service for interface
-- endpoints that is used to communicate with your external key store proxy
-- (XKS proxy). This parameter is required when the value of
-- @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the value of
-- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
--
-- The Amazon VPC endpoint service must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-xks-keystore.html#xks-requirements fulfill all requirements>
-- for use with an external key store.
--
-- __Uniqueness requirements:__
--
-- -   External key stores with @VPC_ENDPOINT_SERVICE@ connectivity can
--     share an Amazon VPC, but each external key store must have its own
--     VPC endpoint service and private DNS name.
--
-- 'customKeyStoreName', 'createCustomKeyStore_customKeyStoreName' - Specifies a friendly name for the custom key store. The name must be
-- unique in your Amazon Web Services account and Region. This parameter is
-- required for all custom key stores.
newCreateCustomKeyStore ::
  -- | 'customKeyStoreName'
  Prelude.Text ->
  CreateCustomKeyStore
newCreateCustomKeyStore pCustomKeyStoreName_ =
  CreateCustomKeyStore'
    { cloudHsmClusterId =
        Prelude.Nothing,
      customKeyStoreType = Prelude.Nothing,
      keyStorePassword = Prelude.Nothing,
      trustAnchorCertificate = Prelude.Nothing,
      xksProxyAuthenticationCredential = Prelude.Nothing,
      xksProxyConnectivity = Prelude.Nothing,
      xksProxyUriEndpoint = Prelude.Nothing,
      xksProxyUriPath = Prelude.Nothing,
      xksProxyVpcEndpointServiceName = Prelude.Nothing,
      customKeyStoreName = pCustomKeyStoreName_
    }

-- | Identifies the CloudHSM cluster for an CloudHSM key store. This
-- parameter is required for custom key stores with @CustomKeyStoreType@ of
-- @AWS_CLOUDHSM@.
--
-- Enter the cluster ID of any active CloudHSM cluster that is not already
-- associated with a custom key store. To find the cluster ID, use the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/API_DescribeClusters.html DescribeClusters>
-- operation.
createCustomKeyStore_cloudHsmClusterId :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_cloudHsmClusterId = Lens.lens (\CreateCustomKeyStore' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@CreateCustomKeyStore' {} a -> s {cloudHsmClusterId = a} :: CreateCustomKeyStore)

-- | Specifies the type of custom key store. The default value is
-- @AWS_CLOUDHSM@.
--
-- For a custom key store backed by an CloudHSM cluster, omit the parameter
-- or enter @AWS_CLOUDHSM@. For a custom key store backed by an external
-- key manager outside of Amazon Web Services, enter @EXTERNAL_KEY_STORE@.
-- You cannot change this property after the key store is created.
createCustomKeyStore_customKeyStoreType :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe CustomKeyStoreType)
createCustomKeyStore_customKeyStoreType = Lens.lens (\CreateCustomKeyStore' {customKeyStoreType} -> customKeyStoreType) (\s@CreateCustomKeyStore' {} a -> s {customKeyStoreType = a} :: CreateCustomKeyStore)

-- | Specifies the @kmsuser@ password for an CloudHSM key store. This
-- parameter is required for custom key stores with a @CustomKeyStoreType@
-- of @AWS_CLOUDHSM@.
--
-- Enter the password of the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-store-concepts.html#concept-kmsuser kmsuser crypto user (CU) account>
-- in the specified CloudHSM cluster. KMS logs into the cluster as this
-- user to manage key material on your behalf.
--
-- The password must be a string of 7 to 32 characters. Its value is case
-- sensitive.
--
-- This parameter tells KMS the @kmsuser@ account password; it does not
-- change the password in the CloudHSM cluster.
createCustomKeyStore_keyStorePassword :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_keyStorePassword = Lens.lens (\CreateCustomKeyStore' {keyStorePassword} -> keyStorePassword) (\s@CreateCustomKeyStore' {} a -> s {keyStorePassword = a} :: CreateCustomKeyStore) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the certificate for an CloudHSM key store. This parameter is
-- required for custom key stores with a @CustomKeyStoreType@ of
-- @AWS_CLOUDHSM@.
--
-- Enter the content of the trust anchor certificate for the CloudHSM
-- cluster. This is the content of the @customerCA.crt@ file that you
-- created when you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html initialized the cluster>.
createCustomKeyStore_trustAnchorCertificate :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_trustAnchorCertificate = Lens.lens (\CreateCustomKeyStore' {trustAnchorCertificate} -> trustAnchorCertificate) (\s@CreateCustomKeyStore' {} a -> s {trustAnchorCertificate = a} :: CreateCustomKeyStore)

-- | Specifies an authentication credential for the external key store proxy
-- (XKS proxy). This parameter is required for all custom key stores with a
-- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The @XksProxyAuthenticationCredential@ has two required elements:
-- @RawSecretAccessKey@, a secret key, and @AccessKeyId@, a unique
-- identifier for the @RawSecretAccessKey@. For character requirements, see
-- <kms/latest/APIReference/API_XksProxyAuthenticationCredentialType.html XksProxyAuthenticationCredentialType>.
--
-- KMS uses this authentication credential to sign requests to the external
-- key store proxy on your behalf. This credential is unrelated to Identity
-- and Access Management (IAM) and Amazon Web Services credentials.
--
-- This parameter doesn\'t set or change the authentication credentials on
-- the XKS proxy. It just tells KMS the credential that you established on
-- your external key store proxy. If you rotate your proxy authentication
-- credential, use the UpdateCustomKeyStore operation to provide the new
-- credential to KMS.
createCustomKeyStore_xksProxyAuthenticationCredential :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe XksProxyAuthenticationCredentialType)
createCustomKeyStore_xksProxyAuthenticationCredential = Lens.lens (\CreateCustomKeyStore' {xksProxyAuthenticationCredential} -> xksProxyAuthenticationCredential) (\s@CreateCustomKeyStore' {} a -> s {xksProxyAuthenticationCredential = a} :: CreateCustomKeyStore)

-- | Indicates how KMS communicates with the external key store proxy. This
-- parameter is required for custom key stores with a @CustomKeyStoreType@
-- of @EXTERNAL_KEY_STORE@.
--
-- If the external key store proxy uses a public endpoint, specify
-- @PUBLIC_ENDPOINT@. If the external key store proxy uses a Amazon VPC
-- endpoint service for communication with KMS, specify
-- @VPC_ENDPOINT_SERVICE@. For help making this choice, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/plan-xks-keystore.html#choose-xks-connectivity Choosing a connectivity option>
-- in the /Key Management Service Developer Guide/.
--
-- An Amazon VPC endpoint service keeps your communication with KMS in a
-- private address space entirely within Amazon Web Services, but it
-- requires more configuration, including establishing a Amazon VPC with
-- multiple subnets, a VPC endpoint service, a network load balancer, and a
-- verified private DNS name. A public endpoint is simpler to set up, but
-- it might be slower and might not fulfill your security requirements. You
-- might consider testing with a public endpoint, and then establishing a
-- VPC endpoint service for production tasks. Note that this choice does
-- not determine the location of the external key store proxy. Even if you
-- choose a VPC endpoint service, the proxy can be hosted within the VPC or
-- outside of Amazon Web Services such as in your corporate data center.
createCustomKeyStore_xksProxyConnectivity :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe XksProxyConnectivityType)
createCustomKeyStore_xksProxyConnectivity = Lens.lens (\CreateCustomKeyStore' {xksProxyConnectivity} -> xksProxyConnectivity) (\s@CreateCustomKeyStore' {} a -> s {xksProxyConnectivity = a} :: CreateCustomKeyStore)

-- | Specifies the endpoint that KMS uses to send requests to the external
-- key store proxy (XKS proxy). This parameter is required for custom key
-- stores with a @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The protocol must be HTTPS. KMS communicates on port 443. Do not specify
-- the port in the @XksProxyUriEndpoint@ value.
--
-- For external key stores with @XksProxyConnectivity@ value of
-- @VPC_ENDPOINT_SERVICE@, specify @https:\/\/@ followed by the private DNS
-- name of the VPC endpoint service.
--
-- For external key stores with @PUBLIC_ENDPOINT@ connectivity, this
-- endpoint must be reachable before you create the custom key store. KMS
-- connects to the external key store proxy while creating the custom key
-- store. For external key stores with @VPC_ENDPOINT_SERVICE@ connectivity,
-- KMS connects when you call the ConnectCustomKeyStore operation.
--
-- The value of this parameter must begin with @https:\/\/@. The remainder
-- can contain upper and lower case letters (A-Z and a-z), numbers (0-9),
-- dots (@.@), and hyphens (@-@). Additional slashes (@\/@ and @\\@) are
-- not permitted.
--
-- __Uniqueness requirements:__
--
-- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
--     be unique in the Amazon Web Services account and Region.
--
-- -   An external key store with @PUBLIC_ENDPOINT@ connectivity cannot use
--     the same @XksProxyUriEndpoint@ value as an external key store with
--     @VPC_ENDPOINT_SERVICE@ connectivity in the same Amazon Web Services
--     Region.
--
-- -   Each external key store with @VPC_ENDPOINT_SERVICE@ connectivity
--     must have its own private DNS name. The @XksProxyUriEndpoint@ value
--     for external key stores with @VPC_ENDPOINT_SERVICE@ connectivity
--     (private DNS name) must be unique in the Amazon Web Services account
--     and Region.
createCustomKeyStore_xksProxyUriEndpoint :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_xksProxyUriEndpoint = Lens.lens (\CreateCustomKeyStore' {xksProxyUriEndpoint} -> xksProxyUriEndpoint) (\s@CreateCustomKeyStore' {} a -> s {xksProxyUriEndpoint = a} :: CreateCustomKeyStore)

-- | Specifies the base path to the proxy APIs for this external key store.
-- To find this value, see the documentation for your external key store
-- proxy. This parameter is required for all custom key stores with a
-- @CustomKeyStoreType@ of @EXTERNAL_KEY_STORE@.
--
-- The value must start with @\/@ and must end with @\/kms\/xks\/v1@ where
-- @v1@ represents the version of the KMS external key store proxy API.
-- This path can include an optional prefix between the required elements
-- such as @\/@/@prefix@/@\/kms\/xks\/v1@.
--
-- __Uniqueness requirements:__
--
-- -   The combined @XksProxyUriEndpoint@ and @XksProxyUriPath@ values must
--     be unique in the Amazon Web Services account and Region.
createCustomKeyStore_xksProxyUriPath :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_xksProxyUriPath = Lens.lens (\CreateCustomKeyStore' {xksProxyUriPath} -> xksProxyUriPath) (\s@CreateCustomKeyStore' {} a -> s {xksProxyUriPath = a} :: CreateCustomKeyStore)

-- | Specifies the name of the Amazon VPC endpoint service for interface
-- endpoints that is used to communicate with your external key store proxy
-- (XKS proxy). This parameter is required when the value of
-- @CustomKeyStoreType@ is @EXTERNAL_KEY_STORE@ and the value of
-- @XksProxyConnectivity@ is @VPC_ENDPOINT_SERVICE@.
--
-- The Amazon VPC endpoint service must
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-xks-keystore.html#xks-requirements fulfill all requirements>
-- for use with an external key store.
--
-- __Uniqueness requirements:__
--
-- -   External key stores with @VPC_ENDPOINT_SERVICE@ connectivity can
--     share an Amazon VPC, but each external key store must have its own
--     VPC endpoint service and private DNS name.
createCustomKeyStore_xksProxyVpcEndpointServiceName :: Lens.Lens' CreateCustomKeyStore (Prelude.Maybe Prelude.Text)
createCustomKeyStore_xksProxyVpcEndpointServiceName = Lens.lens (\CreateCustomKeyStore' {xksProxyVpcEndpointServiceName} -> xksProxyVpcEndpointServiceName) (\s@CreateCustomKeyStore' {} a -> s {xksProxyVpcEndpointServiceName = a} :: CreateCustomKeyStore)

-- | Specifies a friendly name for the custom key store. The name must be
-- unique in your Amazon Web Services account and Region. This parameter is
-- required for all custom key stores.
createCustomKeyStore_customKeyStoreName :: Lens.Lens' CreateCustomKeyStore Prelude.Text
createCustomKeyStore_customKeyStoreName = Lens.lens (\CreateCustomKeyStore' {customKeyStoreName} -> customKeyStoreName) (\s@CreateCustomKeyStore' {} a -> s {customKeyStoreName = a} :: CreateCustomKeyStore)

instance Core.AWSRequest CreateCustomKeyStore where
  type
    AWSResponse CreateCustomKeyStore =
      CreateCustomKeyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomKeyStoreResponse'
            Prelude.<$> (x Data..?> "CustomKeyStoreId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomKeyStore where
  hashWithSalt _salt CreateCustomKeyStore' {..} =
    _salt
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` customKeyStoreType
      `Prelude.hashWithSalt` keyStorePassword
      `Prelude.hashWithSalt` trustAnchorCertificate
      `Prelude.hashWithSalt` xksProxyAuthenticationCredential
      `Prelude.hashWithSalt` xksProxyConnectivity
      `Prelude.hashWithSalt` xksProxyUriEndpoint
      `Prelude.hashWithSalt` xksProxyUriPath
      `Prelude.hashWithSalt` xksProxyVpcEndpointServiceName
      `Prelude.hashWithSalt` customKeyStoreName

instance Prelude.NFData CreateCustomKeyStore where
  rnf CreateCustomKeyStore' {..} =
    Prelude.rnf cloudHsmClusterId `Prelude.seq`
      Prelude.rnf customKeyStoreType `Prelude.seq`
        Prelude.rnf keyStorePassword `Prelude.seq`
          Prelude.rnf trustAnchorCertificate `Prelude.seq`
            Prelude.rnf xksProxyAuthenticationCredential `Prelude.seq`
              Prelude.rnf xksProxyConnectivity `Prelude.seq`
                Prelude.rnf xksProxyUriEndpoint `Prelude.seq`
                  Prelude.rnf xksProxyUriPath `Prelude.seq`
                    Prelude.rnf xksProxyVpcEndpointServiceName `Prelude.seq`
                      Prelude.rnf customKeyStoreName

instance Data.ToHeaders CreateCustomKeyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.CreateCustomKeyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomKeyStore where
  toJSON CreateCustomKeyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudHsmClusterId" Data..=)
              Prelude.<$> cloudHsmClusterId,
            ("CustomKeyStoreType" Data..=)
              Prelude.<$> customKeyStoreType,
            ("KeyStorePassword" Data..=)
              Prelude.<$> keyStorePassword,
            ("TrustAnchorCertificate" Data..=)
              Prelude.<$> trustAnchorCertificate,
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
              ("CustomKeyStoreName" Data..= customKeyStoreName)
          ]
      )

instance Data.ToPath CreateCustomKeyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomKeyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomKeyStoreResponse' smart constructor.
data CreateCustomKeyStoreResponse = CreateCustomKeyStoreResponse'
  { -- | A unique identifier for the new custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomKeyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKeyStoreId', 'createCustomKeyStoreResponse_customKeyStoreId' - A unique identifier for the new custom key store.
--
-- 'httpStatus', 'createCustomKeyStoreResponse_httpStatus' - The response's http status code.
newCreateCustomKeyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomKeyStoreResponse
newCreateCustomKeyStoreResponse pHttpStatus_ =
  CreateCustomKeyStoreResponse'
    { customKeyStoreId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the new custom key store.
createCustomKeyStoreResponse_customKeyStoreId :: Lens.Lens' CreateCustomKeyStoreResponse (Prelude.Maybe Prelude.Text)
createCustomKeyStoreResponse_customKeyStoreId = Lens.lens (\CreateCustomKeyStoreResponse' {customKeyStoreId} -> customKeyStoreId) (\s@CreateCustomKeyStoreResponse' {} a -> s {customKeyStoreId = a} :: CreateCustomKeyStoreResponse)

-- | The response's http status code.
createCustomKeyStoreResponse_httpStatus :: Lens.Lens' CreateCustomKeyStoreResponse Prelude.Int
createCustomKeyStoreResponse_httpStatus = Lens.lens (\CreateCustomKeyStoreResponse' {httpStatus} -> httpStatus) (\s@CreateCustomKeyStoreResponse' {} a -> s {httpStatus = a} :: CreateCustomKeyStoreResponse)

instance Prelude.NFData CreateCustomKeyStoreResponse where
  rnf CreateCustomKeyStoreResponse' {..} =
    Prelude.rnf customKeyStoreId `Prelude.seq`
      Prelude.rnf httpStatus
