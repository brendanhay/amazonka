{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Types.CustomKeyStoresListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.CustomKeyStoresListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.ConnectionErrorCodeType
import Amazonka.KMS.Types.ConnectionStateType
import Amazonka.KMS.Types.CustomKeyStoreType
import Amazonka.KMS.Types.XksProxyConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about each custom key store in the custom key store
-- list.
--
-- /See:/ 'newCustomKeyStoresListEntry' smart constructor.
data CustomKeyStoresListEntry = CustomKeyStoresListEntry'
  { -- | A unique identifier for the CloudHSM cluster that is associated with an
    -- CloudHSM key store. This field appears only when the
    -- @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
    cloudHsmClusterId :: Prelude.Maybe Prelude.Text,
    -- | Describes the connection error. This field appears in the response only
    -- when the @ConnectionState@ is @FAILED@.
    --
    -- Many failures can be resolved by updating the properties of the custom
    -- key store. To update a custom key store, disconnect it
    -- (DisconnectCustomKeyStore), correct the errors (UpdateCustomKeyStore),
    -- and try to connect again (ConnectCustomKeyStore). For additional help
    -- resolving these errors, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
    -- in /Key Management Service Developer Guide/.
    --
    -- __All custom key stores:__
    --
    -- -   @INTERNAL_ERROR@ — KMS could not complete the request due to an
    --     internal error. Retry the request. For @ConnectCustomKeyStore@
    --     requests, disconnect the custom key store before trying to connect
    --     again.
    --
    -- -   @NETWORK_ERRORS@ — Network errors are preventing KMS from connecting
    --     the custom key store to its backing key store.
    --
    -- __CloudHSM key stores:__
    --
    -- -   @CLUSTER_NOT_FOUND@ — KMS cannot find the CloudHSM cluster with the
    --     specified cluster ID.
    --
    -- -   @INSUFFICIENT_CLOUDHSM_HSMS@ — The associated CloudHSM cluster does
    --     not contain any active HSMs. To connect a custom key store to its
    --     CloudHSM cluster, the cluster must contain at least one active HSM.
    --
    -- -   @INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET@ — At least one private
    --     subnet associated with the CloudHSM cluster doesn\'t have any
    --     available IP addresses. A CloudHSM key store connection requires one
    --     free IP address in each of the associated private subnets, although
    --     two are preferable. For details, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
    --     in the /Key Management Service Developer Guide/.
    --
    -- -   @INVALID_CREDENTIALS@ — The @KeyStorePassword@ for the custom key
    --     store doesn\'t match the current password of the @kmsuser@ crypto
    --     user in the CloudHSM cluster. Before you can connect your custom key
    --     store to its CloudHSM cluster, you must change the @kmsuser@ account
    --     password and update the @KeyStorePassword@ value for the custom key
    --     store.
    --
    -- -   @SUBNET_NOT_FOUND@ — A subnet in the CloudHSM cluster configuration
    --     was deleted. If KMS cannot find all of the subnets in the cluster
    --     configuration, attempts to connect the custom key store to the
    --     CloudHSM cluster fail. To fix this error, create a cluster from a
    --     recent backup and associate it with your custom key store. (This
    --     process creates a new cluster configuration with a VPC and private
    --     subnets.) For details, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
    --     in the /Key Management Service Developer Guide/.
    --
    -- -   @USER_LOCKED_OUT@ — The @kmsuser@ CU account is locked out of the
    --     associated CloudHSM cluster due to too many failed password
    --     attempts. Before you can connect your custom key store to its
    --     CloudHSM cluster, you must change the @kmsuser@ account password and
    --     update the key store password value for the custom key store.
    --
    -- -   @USER_LOGGED_IN@ — The @kmsuser@ CU account is logged into the
    --     associated CloudHSM cluster. This prevents KMS from rotating the
    --     @kmsuser@ account password and logging into the cluster. Before you
    --     can connect your custom key store to its CloudHSM cluster, you must
    --     log the @kmsuser@ CU out of the cluster. If you changed the
    --     @kmsuser@ password to log into the cluster, you must also and update
    --     the key store password value for the custom key store. For help, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
    --     in the /Key Management Service Developer Guide/.
    --
    -- -   @USER_NOT_FOUND@ — KMS cannot find a @kmsuser@ CU account in the
    --     associated CloudHSM cluster. Before you can connect your custom key
    --     store to its CloudHSM cluster, you must create a @kmsuser@ CU
    --     account in the cluster, and then update the key store password value
    --     for the custom key store.
    --
    -- __External key stores:__
    --
    -- -   @INVALID_CREDENTIALS@ — One or both of the
    --     @XksProxyAuthenticationCredential@ values is not valid on the
    --     specified external key store proxy.
    --
    -- -   @XKS_PROXY_ACCESS_DENIED@ — KMS requests are denied access to the
    --     external key store proxy. If the external key store proxy has
    --     authorization rules, verify that they permit KMS to communicate with
    --     the proxy on your behalf.
    --
    -- -   @XKS_PROXY_INVALID_CONFIGURATION@ — A configuration error is
    --     preventing the external key store from connecting to its proxy.
    --     Verify the value of the @XksProxyUriPath@.
    --
    -- -   @XKS_PROXY_INVALID_RESPONSE@ — KMS cannot interpret the response
    --     from the external key store proxy. If you see this connection error
    --     code repeatedly, notify your external key store proxy vendor.
    --
    -- -   @XKS_PROXY_INVALID_TLS_CONFIGURATION@ — KMS cannot connect to the
    --     external key store proxy because the TLS configuration is invalid.
    --     Verify that the XKS proxy supports TLS 1.2 or 1.3. Also, verify that
    --     the TLS certificate is not expired, and that it matches the hostname
    --     in the @XksProxyUriEndpoint@ value, and that it is signed by a
    --     certificate authority included in the
    --     <https://github.com/aws/aws-kms-xksproxy-api-spec/blob/main/TrustedCertificateAuthorities Trusted Certificate Authorities>
    --     list.
    --
    -- -   @XKS_PROXY_NOT_REACHABLE@ — KMS can\'t communicate with your
    --     external key store proxy. Verify that the @XksProxyUriEndpoint@ and
    --     @XksProxyUriPath@ are correct. Use the tools for your external key
    --     store proxy to verify that the proxy is active and available on its
    --     network. Also, verify that your external key manager instances are
    --     operating properly. Connection attempts fail with this connection
    --     error code if the proxy reports that all external key manager
    --     instances are unavailable.
    --
    -- -   @XKS_PROXY_TIMED_OUT@ — KMS can connect to the external key store
    --     proxy, but the proxy does not respond to KMS in the time allotted.
    --     If you see this connection error code repeatedly, notify your
    --     external key store proxy vendor.
    --
    -- -   @XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION@ — The Amazon VPC
    --     endpoint service configuration doesn\'t conform to the requirements
    --     for an KMS external key store.
    --
    --     -   The VPC endpoint service must be an endpoint service for
    --         interface endpoints in the caller\'s Amazon Web Services
    --         account.
    --
    --     -   It must have a network load balancer (NLB) connected to at least
    --         two subnets, each in a different Availability Zone.
    --
    --     -   The @Allow principals@ list must include the KMS service
    --         principal for the Region, @cks.kms.\<region>.amazonaws.com@,
    --         such as @cks.kms.us-east-1.amazonaws.com@.
    --
    --     -   It must /not/ require
    --         <https://docs.aws.amazon.com/vpc/latest/privatelink/create-endpoint-service.html acceptance>
    --         of connection requests.
    --
    --     -   It must have a private DNS name. The private DNS name for an
    --         external key store with @VPC_ENDPOINT_SERVICE@ connectivity must
    --         be unique in its Amazon Web Services Region.
    --
    --     -   The domain of the private DNS name must have a
    --         <https://docs.aws.amazon.com/vpc/latest/privatelink/verify-domains.html verification status>
    --         of @verified@.
    --
    --     -   The
    --         <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS certificate>
    --         specifies the private DNS hostname at which the endpoint is
    --         reachable.
    --
    -- -   @XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND@ — KMS can\'t find the VPC
    --     endpoint service that it uses to communicate with the external key
    --     store proxy. Verify that the @XksProxyVpcEndpointServiceName@ is
    --     correct and the KMS service principal has service consumer
    --     permissions on the Amazon VPC endpoint service.
    connectionErrorCode :: Prelude.Maybe ConnectionErrorCodeType,
    -- | Indicates whether the custom key store is connected to its backing key
    -- store. For an CloudHSM key store, the @ConnectionState@ indicates
    -- whether it is connected to its CloudHSM cluster. For an external key
    -- store, the @ConnectionState@ indicates whether it is connected to the
    -- external key store proxy that communicates with your external key
    -- manager.
    --
    -- You can create and use KMS keys in your custom key stores only when its
    -- @ConnectionState@ is @CONNECTED@.
    --
    -- The @ConnectionState@ value is @DISCONNECTED@ only if the key store has
    -- never been connected or you use the DisconnectCustomKeyStore operation
    -- to disconnect it. If the value is @CONNECTED@ but you are having trouble
    -- using the custom key store, make sure that the backing key store is
    -- reachable and active. For an CloudHSM key store, verify that its
    -- associated CloudHSM cluster is active and contains at least one active
    -- HSM. For an external key store, verify that the external key store proxy
    -- and external key manager are connected and enabled.
    --
    -- A value of @FAILED@ indicates that an attempt to connect was
    -- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
    -- the cause of the failure. For help resolving a connection failure, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a custom key store>
    -- in the /Key Management Service Developer Guide/.
    connectionState :: Prelude.Maybe ConnectionStateType,
    -- | The date and time when the custom key store was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the custom key store.
    customKeyStoreId :: Prelude.Maybe Prelude.Text,
    -- | The user-specified friendly name for the custom key store.
    customKeyStoreName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of the custom key store. @AWS_CLOUDHSM@ indicates a
    -- custom key store backed by an CloudHSM cluster. @EXTERNAL_KEY_STORE@
    -- indicates a custom key store backed by an external key store proxy and
    -- external key manager outside of Amazon Web Services.
    customKeyStoreType :: Prelude.Maybe CustomKeyStoreType,
    -- | The trust anchor certificate of the CloudHSM cluster associated with an
    -- CloudHSM key store. When you
    -- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
    -- you create this certificate and save it in the @customerCA.crt@ file.
    --
    -- This field appears only when the @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
    trustAnchorCertificate :: Prelude.Maybe Prelude.Text,
    -- | Configuration settings for the external key store proxy (XKS proxy). The
    -- external key store proxy translates KMS requests into a format that your
    -- external key manager can understand. The proxy configuration includes
    -- connection information that KMS requires.
    --
    -- This field appears only when the @CustomKeyStoreType@ is
    -- @EXTERNAL_KEY_STORE@.
    xksProxyConfiguration :: Prelude.Maybe XksProxyConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomKeyStoresListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudHsmClusterId', 'customKeyStoresListEntry_cloudHsmClusterId' - A unique identifier for the CloudHSM cluster that is associated with an
-- CloudHSM key store. This field appears only when the
-- @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
--
-- 'connectionErrorCode', 'customKeyStoresListEntry_connectionErrorCode' - Describes the connection error. This field appears in the response only
-- when the @ConnectionState@ is @FAILED@.
--
-- Many failures can be resolved by updating the properties of the custom
-- key store. To update a custom key store, disconnect it
-- (DisconnectCustomKeyStore), correct the errors (UpdateCustomKeyStore),
-- and try to connect again (ConnectCustomKeyStore). For additional help
-- resolving these errors, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
-- in /Key Management Service Developer Guide/.
--
-- __All custom key stores:__
--
-- -   @INTERNAL_ERROR@ — KMS could not complete the request due to an
--     internal error. Retry the request. For @ConnectCustomKeyStore@
--     requests, disconnect the custom key store before trying to connect
--     again.
--
-- -   @NETWORK_ERRORS@ — Network errors are preventing KMS from connecting
--     the custom key store to its backing key store.
--
-- __CloudHSM key stores:__
--
-- -   @CLUSTER_NOT_FOUND@ — KMS cannot find the CloudHSM cluster with the
--     specified cluster ID.
--
-- -   @INSUFFICIENT_CLOUDHSM_HSMS@ — The associated CloudHSM cluster does
--     not contain any active HSMs. To connect a custom key store to its
--     CloudHSM cluster, the cluster must contain at least one active HSM.
--
-- -   @INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET@ — At least one private
--     subnet associated with the CloudHSM cluster doesn\'t have any
--     available IP addresses. A CloudHSM key store connection requires one
--     free IP address in each of the associated private subnets, although
--     two are preferable. For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @INVALID_CREDENTIALS@ — The @KeyStorePassword@ for the custom key
--     store doesn\'t match the current password of the @kmsuser@ crypto
--     user in the CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must change the @kmsuser@ account
--     password and update the @KeyStorePassword@ value for the custom key
--     store.
--
-- -   @SUBNET_NOT_FOUND@ — A subnet in the CloudHSM cluster configuration
--     was deleted. If KMS cannot find all of the subnets in the cluster
--     configuration, attempts to connect the custom key store to the
--     CloudHSM cluster fail. To fix this error, create a cluster from a
--     recent backup and associate it with your custom key store. (This
--     process creates a new cluster configuration with a VPC and private
--     subnets.) For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_LOCKED_OUT@ — The @kmsuser@ CU account is locked out of the
--     associated CloudHSM cluster due to too many failed password
--     attempts. Before you can connect your custom key store to its
--     CloudHSM cluster, you must change the @kmsuser@ account password and
--     update the key store password value for the custom key store.
--
-- -   @USER_LOGGED_IN@ — The @kmsuser@ CU account is logged into the
--     associated CloudHSM cluster. This prevents KMS from rotating the
--     @kmsuser@ account password and logging into the cluster. Before you
--     can connect your custom key store to its CloudHSM cluster, you must
--     log the @kmsuser@ CU out of the cluster. If you changed the
--     @kmsuser@ password to log into the cluster, you must also and update
--     the key store password value for the custom key store. For help, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_NOT_FOUND@ — KMS cannot find a @kmsuser@ CU account in the
--     associated CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must create a @kmsuser@ CU
--     account in the cluster, and then update the key store password value
--     for the custom key store.
--
-- __External key stores:__
--
-- -   @INVALID_CREDENTIALS@ — One or both of the
--     @XksProxyAuthenticationCredential@ values is not valid on the
--     specified external key store proxy.
--
-- -   @XKS_PROXY_ACCESS_DENIED@ — KMS requests are denied access to the
--     external key store proxy. If the external key store proxy has
--     authorization rules, verify that they permit KMS to communicate with
--     the proxy on your behalf.
--
-- -   @XKS_PROXY_INVALID_CONFIGURATION@ — A configuration error is
--     preventing the external key store from connecting to its proxy.
--     Verify the value of the @XksProxyUriPath@.
--
-- -   @XKS_PROXY_INVALID_RESPONSE@ — KMS cannot interpret the response
--     from the external key store proxy. If you see this connection error
--     code repeatedly, notify your external key store proxy vendor.
--
-- -   @XKS_PROXY_INVALID_TLS_CONFIGURATION@ — KMS cannot connect to the
--     external key store proxy because the TLS configuration is invalid.
--     Verify that the XKS proxy supports TLS 1.2 or 1.3. Also, verify that
--     the TLS certificate is not expired, and that it matches the hostname
--     in the @XksProxyUriEndpoint@ value, and that it is signed by a
--     certificate authority included in the
--     <https://github.com/aws/aws-kms-xksproxy-api-spec/blob/main/TrustedCertificateAuthorities Trusted Certificate Authorities>
--     list.
--
-- -   @XKS_PROXY_NOT_REACHABLE@ — KMS can\'t communicate with your
--     external key store proxy. Verify that the @XksProxyUriEndpoint@ and
--     @XksProxyUriPath@ are correct. Use the tools for your external key
--     store proxy to verify that the proxy is active and available on its
--     network. Also, verify that your external key manager instances are
--     operating properly. Connection attempts fail with this connection
--     error code if the proxy reports that all external key manager
--     instances are unavailable.
--
-- -   @XKS_PROXY_TIMED_OUT@ — KMS can connect to the external key store
--     proxy, but the proxy does not respond to KMS in the time allotted.
--     If you see this connection error code repeatedly, notify your
--     external key store proxy vendor.
--
-- -   @XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION@ — The Amazon VPC
--     endpoint service configuration doesn\'t conform to the requirements
--     for an KMS external key store.
--
--     -   The VPC endpoint service must be an endpoint service for
--         interface endpoints in the caller\'s Amazon Web Services
--         account.
--
--     -   It must have a network load balancer (NLB) connected to at least
--         two subnets, each in a different Availability Zone.
--
--     -   The @Allow principals@ list must include the KMS service
--         principal for the Region, @cks.kms.\<region>.amazonaws.com@,
--         such as @cks.kms.us-east-1.amazonaws.com@.
--
--     -   It must /not/ require
--         <https://docs.aws.amazon.com/vpc/latest/privatelink/create-endpoint-service.html acceptance>
--         of connection requests.
--
--     -   It must have a private DNS name. The private DNS name for an
--         external key store with @VPC_ENDPOINT_SERVICE@ connectivity must
--         be unique in its Amazon Web Services Region.
--
--     -   The domain of the private DNS name must have a
--         <https://docs.aws.amazon.com/vpc/latest/privatelink/verify-domains.html verification status>
--         of @verified@.
--
--     -   The
--         <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS certificate>
--         specifies the private DNS hostname at which the endpoint is
--         reachable.
--
-- -   @XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND@ — KMS can\'t find the VPC
--     endpoint service that it uses to communicate with the external key
--     store proxy. Verify that the @XksProxyVpcEndpointServiceName@ is
--     correct and the KMS service principal has service consumer
--     permissions on the Amazon VPC endpoint service.
--
-- 'connectionState', 'customKeyStoresListEntry_connectionState' - Indicates whether the custom key store is connected to its backing key
-- store. For an CloudHSM key store, the @ConnectionState@ indicates
-- whether it is connected to its CloudHSM cluster. For an external key
-- store, the @ConnectionState@ indicates whether it is connected to the
-- external key store proxy that communicates with your external key
-- manager.
--
-- You can create and use KMS keys in your custom key stores only when its
-- @ConnectionState@ is @CONNECTED@.
--
-- The @ConnectionState@ value is @DISCONNECTED@ only if the key store has
-- never been connected or you use the DisconnectCustomKeyStore operation
-- to disconnect it. If the value is @CONNECTED@ but you are having trouble
-- using the custom key store, make sure that the backing key store is
-- reachable and active. For an CloudHSM key store, verify that its
-- associated CloudHSM cluster is active and contains at least one active
-- HSM. For an external key store, verify that the external key store proxy
-- and external key manager are connected and enabled.
--
-- A value of @FAILED@ indicates that an attempt to connect was
-- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
-- the cause of the failure. For help resolving a connection failure, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a custom key store>
-- in the /Key Management Service Developer Guide/.
--
-- 'creationDate', 'customKeyStoresListEntry_creationDate' - The date and time when the custom key store was created.
--
-- 'customKeyStoreId', 'customKeyStoresListEntry_customKeyStoreId' - A unique identifier for the custom key store.
--
-- 'customKeyStoreName', 'customKeyStoresListEntry_customKeyStoreName' - The user-specified friendly name for the custom key store.
--
-- 'customKeyStoreType', 'customKeyStoresListEntry_customKeyStoreType' - Indicates the type of the custom key store. @AWS_CLOUDHSM@ indicates a
-- custom key store backed by an CloudHSM cluster. @EXTERNAL_KEY_STORE@
-- indicates a custom key store backed by an external key store proxy and
-- external key manager outside of Amazon Web Services.
--
-- 'trustAnchorCertificate', 'customKeyStoresListEntry_trustAnchorCertificate' - The trust anchor certificate of the CloudHSM cluster associated with an
-- CloudHSM key store. When you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
-- you create this certificate and save it in the @customerCA.crt@ file.
--
-- This field appears only when the @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
--
-- 'xksProxyConfiguration', 'customKeyStoresListEntry_xksProxyConfiguration' - Configuration settings for the external key store proxy (XKS proxy). The
-- external key store proxy translates KMS requests into a format that your
-- external key manager can understand. The proxy configuration includes
-- connection information that KMS requires.
--
-- This field appears only when the @CustomKeyStoreType@ is
-- @EXTERNAL_KEY_STORE@.
newCustomKeyStoresListEntry ::
  CustomKeyStoresListEntry
newCustomKeyStoresListEntry =
  CustomKeyStoresListEntry'
    { cloudHsmClusterId =
        Prelude.Nothing,
      connectionErrorCode = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      customKeyStoreId = Prelude.Nothing,
      customKeyStoreName = Prelude.Nothing,
      customKeyStoreType = Prelude.Nothing,
      trustAnchorCertificate = Prelude.Nothing,
      xksProxyConfiguration = Prelude.Nothing
    }

-- | A unique identifier for the CloudHSM cluster that is associated with an
-- CloudHSM key store. This field appears only when the
-- @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
customKeyStoresListEntry_cloudHsmClusterId :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_cloudHsmClusterId = Lens.lens (\CustomKeyStoresListEntry' {cloudHsmClusterId} -> cloudHsmClusterId) (\s@CustomKeyStoresListEntry' {} a -> s {cloudHsmClusterId = a} :: CustomKeyStoresListEntry)

-- | Describes the connection error. This field appears in the response only
-- when the @ConnectionState@ is @FAILED@.
--
-- Many failures can be resolved by updating the properties of the custom
-- key store. To update a custom key store, disconnect it
-- (DisconnectCustomKeyStore), correct the errors (UpdateCustomKeyStore),
-- and try to connect again (ConnectCustomKeyStore). For additional help
-- resolving these errors, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
-- in /Key Management Service Developer Guide/.
--
-- __All custom key stores:__
--
-- -   @INTERNAL_ERROR@ — KMS could not complete the request due to an
--     internal error. Retry the request. For @ConnectCustomKeyStore@
--     requests, disconnect the custom key store before trying to connect
--     again.
--
-- -   @NETWORK_ERRORS@ — Network errors are preventing KMS from connecting
--     the custom key store to its backing key store.
--
-- __CloudHSM key stores:__
--
-- -   @CLUSTER_NOT_FOUND@ — KMS cannot find the CloudHSM cluster with the
--     specified cluster ID.
--
-- -   @INSUFFICIENT_CLOUDHSM_HSMS@ — The associated CloudHSM cluster does
--     not contain any active HSMs. To connect a custom key store to its
--     CloudHSM cluster, the cluster must contain at least one active HSM.
--
-- -   @INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET@ — At least one private
--     subnet associated with the CloudHSM cluster doesn\'t have any
--     available IP addresses. A CloudHSM key store connection requires one
--     free IP address in each of the associated private subnets, although
--     two are preferable. For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @INVALID_CREDENTIALS@ — The @KeyStorePassword@ for the custom key
--     store doesn\'t match the current password of the @kmsuser@ crypto
--     user in the CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must change the @kmsuser@ account
--     password and update the @KeyStorePassword@ value for the custom key
--     store.
--
-- -   @SUBNET_NOT_FOUND@ — A subnet in the CloudHSM cluster configuration
--     was deleted. If KMS cannot find all of the subnets in the cluster
--     configuration, attempts to connect the custom key store to the
--     CloudHSM cluster fail. To fix this error, create a cluster from a
--     recent backup and associate it with your custom key store. (This
--     process creates a new cluster configuration with a VPC and private
--     subnets.) For details, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-failed How to Fix a Connection Failure>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_LOCKED_OUT@ — The @kmsuser@ CU account is locked out of the
--     associated CloudHSM cluster due to too many failed password
--     attempts. Before you can connect your custom key store to its
--     CloudHSM cluster, you must change the @kmsuser@ account password and
--     update the key store password value for the custom key store.
--
-- -   @USER_LOGGED_IN@ — The @kmsuser@ CU account is logged into the
--     associated CloudHSM cluster. This prevents KMS from rotating the
--     @kmsuser@ account password and logging into the cluster. Before you
--     can connect your custom key store to its CloudHSM cluster, you must
--     log the @kmsuser@ CU out of the cluster. If you changed the
--     @kmsuser@ password to log into the cluster, you must also and update
--     the key store password value for the custom key store. For help, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#login-kmsuser-2 How to Log Out and Reconnect>
--     in the /Key Management Service Developer Guide/.
--
-- -   @USER_NOT_FOUND@ — KMS cannot find a @kmsuser@ CU account in the
--     associated CloudHSM cluster. Before you can connect your custom key
--     store to its CloudHSM cluster, you must create a @kmsuser@ CU
--     account in the cluster, and then update the key store password value
--     for the custom key store.
--
-- __External key stores:__
--
-- -   @INVALID_CREDENTIALS@ — One or both of the
--     @XksProxyAuthenticationCredential@ values is not valid on the
--     specified external key store proxy.
--
-- -   @XKS_PROXY_ACCESS_DENIED@ — KMS requests are denied access to the
--     external key store proxy. If the external key store proxy has
--     authorization rules, verify that they permit KMS to communicate with
--     the proxy on your behalf.
--
-- -   @XKS_PROXY_INVALID_CONFIGURATION@ — A configuration error is
--     preventing the external key store from connecting to its proxy.
--     Verify the value of the @XksProxyUriPath@.
--
-- -   @XKS_PROXY_INVALID_RESPONSE@ — KMS cannot interpret the response
--     from the external key store proxy. If you see this connection error
--     code repeatedly, notify your external key store proxy vendor.
--
-- -   @XKS_PROXY_INVALID_TLS_CONFIGURATION@ — KMS cannot connect to the
--     external key store proxy because the TLS configuration is invalid.
--     Verify that the XKS proxy supports TLS 1.2 or 1.3. Also, verify that
--     the TLS certificate is not expired, and that it matches the hostname
--     in the @XksProxyUriEndpoint@ value, and that it is signed by a
--     certificate authority included in the
--     <https://github.com/aws/aws-kms-xksproxy-api-spec/blob/main/TrustedCertificateAuthorities Trusted Certificate Authorities>
--     list.
--
-- -   @XKS_PROXY_NOT_REACHABLE@ — KMS can\'t communicate with your
--     external key store proxy. Verify that the @XksProxyUriEndpoint@ and
--     @XksProxyUriPath@ are correct. Use the tools for your external key
--     store proxy to verify that the proxy is active and available on its
--     network. Also, verify that your external key manager instances are
--     operating properly. Connection attempts fail with this connection
--     error code if the proxy reports that all external key manager
--     instances are unavailable.
--
-- -   @XKS_PROXY_TIMED_OUT@ — KMS can connect to the external key store
--     proxy, but the proxy does not respond to KMS in the time allotted.
--     If you see this connection error code repeatedly, notify your
--     external key store proxy vendor.
--
-- -   @XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION@ — The Amazon VPC
--     endpoint service configuration doesn\'t conform to the requirements
--     for an KMS external key store.
--
--     -   The VPC endpoint service must be an endpoint service for
--         interface endpoints in the caller\'s Amazon Web Services
--         account.
--
--     -   It must have a network load balancer (NLB) connected to at least
--         two subnets, each in a different Availability Zone.
--
--     -   The @Allow principals@ list must include the KMS service
--         principal for the Region, @cks.kms.\<region>.amazonaws.com@,
--         such as @cks.kms.us-east-1.amazonaws.com@.
--
--     -   It must /not/ require
--         <https://docs.aws.amazon.com/vpc/latest/privatelink/create-endpoint-service.html acceptance>
--         of connection requests.
--
--     -   It must have a private DNS name. The private DNS name for an
--         external key store with @VPC_ENDPOINT_SERVICE@ connectivity must
--         be unique in its Amazon Web Services Region.
--
--     -   The domain of the private DNS name must have a
--         <https://docs.aws.amazon.com/vpc/latest/privatelink/verify-domains.html verification status>
--         of @verified@.
--
--     -   The
--         <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS certificate>
--         specifies the private DNS hostname at which the endpoint is
--         reachable.
--
-- -   @XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND@ — KMS can\'t find the VPC
--     endpoint service that it uses to communicate with the external key
--     store proxy. Verify that the @XksProxyVpcEndpointServiceName@ is
--     correct and the KMS service principal has service consumer
--     permissions on the Amazon VPC endpoint service.
customKeyStoresListEntry_connectionErrorCode :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe ConnectionErrorCodeType)
customKeyStoresListEntry_connectionErrorCode = Lens.lens (\CustomKeyStoresListEntry' {connectionErrorCode} -> connectionErrorCode) (\s@CustomKeyStoresListEntry' {} a -> s {connectionErrorCode = a} :: CustomKeyStoresListEntry)

-- | Indicates whether the custom key store is connected to its backing key
-- store. For an CloudHSM key store, the @ConnectionState@ indicates
-- whether it is connected to its CloudHSM cluster. For an external key
-- store, the @ConnectionState@ indicates whether it is connected to the
-- external key store proxy that communicates with your external key
-- manager.
--
-- You can create and use KMS keys in your custom key stores only when its
-- @ConnectionState@ is @CONNECTED@.
--
-- The @ConnectionState@ value is @DISCONNECTED@ only if the key store has
-- never been connected or you use the DisconnectCustomKeyStore operation
-- to disconnect it. If the value is @CONNECTED@ but you are having trouble
-- using the custom key store, make sure that the backing key store is
-- reachable and active. For an CloudHSM key store, verify that its
-- associated CloudHSM cluster is active and contains at least one active
-- HSM. For an external key store, verify that the external key store proxy
-- and external key manager are connected and enabled.
--
-- A value of @FAILED@ indicates that an attempt to connect was
-- unsuccessful. The @ConnectionErrorCode@ field in the response indicates
-- the cause of the failure. For help resolving a connection failure, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html Troubleshooting a custom key store>
-- in the /Key Management Service Developer Guide/.
customKeyStoresListEntry_connectionState :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe ConnectionStateType)
customKeyStoresListEntry_connectionState = Lens.lens (\CustomKeyStoresListEntry' {connectionState} -> connectionState) (\s@CustomKeyStoresListEntry' {} a -> s {connectionState = a} :: CustomKeyStoresListEntry)

-- | The date and time when the custom key store was created.
customKeyStoresListEntry_creationDate :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.UTCTime)
customKeyStoresListEntry_creationDate = Lens.lens (\CustomKeyStoresListEntry' {creationDate} -> creationDate) (\s@CustomKeyStoresListEntry' {} a -> s {creationDate = a} :: CustomKeyStoresListEntry) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the custom key store.
customKeyStoresListEntry_customKeyStoreId :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_customKeyStoreId = Lens.lens (\CustomKeyStoresListEntry' {customKeyStoreId} -> customKeyStoreId) (\s@CustomKeyStoresListEntry' {} a -> s {customKeyStoreId = a} :: CustomKeyStoresListEntry)

-- | The user-specified friendly name for the custom key store.
customKeyStoresListEntry_customKeyStoreName :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_customKeyStoreName = Lens.lens (\CustomKeyStoresListEntry' {customKeyStoreName} -> customKeyStoreName) (\s@CustomKeyStoresListEntry' {} a -> s {customKeyStoreName = a} :: CustomKeyStoresListEntry)

-- | Indicates the type of the custom key store. @AWS_CLOUDHSM@ indicates a
-- custom key store backed by an CloudHSM cluster. @EXTERNAL_KEY_STORE@
-- indicates a custom key store backed by an external key store proxy and
-- external key manager outside of Amazon Web Services.
customKeyStoresListEntry_customKeyStoreType :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe CustomKeyStoreType)
customKeyStoresListEntry_customKeyStoreType = Lens.lens (\CustomKeyStoresListEntry' {customKeyStoreType} -> customKeyStoreType) (\s@CustomKeyStoresListEntry' {} a -> s {customKeyStoreType = a} :: CustomKeyStoresListEntry)

-- | The trust anchor certificate of the CloudHSM cluster associated with an
-- CloudHSM key store. When you
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/initialize-cluster.html#sign-csr initialize the cluster>,
-- you create this certificate and save it in the @customerCA.crt@ file.
--
-- This field appears only when the @CustomKeyStoreType@ is @AWS_CLOUDHSM@.
customKeyStoresListEntry_trustAnchorCertificate :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe Prelude.Text)
customKeyStoresListEntry_trustAnchorCertificate = Lens.lens (\CustomKeyStoresListEntry' {trustAnchorCertificate} -> trustAnchorCertificate) (\s@CustomKeyStoresListEntry' {} a -> s {trustAnchorCertificate = a} :: CustomKeyStoresListEntry)

-- | Configuration settings for the external key store proxy (XKS proxy). The
-- external key store proxy translates KMS requests into a format that your
-- external key manager can understand. The proxy configuration includes
-- connection information that KMS requires.
--
-- This field appears only when the @CustomKeyStoreType@ is
-- @EXTERNAL_KEY_STORE@.
customKeyStoresListEntry_xksProxyConfiguration :: Lens.Lens' CustomKeyStoresListEntry (Prelude.Maybe XksProxyConfigurationType)
customKeyStoresListEntry_xksProxyConfiguration = Lens.lens (\CustomKeyStoresListEntry' {xksProxyConfiguration} -> xksProxyConfiguration) (\s@CustomKeyStoresListEntry' {} a -> s {xksProxyConfiguration = a} :: CustomKeyStoresListEntry)

instance Data.FromJSON CustomKeyStoresListEntry where
  parseJSON =
    Data.withObject
      "CustomKeyStoresListEntry"
      ( \x ->
          CustomKeyStoresListEntry'
            Prelude.<$> (x Data..:? "CloudHsmClusterId")
            Prelude.<*> (x Data..:? "ConnectionErrorCode")
            Prelude.<*> (x Data..:? "ConnectionState")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CustomKeyStoreId")
            Prelude.<*> (x Data..:? "CustomKeyStoreName")
            Prelude.<*> (x Data..:? "CustomKeyStoreType")
            Prelude.<*> (x Data..:? "TrustAnchorCertificate")
            Prelude.<*> (x Data..:? "XksProxyConfiguration")
      )

instance Prelude.Hashable CustomKeyStoresListEntry where
  hashWithSalt _salt CustomKeyStoresListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` cloudHsmClusterId
      `Prelude.hashWithSalt` connectionErrorCode
      `Prelude.hashWithSalt` connectionState
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` customKeyStoreId
      `Prelude.hashWithSalt` customKeyStoreName
      `Prelude.hashWithSalt` customKeyStoreType
      `Prelude.hashWithSalt` trustAnchorCertificate
      `Prelude.hashWithSalt` xksProxyConfiguration

instance Prelude.NFData CustomKeyStoresListEntry where
  rnf CustomKeyStoresListEntry' {..} =
    Prelude.rnf cloudHsmClusterId
      `Prelude.seq` Prelude.rnf connectionErrorCode
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf customKeyStoreId
      `Prelude.seq` Prelude.rnf customKeyStoreName
      `Prelude.seq` Prelude.rnf customKeyStoreType
      `Prelude.seq` Prelude.rnf trustAnchorCertificate
      `Prelude.seq` Prelude.rnf xksProxyConfiguration
