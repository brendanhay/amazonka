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
-- Module      : Amazonka.KMS.Types.XksProxyConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.XksProxyConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.XksProxyConnectivityType
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the external key store proxy (XKS proxy).
-- Your external key store proxy translates KMS requests into a format that
-- your external key manager can understand. These fields appear in a
-- DescribeCustomKeyStores response only when the @CustomKeyStoreType@ is
-- @EXTERNAL_KEY_STORE@.
--
-- /See:/ 'newXksProxyConfigurationType' smart constructor.
data XksProxyConfigurationType = XksProxyConfigurationType'
  { -- | The part of the external key store
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateCustomKeyStore.html#KMS-CreateCustomKeyStore-request-XksProxyAuthenticationCredential proxy authentication credential>
    -- that uniquely identifies the secret access key.
    accessKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether the external key store proxy uses a public endpoint or
    -- an Amazon VPC endpoint service to communicate with KMS.
    connectivity :: Prelude.Maybe XksProxyConnectivityType,
    -- | The URI endpoint for the external key store proxy.
    --
    -- If the external key store proxy has a public endpoint, it is displayed
    -- here.
    --
    -- If the external key store proxy uses an Amazon VPC endpoint service
    -- name, this field displays the private DNS name associated with the VPC
    -- endpoint service.
    uriEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The path to the external key store proxy APIs.
    uriPath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon VPC endpoint service used to communicate with the external
    -- key store proxy. This field appears only when the external key store
    -- proxy uses an Amazon VPC endpoint service to communicate with KMS.
    vpcEndpointServiceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XksProxyConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'xksProxyConfigurationType_accessKeyId' - The part of the external key store
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateCustomKeyStore.html#KMS-CreateCustomKeyStore-request-XksProxyAuthenticationCredential proxy authentication credential>
-- that uniquely identifies the secret access key.
--
-- 'connectivity', 'xksProxyConfigurationType_connectivity' - Indicates whether the external key store proxy uses a public endpoint or
-- an Amazon VPC endpoint service to communicate with KMS.
--
-- 'uriEndpoint', 'xksProxyConfigurationType_uriEndpoint' - The URI endpoint for the external key store proxy.
--
-- If the external key store proxy has a public endpoint, it is displayed
-- here.
--
-- If the external key store proxy uses an Amazon VPC endpoint service
-- name, this field displays the private DNS name associated with the VPC
-- endpoint service.
--
-- 'uriPath', 'xksProxyConfigurationType_uriPath' - The path to the external key store proxy APIs.
--
-- 'vpcEndpointServiceName', 'xksProxyConfigurationType_vpcEndpointServiceName' - The Amazon VPC endpoint service used to communicate with the external
-- key store proxy. This field appears only when the external key store
-- proxy uses an Amazon VPC endpoint service to communicate with KMS.
newXksProxyConfigurationType ::
  XksProxyConfigurationType
newXksProxyConfigurationType =
  XksProxyConfigurationType'
    { accessKeyId =
        Prelude.Nothing,
      connectivity = Prelude.Nothing,
      uriEndpoint = Prelude.Nothing,
      uriPath = Prelude.Nothing,
      vpcEndpointServiceName = Prelude.Nothing
    }

-- | The part of the external key store
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateCustomKeyStore.html#KMS-CreateCustomKeyStore-request-XksProxyAuthenticationCredential proxy authentication credential>
-- that uniquely identifies the secret access key.
xksProxyConfigurationType_accessKeyId :: Lens.Lens' XksProxyConfigurationType (Prelude.Maybe Prelude.Text)
xksProxyConfigurationType_accessKeyId = Lens.lens (\XksProxyConfigurationType' {accessKeyId} -> accessKeyId) (\s@XksProxyConfigurationType' {} a -> s {accessKeyId = a} :: XksProxyConfigurationType) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether the external key store proxy uses a public endpoint or
-- an Amazon VPC endpoint service to communicate with KMS.
xksProxyConfigurationType_connectivity :: Lens.Lens' XksProxyConfigurationType (Prelude.Maybe XksProxyConnectivityType)
xksProxyConfigurationType_connectivity = Lens.lens (\XksProxyConfigurationType' {connectivity} -> connectivity) (\s@XksProxyConfigurationType' {} a -> s {connectivity = a} :: XksProxyConfigurationType)

-- | The URI endpoint for the external key store proxy.
--
-- If the external key store proxy has a public endpoint, it is displayed
-- here.
--
-- If the external key store proxy uses an Amazon VPC endpoint service
-- name, this field displays the private DNS name associated with the VPC
-- endpoint service.
xksProxyConfigurationType_uriEndpoint :: Lens.Lens' XksProxyConfigurationType (Prelude.Maybe Prelude.Text)
xksProxyConfigurationType_uriEndpoint = Lens.lens (\XksProxyConfigurationType' {uriEndpoint} -> uriEndpoint) (\s@XksProxyConfigurationType' {} a -> s {uriEndpoint = a} :: XksProxyConfigurationType)

-- | The path to the external key store proxy APIs.
xksProxyConfigurationType_uriPath :: Lens.Lens' XksProxyConfigurationType (Prelude.Maybe Prelude.Text)
xksProxyConfigurationType_uriPath = Lens.lens (\XksProxyConfigurationType' {uriPath} -> uriPath) (\s@XksProxyConfigurationType' {} a -> s {uriPath = a} :: XksProxyConfigurationType)

-- | The Amazon VPC endpoint service used to communicate with the external
-- key store proxy. This field appears only when the external key store
-- proxy uses an Amazon VPC endpoint service to communicate with KMS.
xksProxyConfigurationType_vpcEndpointServiceName :: Lens.Lens' XksProxyConfigurationType (Prelude.Maybe Prelude.Text)
xksProxyConfigurationType_vpcEndpointServiceName = Lens.lens (\XksProxyConfigurationType' {vpcEndpointServiceName} -> vpcEndpointServiceName) (\s@XksProxyConfigurationType' {} a -> s {vpcEndpointServiceName = a} :: XksProxyConfigurationType)

instance Data.FromJSON XksProxyConfigurationType where
  parseJSON =
    Data.withObject
      "XksProxyConfigurationType"
      ( \x ->
          XksProxyConfigurationType'
            Prelude.<$> (x Data..:? "AccessKeyId")
            Prelude.<*> (x Data..:? "Connectivity")
            Prelude.<*> (x Data..:? "UriEndpoint")
            Prelude.<*> (x Data..:? "UriPath")
            Prelude.<*> (x Data..:? "VpcEndpointServiceName")
      )

instance Prelude.Hashable XksProxyConfigurationType where
  hashWithSalt _salt XksProxyConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` connectivity
      `Prelude.hashWithSalt` uriEndpoint
      `Prelude.hashWithSalt` uriPath
      `Prelude.hashWithSalt` vpcEndpointServiceName

instance Prelude.NFData XksProxyConfigurationType where
  rnf XksProxyConfigurationType' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf connectivity
      `Prelude.seq` Prelude.rnf uriEndpoint
      `Prelude.seq` Prelude.rnf uriPath
      `Prelude.seq` Prelude.rnf vpcEndpointServiceName
