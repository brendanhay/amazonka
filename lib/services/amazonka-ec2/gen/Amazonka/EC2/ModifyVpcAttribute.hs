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
-- Module      : Amazonka.EC2.ModifyVpcAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
module Amazonka.EC2.ModifyVpcAttribute
  ( -- * Creating a Request
    ModifyVpcAttribute (..),
    newModifyVpcAttribute,

    -- * Request Lenses
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_enableNetworkAddressUsageMetrics,
    modifyVpcAttribute_vpcId,

    -- * Destructuring the Response
    ModifyVpcAttributeResponse (..),
    newModifyVpcAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcAttribute' smart constructor.
data ModifyVpcAttribute = ModifyVpcAttribute'
  { -- | Indicates whether the instances launched in the VPC get DNS hostnames.
    -- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
    -- not.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the
    -- same request. Use separate requests for each attribute. You can only
    -- enable DNS hostnames if you\'ve enabled DNS support.
    enableDnsHostnames :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates whether the DNS resolution is supported for the VPC. If
    -- enabled, queries to the Amazon provided DNS server at the
    -- 169.254.169.253 IP address, or the reserved IP address at the base of
    -- the VPC network range \"plus two\" succeed. If disabled, the Amazon
    -- provided DNS service in the VPC that resolves public DNS hostnames to IP
    -- addresses is not enabled.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the
    -- same request. Use separate requests for each attribute.
    enableDnsSupport :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates whether Network Address Usage metrics are enabled for your
    -- VPC.
    enableNetworkAddressUsageMetrics :: Prelude.Maybe AttributeBooleanValue,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableDnsHostnames', 'modifyVpcAttribute_enableDnsHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute. You can only
-- enable DNS hostnames if you\'ve enabled DNS support.
--
-- 'enableDnsSupport', 'modifyVpcAttribute_enableDnsSupport' - Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of
-- the VPC network range \"plus two\" succeed. If disabled, the Amazon
-- provided DNS service in the VPC that resolves public DNS hostnames to IP
-- addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute.
--
-- 'enableNetworkAddressUsageMetrics', 'modifyVpcAttribute_enableNetworkAddressUsageMetrics' - Indicates whether Network Address Usage metrics are enabled for your
-- VPC.
--
-- 'vpcId', 'modifyVpcAttribute_vpcId' - The ID of the VPC.
newModifyVpcAttribute ::
  -- | 'vpcId'
  Prelude.Text ->
  ModifyVpcAttribute
newModifyVpcAttribute pVpcId_ =
  ModifyVpcAttribute'
    { enableDnsHostnames =
        Prelude.Nothing,
      enableDnsSupport = Prelude.Nothing,
      enableNetworkAddressUsageMetrics = Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute. You can only
-- enable DNS hostnames if you\'ve enabled DNS support.
modifyVpcAttribute_enableDnsHostnames :: Lens.Lens' ModifyVpcAttribute (Prelude.Maybe AttributeBooleanValue)
modifyVpcAttribute_enableDnsHostnames = Lens.lens (\ModifyVpcAttribute' {enableDnsHostnames} -> enableDnsHostnames) (\s@ModifyVpcAttribute' {} a -> s {enableDnsHostnames = a} :: ModifyVpcAttribute)

-- | Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of
-- the VPC network range \"plus two\" succeed. If disabled, the Amazon
-- provided DNS service in the VPC that resolves public DNS hostnames to IP
-- addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute.
modifyVpcAttribute_enableDnsSupport :: Lens.Lens' ModifyVpcAttribute (Prelude.Maybe AttributeBooleanValue)
modifyVpcAttribute_enableDnsSupport = Lens.lens (\ModifyVpcAttribute' {enableDnsSupport} -> enableDnsSupport) (\s@ModifyVpcAttribute' {} a -> s {enableDnsSupport = a} :: ModifyVpcAttribute)

-- | Indicates whether Network Address Usage metrics are enabled for your
-- VPC.
modifyVpcAttribute_enableNetworkAddressUsageMetrics :: Lens.Lens' ModifyVpcAttribute (Prelude.Maybe AttributeBooleanValue)
modifyVpcAttribute_enableNetworkAddressUsageMetrics = Lens.lens (\ModifyVpcAttribute' {enableNetworkAddressUsageMetrics} -> enableNetworkAddressUsageMetrics) (\s@ModifyVpcAttribute' {} a -> s {enableNetworkAddressUsageMetrics = a} :: ModifyVpcAttribute)

-- | The ID of the VPC.
modifyVpcAttribute_vpcId :: Lens.Lens' ModifyVpcAttribute Prelude.Text
modifyVpcAttribute_vpcId = Lens.lens (\ModifyVpcAttribute' {vpcId} -> vpcId) (\s@ModifyVpcAttribute' {} a -> s {vpcId = a} :: ModifyVpcAttribute)

instance Core.AWSRequest ModifyVpcAttribute where
  type
    AWSResponse ModifyVpcAttribute =
      ModifyVpcAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ModifyVpcAttributeResponse'

instance Prelude.Hashable ModifyVpcAttribute where
  hashWithSalt _salt ModifyVpcAttribute' {..} =
    _salt `Prelude.hashWithSalt` enableDnsHostnames
      `Prelude.hashWithSalt` enableDnsSupport
      `Prelude.hashWithSalt` enableNetworkAddressUsageMetrics
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ModifyVpcAttribute where
  rnf ModifyVpcAttribute' {..} =
    Prelude.rnf enableDnsHostnames
      `Prelude.seq` Prelude.rnf enableDnsSupport
      `Prelude.seq` Prelude.rnf enableNetworkAddressUsageMetrics
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToHeaders ModifyVpcAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVpcAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVpcAttribute where
  toQuery ModifyVpcAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVpcAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "EnableDnsHostnames" Data.=: enableDnsHostnames,
        "EnableDnsSupport" Data.=: enableDnsSupport,
        "EnableNetworkAddressUsageMetrics"
          Data.=: enableNetworkAddressUsageMetrics,
        "VpcId" Data.=: vpcId
      ]

-- | /See:/ 'newModifyVpcAttributeResponse' smart constructor.
data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyVpcAttributeResponse ::
  ModifyVpcAttributeResponse
newModifyVpcAttributeResponse =
  ModifyVpcAttributeResponse'

instance Prelude.NFData ModifyVpcAttributeResponse where
  rnf _ = ()
