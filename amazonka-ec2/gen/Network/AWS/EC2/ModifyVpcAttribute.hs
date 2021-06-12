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
-- Module      : Network.AWS.EC2.ModifyVpcAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified VPC.
module Network.AWS.EC2.ModifyVpcAttribute
  ( -- * Creating a Request
    ModifyVpcAttribute (..),
    newModifyVpcAttribute,

    -- * Request Lenses
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_vpcId,

    -- * Destructuring the Response
    ModifyVpcAttributeResponse (..),
    newModifyVpcAttributeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcAttribute' smart constructor.
data ModifyVpcAttribute = ModifyVpcAttribute'
  { -- | Indicates whether the DNS resolution is supported for the VPC. If
    -- enabled, queries to the Amazon provided DNS server at the
    -- 169.254.169.253 IP address, or the reserved IP address at the base of
    -- the VPC network range \"plus two\" succeed. If disabled, the Amazon
    -- provided DNS service in the VPC that resolves public DNS hostnames to IP
    -- addresses is not enabled.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the
    -- same request. Use separate requests for each attribute.
    enableDnsSupport :: Core.Maybe AttributeBooleanValue,
    -- | Indicates whether the instances launched in the VPC get DNS hostnames.
    -- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
    -- not.
    --
    -- You cannot modify the DNS resolution and DNS hostnames attributes in the
    -- same request. Use separate requests for each attribute. You can only
    -- enable DNS hostnames if you\'ve enabled DNS support.
    enableDnsHostnames :: Core.Maybe AttributeBooleanValue,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'enableDnsHostnames', 'modifyVpcAttribute_enableDnsHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute. You can only
-- enable DNS hostnames if you\'ve enabled DNS support.
--
-- 'vpcId', 'modifyVpcAttribute_vpcId' - The ID of the VPC.
newModifyVpcAttribute ::
  -- | 'vpcId'
  Core.Text ->
  ModifyVpcAttribute
newModifyVpcAttribute pVpcId_ =
  ModifyVpcAttribute'
    { enableDnsSupport =
        Core.Nothing,
      enableDnsHostnames = Core.Nothing,
      vpcId = pVpcId_
    }

-- | Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of
-- the VPC network range \"plus two\" succeed. If disabled, the Amazon
-- provided DNS service in the VPC that resolves public DNS hostnames to IP
-- addresses is not enabled.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute.
modifyVpcAttribute_enableDnsSupport :: Lens.Lens' ModifyVpcAttribute (Core.Maybe AttributeBooleanValue)
modifyVpcAttribute_enableDnsSupport = Lens.lens (\ModifyVpcAttribute' {enableDnsSupport} -> enableDnsSupport) (\s@ModifyVpcAttribute' {} a -> s {enableDnsSupport = a} :: ModifyVpcAttribute)

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If enabled, instances in the VPC get DNS hostnames; otherwise, they do
-- not.
--
-- You cannot modify the DNS resolution and DNS hostnames attributes in the
-- same request. Use separate requests for each attribute. You can only
-- enable DNS hostnames if you\'ve enabled DNS support.
modifyVpcAttribute_enableDnsHostnames :: Lens.Lens' ModifyVpcAttribute (Core.Maybe AttributeBooleanValue)
modifyVpcAttribute_enableDnsHostnames = Lens.lens (\ModifyVpcAttribute' {enableDnsHostnames} -> enableDnsHostnames) (\s@ModifyVpcAttribute' {} a -> s {enableDnsHostnames = a} :: ModifyVpcAttribute)

-- | The ID of the VPC.
modifyVpcAttribute_vpcId :: Lens.Lens' ModifyVpcAttribute Core.Text
modifyVpcAttribute_vpcId = Lens.lens (\ModifyVpcAttribute' {vpcId} -> vpcId) (\s@ModifyVpcAttribute' {} a -> s {vpcId = a} :: ModifyVpcAttribute)

instance Core.AWSRequest ModifyVpcAttribute where
  type
    AWSResponse ModifyVpcAttribute =
      ModifyVpcAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ModifyVpcAttributeResponse'

instance Core.Hashable ModifyVpcAttribute

instance Core.NFData ModifyVpcAttribute

instance Core.ToHeaders ModifyVpcAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyVpcAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ModifyVpcAttribute where
  toQuery ModifyVpcAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyVpcAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "EnableDnsSupport" Core.=: enableDnsSupport,
        "EnableDnsHostnames" Core.=: enableDnsHostnames,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newModifyVpcAttributeResponse' smart constructor.
data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyVpcAttributeResponse ::
  ModifyVpcAttributeResponse
newModifyVpcAttributeResponse =
  ModifyVpcAttributeResponse'

instance Core.NFData ModifyVpcAttributeResponse
