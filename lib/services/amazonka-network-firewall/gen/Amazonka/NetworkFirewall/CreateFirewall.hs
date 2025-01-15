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
-- Module      : Amazonka.NetworkFirewall.CreateFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Network Firewall Firewall and accompanying FirewallStatus for
-- a VPC.
--
-- The firewall defines the configuration settings for an Network Firewall
-- firewall. The settings that you can define at creation include the
-- firewall policy, the subnets in your VPC to use for the firewall
-- endpoints, and any tags that are attached to the firewall Amazon Web
-- Services resource.
--
-- After you create a firewall, you can provide additional settings, like
-- the logging configuration.
--
-- To update the settings for a firewall, you use the operations that apply
-- to the settings themselves, for example UpdateLoggingConfiguration,
-- AssociateSubnets, and UpdateFirewallDeleteProtection.
--
-- To manage a firewall\'s tags, use the standard Amazon Web Services
-- resource tagging operations, ListTagsForResource, TagResource, and
-- UntagResource.
--
-- To retrieve information about firewalls, use ListFirewalls and
-- DescribeFirewall.
module Amazonka.NetworkFirewall.CreateFirewall
  ( -- * Creating a Request
    CreateFirewall (..),
    newCreateFirewall,

    -- * Request Lenses
    createFirewall_deleteProtection,
    createFirewall_description,
    createFirewall_encryptionConfiguration,
    createFirewall_firewallPolicyChangeProtection,
    createFirewall_subnetChangeProtection,
    createFirewall_tags,
    createFirewall_firewallName,
    createFirewall_firewallPolicyArn,
    createFirewall_vpcId,
    createFirewall_subnetMappings,

    -- * Destructuring the Response
    CreateFirewallResponse (..),
    newCreateFirewallResponse,

    -- * Response Lenses
    createFirewallResponse_firewall,
    createFirewallResponse_firewallStatus,
    createFirewallResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFirewall' smart constructor.
data CreateFirewall = CreateFirewall'
  { -- | A flag indicating whether it is possible to delete the firewall. A
    -- setting of @TRUE@ indicates that the firewall is protected against
    -- deletion. Use this setting to protect against accidentally deleting a
    -- firewall that is in use. When you create a firewall, the operation
    -- initializes this flag to @TRUE@.
    deleteProtection :: Prelude.Maybe Prelude.Bool,
    -- | A description of the firewall.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains settings for encryption of your firewall
    -- resources.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | A setting indicating whether the firewall is protected against a change
    -- to the firewall policy association. Use this setting to protect against
    -- accidentally modifying the firewall policy for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | A setting indicating whether the firewall is protected against changes
    -- to the subnet associations. Use this setting to protect against
    -- accidentally modifying the subnet associations for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    subnetChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the FirewallPolicy that you want to
    -- use for the firewall.
    firewallPolicyArn :: Prelude.Text,
    -- | The unique identifier of the VPC where Network Firewall should create
    -- the firewall.
    --
    -- You can\'t change this setting after you create the firewall.
    vpcId :: Prelude.Text,
    -- | The public subnets to use for your Network Firewall firewalls. Each
    -- subnet must belong to a different Availability Zone in the VPC. Network
    -- Firewall creates a firewall endpoint in each subnet.
    subnetMappings :: [SubnetMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteProtection', 'createFirewall_deleteProtection' - A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
--
-- 'description', 'createFirewall_description' - A description of the firewall.
--
-- 'encryptionConfiguration', 'createFirewall_encryptionConfiguration' - A complex type that contains settings for encryption of your firewall
-- resources.
--
-- 'firewallPolicyChangeProtection', 'createFirewall_firewallPolicyChangeProtection' - A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'subnetChangeProtection', 'createFirewall_subnetChangeProtection' - A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'tags', 'createFirewall_tags' - The key:value pairs to associate with the resource.
--
-- 'firewallName', 'createFirewall_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'firewallPolicyArn', 'createFirewall_firewallPolicyArn' - The Amazon Resource Name (ARN) of the FirewallPolicy that you want to
-- use for the firewall.
--
-- 'vpcId', 'createFirewall_vpcId' - The unique identifier of the VPC where Network Firewall should create
-- the firewall.
--
-- You can\'t change this setting after you create the firewall.
--
-- 'subnetMappings', 'createFirewall_subnetMappings' - The public subnets to use for your Network Firewall firewalls. Each
-- subnet must belong to a different Availability Zone in the VPC. Network
-- Firewall creates a firewall endpoint in each subnet.
newCreateFirewall ::
  -- | 'firewallName'
  Prelude.Text ->
  -- | 'firewallPolicyArn'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateFirewall
newCreateFirewall
  pFirewallName_
  pFirewallPolicyArn_
  pVpcId_ =
    CreateFirewall'
      { deleteProtection = Prelude.Nothing,
        description = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        firewallPolicyChangeProtection = Prelude.Nothing,
        subnetChangeProtection = Prelude.Nothing,
        tags = Prelude.Nothing,
        firewallName = pFirewallName_,
        firewallPolicyArn = pFirewallPolicyArn_,
        vpcId = pVpcId_,
        subnetMappings = Prelude.mempty
      }

-- | A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
createFirewall_deleteProtection :: Lens.Lens' CreateFirewall (Prelude.Maybe Prelude.Bool)
createFirewall_deleteProtection = Lens.lens (\CreateFirewall' {deleteProtection} -> deleteProtection) (\s@CreateFirewall' {} a -> s {deleteProtection = a} :: CreateFirewall)

-- | A description of the firewall.
createFirewall_description :: Lens.Lens' CreateFirewall (Prelude.Maybe Prelude.Text)
createFirewall_description = Lens.lens (\CreateFirewall' {description} -> description) (\s@CreateFirewall' {} a -> s {description = a} :: CreateFirewall)

-- | A complex type that contains settings for encryption of your firewall
-- resources.
createFirewall_encryptionConfiguration :: Lens.Lens' CreateFirewall (Prelude.Maybe EncryptionConfiguration)
createFirewall_encryptionConfiguration = Lens.lens (\CreateFirewall' {encryptionConfiguration} -> encryptionConfiguration) (\s@CreateFirewall' {} a -> s {encryptionConfiguration = a} :: CreateFirewall)

-- | A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
createFirewall_firewallPolicyChangeProtection :: Lens.Lens' CreateFirewall (Prelude.Maybe Prelude.Bool)
createFirewall_firewallPolicyChangeProtection = Lens.lens (\CreateFirewall' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@CreateFirewall' {} a -> s {firewallPolicyChangeProtection = a} :: CreateFirewall)

-- | A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
createFirewall_subnetChangeProtection :: Lens.Lens' CreateFirewall (Prelude.Maybe Prelude.Bool)
createFirewall_subnetChangeProtection = Lens.lens (\CreateFirewall' {subnetChangeProtection} -> subnetChangeProtection) (\s@CreateFirewall' {} a -> s {subnetChangeProtection = a} :: CreateFirewall)

-- | The key:value pairs to associate with the resource.
createFirewall_tags :: Lens.Lens' CreateFirewall (Prelude.Maybe (Prelude.NonEmpty Tag))
createFirewall_tags = Lens.lens (\CreateFirewall' {tags} -> tags) (\s@CreateFirewall' {} a -> s {tags = a} :: CreateFirewall) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
createFirewall_firewallName :: Lens.Lens' CreateFirewall Prelude.Text
createFirewall_firewallName = Lens.lens (\CreateFirewall' {firewallName} -> firewallName) (\s@CreateFirewall' {} a -> s {firewallName = a} :: CreateFirewall)

-- | The Amazon Resource Name (ARN) of the FirewallPolicy that you want to
-- use for the firewall.
createFirewall_firewallPolicyArn :: Lens.Lens' CreateFirewall Prelude.Text
createFirewall_firewallPolicyArn = Lens.lens (\CreateFirewall' {firewallPolicyArn} -> firewallPolicyArn) (\s@CreateFirewall' {} a -> s {firewallPolicyArn = a} :: CreateFirewall)

-- | The unique identifier of the VPC where Network Firewall should create
-- the firewall.
--
-- You can\'t change this setting after you create the firewall.
createFirewall_vpcId :: Lens.Lens' CreateFirewall Prelude.Text
createFirewall_vpcId = Lens.lens (\CreateFirewall' {vpcId} -> vpcId) (\s@CreateFirewall' {} a -> s {vpcId = a} :: CreateFirewall)

-- | The public subnets to use for your Network Firewall firewalls. Each
-- subnet must belong to a different Availability Zone in the VPC. Network
-- Firewall creates a firewall endpoint in each subnet.
createFirewall_subnetMappings :: Lens.Lens' CreateFirewall [SubnetMapping]
createFirewall_subnetMappings = Lens.lens (\CreateFirewall' {subnetMappings} -> subnetMappings) (\s@CreateFirewall' {} a -> s {subnetMappings = a} :: CreateFirewall) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFirewall where
  type
    AWSResponse CreateFirewall =
      CreateFirewallResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFirewallResponse'
            Prelude.<$> (x Data..?> "Firewall")
            Prelude.<*> (x Data..?> "FirewallStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFirewall where
  hashWithSalt _salt CreateFirewall' {..} =
    _salt
      `Prelude.hashWithSalt` deleteProtection
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` firewallPolicyChangeProtection
      `Prelude.hashWithSalt` subnetChangeProtection
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetMappings

instance Prelude.NFData CreateFirewall where
  rnf CreateFirewall' {..} =
    Prelude.rnf deleteProtection `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf encryptionConfiguration `Prelude.seq`
          Prelude.rnf firewallPolicyChangeProtection `Prelude.seq`
            Prelude.rnf subnetChangeProtection `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf firewallName `Prelude.seq`
                  Prelude.rnf firewallPolicyArn `Prelude.seq`
                    Prelude.rnf vpcId `Prelude.seq`
                      Prelude.rnf subnetMappings

instance Data.ToHeaders CreateFirewall where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.CreateFirewall" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFirewall where
  toJSON CreateFirewall' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteProtection" Data..=)
              Prelude.<$> deleteProtection,
            ("Description" Data..=) Prelude.<$> description,
            ("EncryptionConfiguration" Data..=)
              Prelude.<$> encryptionConfiguration,
            ("FirewallPolicyChangeProtection" Data..=)
              Prelude.<$> firewallPolicyChangeProtection,
            ("SubnetChangeProtection" Data..=)
              Prelude.<$> subnetChangeProtection,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("FirewallName" Data..= firewallName),
            Prelude.Just
              ("FirewallPolicyArn" Data..= firewallPolicyArn),
            Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just
              ("SubnetMappings" Data..= subnetMappings)
          ]
      )

instance Data.ToPath CreateFirewall where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFirewall where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFirewallResponse' smart constructor.
data CreateFirewallResponse = CreateFirewallResponse'
  { -- | The configuration settings for the firewall. These settings include the
    -- firewall policy and the subnets in your VPC to use for the firewall
    -- endpoints.
    firewall :: Prelude.Maybe Firewall,
    -- | Detailed information about the current status of a Firewall. You can
    -- retrieve this for a firewall by calling DescribeFirewall and providing
    -- the firewall name and ARN.
    firewallStatus :: Prelude.Maybe FirewallStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFirewallResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewall', 'createFirewallResponse_firewall' - The configuration settings for the firewall. These settings include the
-- firewall policy and the subnets in your VPC to use for the firewall
-- endpoints.
--
-- 'firewallStatus', 'createFirewallResponse_firewallStatus' - Detailed information about the current status of a Firewall. You can
-- retrieve this for a firewall by calling DescribeFirewall and providing
-- the firewall name and ARN.
--
-- 'httpStatus', 'createFirewallResponse_httpStatus' - The response's http status code.
newCreateFirewallResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFirewallResponse
newCreateFirewallResponse pHttpStatus_ =
  CreateFirewallResponse'
    { firewall = Prelude.Nothing,
      firewallStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration settings for the firewall. These settings include the
-- firewall policy and the subnets in your VPC to use for the firewall
-- endpoints.
createFirewallResponse_firewall :: Lens.Lens' CreateFirewallResponse (Prelude.Maybe Firewall)
createFirewallResponse_firewall = Lens.lens (\CreateFirewallResponse' {firewall} -> firewall) (\s@CreateFirewallResponse' {} a -> s {firewall = a} :: CreateFirewallResponse)

-- | Detailed information about the current status of a Firewall. You can
-- retrieve this for a firewall by calling DescribeFirewall and providing
-- the firewall name and ARN.
createFirewallResponse_firewallStatus :: Lens.Lens' CreateFirewallResponse (Prelude.Maybe FirewallStatus)
createFirewallResponse_firewallStatus = Lens.lens (\CreateFirewallResponse' {firewallStatus} -> firewallStatus) (\s@CreateFirewallResponse' {} a -> s {firewallStatus = a} :: CreateFirewallResponse)

-- | The response's http status code.
createFirewallResponse_httpStatus :: Lens.Lens' CreateFirewallResponse Prelude.Int
createFirewallResponse_httpStatus = Lens.lens (\CreateFirewallResponse' {httpStatus} -> httpStatus) (\s@CreateFirewallResponse' {} a -> s {httpStatus = a} :: CreateFirewallResponse)

instance Prelude.NFData CreateFirewallResponse where
  rnf CreateFirewallResponse' {..} =
    Prelude.rnf firewall `Prelude.seq`
      Prelude.rnf firewallStatus `Prelude.seq`
        Prelude.rnf httpStatus
