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
-- Module      : Amazonka.NetworkFirewall.Types.FirewallPolicyResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.FirewallPolicyResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.EncryptionConfiguration
import Amazonka.NetworkFirewall.Types.ResourceStatus
import Amazonka.NetworkFirewall.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The high-level properties of a firewall policy. This, along with the
-- FirewallPolicy, define the policy. You can retrieve all objects for a
-- firewall policy by calling DescribeFirewallPolicy.
--
-- /See:/ 'newFirewallPolicyResponse' smart constructor.
data FirewallPolicyResponse = FirewallPolicyResponse'
  { -- | The number of capacity units currently consumed by the policy\'s
    -- stateful rules.
    consumedStatefulRuleCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of capacity units currently consumed by the policy\'s
    -- stateless rules.
    consumedStatelessRuleCapacity :: Prelude.Maybe Prelude.Int,
    -- | A description of the firewall policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains the Amazon Web Services KMS encryption
    -- configuration settings for your firewall policy.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The current status of the firewall policy. You can retrieve this for a
    -- firewall policy by calling DescribeFirewallPolicy and providing the
    -- firewall policy\'s name or ARN.
    firewallPolicyStatus :: Prelude.Maybe ResourceStatus,
    -- | The last time that the firewall policy was changed.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The number of firewalls that are associated with this firewall policy.
    numberOfAssociations :: Prelude.Maybe Prelude.Int,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    firewallPolicyName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- If this response is for a create request that had @DryRun@ set to
    -- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
    -- resource.
    firewallPolicyArn :: Prelude.Text,
    -- | The unique identifier for the firewall policy.
    firewallPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedStatefulRuleCapacity', 'firewallPolicyResponse_consumedStatefulRuleCapacity' - The number of capacity units currently consumed by the policy\'s
-- stateful rules.
--
-- 'consumedStatelessRuleCapacity', 'firewallPolicyResponse_consumedStatelessRuleCapacity' - The number of capacity units currently consumed by the policy\'s
-- stateless rules.
--
-- 'description', 'firewallPolicyResponse_description' - A description of the firewall policy.
--
-- 'encryptionConfiguration', 'firewallPolicyResponse_encryptionConfiguration' - A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your firewall policy.
--
-- 'firewallPolicyStatus', 'firewallPolicyResponse_firewallPolicyStatus' - The current status of the firewall policy. You can retrieve this for a
-- firewall policy by calling DescribeFirewallPolicy and providing the
-- firewall policy\'s name or ARN.
--
-- 'lastModifiedTime', 'firewallPolicyResponse_lastModifiedTime' - The last time that the firewall policy was changed.
--
-- 'numberOfAssociations', 'firewallPolicyResponse_numberOfAssociations' - The number of firewalls that are associated with this firewall policy.
--
-- 'tags', 'firewallPolicyResponse_tags' - The key:value pairs to associate with the resource.
--
-- 'firewallPolicyName', 'firewallPolicyResponse_firewallPolicyName' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
--
-- 'firewallPolicyArn', 'firewallPolicyResponse_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
--
-- 'firewallPolicyId', 'firewallPolicyResponse_firewallPolicyId' - The unique identifier for the firewall policy.
newFirewallPolicyResponse ::
  -- | 'firewallPolicyName'
  Prelude.Text ->
  -- | 'firewallPolicyArn'
  Prelude.Text ->
  -- | 'firewallPolicyId'
  Prelude.Text ->
  FirewallPolicyResponse
newFirewallPolicyResponse
  pFirewallPolicyName_
  pFirewallPolicyArn_
  pFirewallPolicyId_ =
    FirewallPolicyResponse'
      { consumedStatefulRuleCapacity =
          Prelude.Nothing,
        consumedStatelessRuleCapacity = Prelude.Nothing,
        description = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        firewallPolicyStatus = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        numberOfAssociations = Prelude.Nothing,
        tags = Prelude.Nothing,
        firewallPolicyName = pFirewallPolicyName_,
        firewallPolicyArn = pFirewallPolicyArn_,
        firewallPolicyId = pFirewallPolicyId_
      }

-- | The number of capacity units currently consumed by the policy\'s
-- stateful rules.
firewallPolicyResponse_consumedStatefulRuleCapacity :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_consumedStatefulRuleCapacity = Lens.lens (\FirewallPolicyResponse' {consumedStatefulRuleCapacity} -> consumedStatefulRuleCapacity) (\s@FirewallPolicyResponse' {} a -> s {consumedStatefulRuleCapacity = a} :: FirewallPolicyResponse)

-- | The number of capacity units currently consumed by the policy\'s
-- stateless rules.
firewallPolicyResponse_consumedStatelessRuleCapacity :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_consumedStatelessRuleCapacity = Lens.lens (\FirewallPolicyResponse' {consumedStatelessRuleCapacity} -> consumedStatelessRuleCapacity) (\s@FirewallPolicyResponse' {} a -> s {consumedStatelessRuleCapacity = a} :: FirewallPolicyResponse)

-- | A description of the firewall policy.
firewallPolicyResponse_description :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Text)
firewallPolicyResponse_description = Lens.lens (\FirewallPolicyResponse' {description} -> description) (\s@FirewallPolicyResponse' {} a -> s {description = a} :: FirewallPolicyResponse)

-- | A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your firewall policy.
firewallPolicyResponse_encryptionConfiguration :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe EncryptionConfiguration)
firewallPolicyResponse_encryptionConfiguration = Lens.lens (\FirewallPolicyResponse' {encryptionConfiguration} -> encryptionConfiguration) (\s@FirewallPolicyResponse' {} a -> s {encryptionConfiguration = a} :: FirewallPolicyResponse)

-- | The current status of the firewall policy. You can retrieve this for a
-- firewall policy by calling DescribeFirewallPolicy and providing the
-- firewall policy\'s name or ARN.
firewallPolicyResponse_firewallPolicyStatus :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe ResourceStatus)
firewallPolicyResponse_firewallPolicyStatus = Lens.lens (\FirewallPolicyResponse' {firewallPolicyStatus} -> firewallPolicyStatus) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyStatus = a} :: FirewallPolicyResponse)

-- | The last time that the firewall policy was changed.
firewallPolicyResponse_lastModifiedTime :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.UTCTime)
firewallPolicyResponse_lastModifiedTime = Lens.lens (\FirewallPolicyResponse' {lastModifiedTime} -> lastModifiedTime) (\s@FirewallPolicyResponse' {} a -> s {lastModifiedTime = a} :: FirewallPolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The number of firewalls that are associated with this firewall policy.
firewallPolicyResponse_numberOfAssociations :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe Prelude.Int)
firewallPolicyResponse_numberOfAssociations = Lens.lens (\FirewallPolicyResponse' {numberOfAssociations} -> numberOfAssociations) (\s@FirewallPolicyResponse' {} a -> s {numberOfAssociations = a} :: FirewallPolicyResponse)

-- | The key:value pairs to associate with the resource.
firewallPolicyResponse_tags :: Lens.Lens' FirewallPolicyResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
firewallPolicyResponse_tags = Lens.lens (\FirewallPolicyResponse' {tags} -> tags) (\s@FirewallPolicyResponse' {} a -> s {tags = a} :: FirewallPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
firewallPolicyResponse_firewallPolicyName :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyName = Lens.lens (\FirewallPolicyResponse' {firewallPolicyName} -> firewallPolicyName) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyName = a} :: FirewallPolicyResponse)

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
firewallPolicyResponse_firewallPolicyArn :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyArn = Lens.lens (\FirewallPolicyResponse' {firewallPolicyArn} -> firewallPolicyArn) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyArn = a} :: FirewallPolicyResponse)

-- | The unique identifier for the firewall policy.
firewallPolicyResponse_firewallPolicyId :: Lens.Lens' FirewallPolicyResponse Prelude.Text
firewallPolicyResponse_firewallPolicyId = Lens.lens (\FirewallPolicyResponse' {firewallPolicyId} -> firewallPolicyId) (\s@FirewallPolicyResponse' {} a -> s {firewallPolicyId = a} :: FirewallPolicyResponse)

instance Data.FromJSON FirewallPolicyResponse where
  parseJSON =
    Data.withObject
      "FirewallPolicyResponse"
      ( \x ->
          FirewallPolicyResponse'
            Prelude.<$> (x Data..:? "ConsumedStatefulRuleCapacity")
            Prelude.<*> (x Data..:? "ConsumedStatelessRuleCapacity")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "FirewallPolicyStatus")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "NumberOfAssociations")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..: "FirewallPolicyName")
            Prelude.<*> (x Data..: "FirewallPolicyArn")
            Prelude.<*> (x Data..: "FirewallPolicyId")
      )

instance Prelude.Hashable FirewallPolicyResponse where
  hashWithSalt _salt FirewallPolicyResponse' {..} =
    _salt
      `Prelude.hashWithSalt` consumedStatefulRuleCapacity
      `Prelude.hashWithSalt` consumedStatelessRuleCapacity
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` firewallPolicyStatus
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` numberOfAssociations
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` firewallPolicyName
      `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` firewallPolicyId

instance Prelude.NFData FirewallPolicyResponse where
  rnf FirewallPolicyResponse' {..} =
    Prelude.rnf consumedStatefulRuleCapacity `Prelude.seq`
      Prelude.rnf consumedStatelessRuleCapacity `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf encryptionConfiguration `Prelude.seq`
            Prelude.rnf firewallPolicyStatus `Prelude.seq`
              Prelude.rnf lastModifiedTime `Prelude.seq`
                Prelude.rnf numberOfAssociations `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf firewallPolicyName `Prelude.seq`
                      Prelude.rnf firewallPolicyArn `Prelude.seq`
                        Prelude.rnf firewallPolicyId
