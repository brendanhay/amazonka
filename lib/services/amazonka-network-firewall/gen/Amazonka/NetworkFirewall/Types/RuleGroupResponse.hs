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
-- Module      : Amazonka.NetworkFirewall.Types.RuleGroupResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleGroupResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.EncryptionConfiguration
import Amazonka.NetworkFirewall.Types.ResourceStatus
import Amazonka.NetworkFirewall.Types.RuleGroupType
import Amazonka.NetworkFirewall.Types.SourceMetadata
import Amazonka.NetworkFirewall.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
--
-- /See:/ 'newRuleGroupResponse' smart constructor.
data RuleGroupResponse = RuleGroupResponse'
  { -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    type' :: Prelude.Maybe RuleGroupType,
    -- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
    -- SNS topic that\'s used to record changes to the managed rule group. You
    -- can subscribe to the SNS topic to receive notifications when the managed
    -- rule group is modified, such as for new versions and for version
    -- expiration. For more information, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide.>.
    snsTopic :: Prelude.Maybe Prelude.Text,
    -- | A description of the rule group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last time that the rule group was changed.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A complex type that contains metadata about the rule group that your own
    -- rule group is copied from. You can use the metadata to track the version
    -- updates made to the originating rule group.
    sourceMetadata :: Prelude.Maybe SourceMetadata,
    -- | The number of capacity units currently consumed by the rule group rules.
    consumedCapacity :: Prelude.Maybe Prelude.Int,
    -- | A complex type that contains the Amazon Web Services KMS encryption
    -- configuration settings for your rule group.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The number of firewall policies that use this rule group.
    numberOfAssociations :: Prelude.Maybe Prelude.Int,
    -- | The maximum operating resources that this rule group can use. Rule group
    -- capacity is fixed at creation. When you update a rule group, you are
    -- limited to this capacity. When you reference a rule group from a
    -- firewall policy, Network Firewall reserves this capacity for the rule
    -- group.
    --
    -- You can retrieve the capacity that would be required for a rule group
    -- before you create the rule group by calling CreateRuleGroup with
    -- @DryRun@ set to @TRUE@.
    capacity :: Prelude.Maybe Prelude.Int,
    -- | Detailed information about the current status of a rule group.
    ruleGroupStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the rule group.
    --
    -- If this response is for a create request that had @DryRun@ set to
    -- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
    -- resource.
    ruleGroupArn :: Prelude.Text,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    ruleGroupName :: Prelude.Text,
    -- | The unique identifier for the rule group.
    ruleGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ruleGroupResponse_tags' - The key:value pairs to associate with the resource.
--
-- 'type'', 'ruleGroupResponse_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- 'snsTopic', 'ruleGroupResponse_snsTopic' - The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to record changes to the managed rule group. You
-- can subscribe to the SNS topic to receive notifications when the managed
-- rule group is modified, such as for new versions and for version
-- expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide.>.
--
-- 'description', 'ruleGroupResponse_description' - A description of the rule group.
--
-- 'lastModifiedTime', 'ruleGroupResponse_lastModifiedTime' - The last time that the rule group was changed.
--
-- 'sourceMetadata', 'ruleGroupResponse_sourceMetadata' - A complex type that contains metadata about the rule group that your own
-- rule group is copied from. You can use the metadata to track the version
-- updates made to the originating rule group.
--
-- 'consumedCapacity', 'ruleGroupResponse_consumedCapacity' - The number of capacity units currently consumed by the rule group rules.
--
-- 'encryptionConfiguration', 'ruleGroupResponse_encryptionConfiguration' - A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your rule group.
--
-- 'numberOfAssociations', 'ruleGroupResponse_numberOfAssociations' - The number of firewall policies that use this rule group.
--
-- 'capacity', 'ruleGroupResponse_capacity' - The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
--
-- 'ruleGroupStatus', 'ruleGroupResponse_ruleGroupStatus' - Detailed information about the current status of a rule group.
--
-- 'ruleGroupArn', 'ruleGroupResponse_ruleGroupArn' - The Amazon Resource Name (ARN) of the rule group.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
--
-- 'ruleGroupName', 'ruleGroupResponse_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- 'ruleGroupId', 'ruleGroupResponse_ruleGroupId' - The unique identifier for the rule group.
newRuleGroupResponse ::
  -- | 'ruleGroupArn'
  Prelude.Text ->
  -- | 'ruleGroupName'
  Prelude.Text ->
  -- | 'ruleGroupId'
  Prelude.Text ->
  RuleGroupResponse
newRuleGroupResponse
  pRuleGroupArn_
  pRuleGroupName_
  pRuleGroupId_ =
    RuleGroupResponse'
      { tags = Prelude.Nothing,
        type' = Prelude.Nothing,
        snsTopic = Prelude.Nothing,
        description = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        sourceMetadata = Prelude.Nothing,
        consumedCapacity = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        numberOfAssociations = Prelude.Nothing,
        capacity = Prelude.Nothing,
        ruleGroupStatus = Prelude.Nothing,
        ruleGroupArn = pRuleGroupArn_,
        ruleGroupName = pRuleGroupName_,
        ruleGroupId = pRuleGroupId_
      }

-- | The key:value pairs to associate with the resource.
ruleGroupResponse_tags :: Lens.Lens' RuleGroupResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
ruleGroupResponse_tags = Lens.lens (\RuleGroupResponse' {tags} -> tags) (\s@RuleGroupResponse' {} a -> s {tags = a} :: RuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
ruleGroupResponse_type :: Lens.Lens' RuleGroupResponse (Prelude.Maybe RuleGroupType)
ruleGroupResponse_type = Lens.lens (\RuleGroupResponse' {type'} -> type') (\s@RuleGroupResponse' {} a -> s {type' = a} :: RuleGroupResponse)

-- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to record changes to the managed rule group. You
-- can subscribe to the SNS topic to receive notifications when the managed
-- rule group is modified, such as for new versions and for version
-- expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide.>.
ruleGroupResponse_snsTopic :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Text)
ruleGroupResponse_snsTopic = Lens.lens (\RuleGroupResponse' {snsTopic} -> snsTopic) (\s@RuleGroupResponse' {} a -> s {snsTopic = a} :: RuleGroupResponse)

-- | A description of the rule group.
ruleGroupResponse_description :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Text)
ruleGroupResponse_description = Lens.lens (\RuleGroupResponse' {description} -> description) (\s@RuleGroupResponse' {} a -> s {description = a} :: RuleGroupResponse)

-- | The last time that the rule group was changed.
ruleGroupResponse_lastModifiedTime :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.UTCTime)
ruleGroupResponse_lastModifiedTime = Lens.lens (\RuleGroupResponse' {lastModifiedTime} -> lastModifiedTime) (\s@RuleGroupResponse' {} a -> s {lastModifiedTime = a} :: RuleGroupResponse) Prelude.. Lens.mapping Data._Time

-- | A complex type that contains metadata about the rule group that your own
-- rule group is copied from. You can use the metadata to track the version
-- updates made to the originating rule group.
ruleGroupResponse_sourceMetadata :: Lens.Lens' RuleGroupResponse (Prelude.Maybe SourceMetadata)
ruleGroupResponse_sourceMetadata = Lens.lens (\RuleGroupResponse' {sourceMetadata} -> sourceMetadata) (\s@RuleGroupResponse' {} a -> s {sourceMetadata = a} :: RuleGroupResponse)

-- | The number of capacity units currently consumed by the rule group rules.
ruleGroupResponse_consumedCapacity :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_consumedCapacity = Lens.lens (\RuleGroupResponse' {consumedCapacity} -> consumedCapacity) (\s@RuleGroupResponse' {} a -> s {consumedCapacity = a} :: RuleGroupResponse)

-- | A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your rule group.
ruleGroupResponse_encryptionConfiguration :: Lens.Lens' RuleGroupResponse (Prelude.Maybe EncryptionConfiguration)
ruleGroupResponse_encryptionConfiguration = Lens.lens (\RuleGroupResponse' {encryptionConfiguration} -> encryptionConfiguration) (\s@RuleGroupResponse' {} a -> s {encryptionConfiguration = a} :: RuleGroupResponse)

-- | The number of firewall policies that use this rule group.
ruleGroupResponse_numberOfAssociations :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_numberOfAssociations = Lens.lens (\RuleGroupResponse' {numberOfAssociations} -> numberOfAssociations) (\s@RuleGroupResponse' {} a -> s {numberOfAssociations = a} :: RuleGroupResponse)

-- | The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
ruleGroupResponse_capacity :: Lens.Lens' RuleGroupResponse (Prelude.Maybe Prelude.Int)
ruleGroupResponse_capacity = Lens.lens (\RuleGroupResponse' {capacity} -> capacity) (\s@RuleGroupResponse' {} a -> s {capacity = a} :: RuleGroupResponse)

-- | Detailed information about the current status of a rule group.
ruleGroupResponse_ruleGroupStatus :: Lens.Lens' RuleGroupResponse (Prelude.Maybe ResourceStatus)
ruleGroupResponse_ruleGroupStatus = Lens.lens (\RuleGroupResponse' {ruleGroupStatus} -> ruleGroupStatus) (\s@RuleGroupResponse' {} a -> s {ruleGroupStatus = a} :: RuleGroupResponse)

-- | The Amazon Resource Name (ARN) of the rule group.
--
-- If this response is for a create request that had @DryRun@ set to
-- @TRUE@, then this ARN is a placeholder that isn\'t attached to a valid
-- resource.
ruleGroupResponse_ruleGroupArn :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupArn = Lens.lens (\RuleGroupResponse' {ruleGroupArn} -> ruleGroupArn) (\s@RuleGroupResponse' {} a -> s {ruleGroupArn = a} :: RuleGroupResponse)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
ruleGroupResponse_ruleGroupName :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupName = Lens.lens (\RuleGroupResponse' {ruleGroupName} -> ruleGroupName) (\s@RuleGroupResponse' {} a -> s {ruleGroupName = a} :: RuleGroupResponse)

-- | The unique identifier for the rule group.
ruleGroupResponse_ruleGroupId :: Lens.Lens' RuleGroupResponse Prelude.Text
ruleGroupResponse_ruleGroupId = Lens.lens (\RuleGroupResponse' {ruleGroupId} -> ruleGroupId) (\s@RuleGroupResponse' {} a -> s {ruleGroupId = a} :: RuleGroupResponse)

instance Data.FromJSON RuleGroupResponse where
  parseJSON =
    Data.withObject
      "RuleGroupResponse"
      ( \x ->
          RuleGroupResponse'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "SnsTopic")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "SourceMetadata")
            Prelude.<*> (x Data..:? "ConsumedCapacity")
            Prelude.<*> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "NumberOfAssociations")
            Prelude.<*> (x Data..:? "Capacity")
            Prelude.<*> (x Data..:? "RuleGroupStatus")
            Prelude.<*> (x Data..: "RuleGroupArn")
            Prelude.<*> (x Data..: "RuleGroupName")
            Prelude.<*> (x Data..: "RuleGroupId")
      )

instance Prelude.Hashable RuleGroupResponse where
  hashWithSalt _salt RuleGroupResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` snsTopic
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` sourceMetadata
      `Prelude.hashWithSalt` consumedCapacity
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` numberOfAssociations
      `Prelude.hashWithSalt` capacity
      `Prelude.hashWithSalt` ruleGroupStatus
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` ruleGroupId

instance Prelude.NFData RuleGroupResponse where
  rnf RuleGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf snsTopic
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf sourceMetadata
      `Prelude.seq` Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf numberOfAssociations
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf ruleGroupStatus
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf ruleGroupId
