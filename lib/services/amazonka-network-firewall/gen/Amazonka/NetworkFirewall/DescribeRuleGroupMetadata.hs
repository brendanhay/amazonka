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
-- Module      : Amazonka.NetworkFirewall.DescribeRuleGroupMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- High-level information about a rule group, returned by operations like
-- create and describe. You can use the information provided in the
-- metadata to retrieve and manage a rule group. You can retrieve all
-- objects for a rule group by calling DescribeRuleGroup.
module Amazonka.NetworkFirewall.DescribeRuleGroupMetadata
  ( -- * Creating a Request
    DescribeRuleGroupMetadata (..),
    newDescribeRuleGroupMetadata,

    -- * Request Lenses
    describeRuleGroupMetadata_ruleGroupName,
    describeRuleGroupMetadata_type,
    describeRuleGroupMetadata_ruleGroupArn,

    -- * Destructuring the Response
    DescribeRuleGroupMetadataResponse (..),
    newDescribeRuleGroupMetadataResponse,

    -- * Response Lenses
    describeRuleGroupMetadataResponse_type,
    describeRuleGroupMetadataResponse_statefulRuleOptions,
    describeRuleGroupMetadataResponse_description,
    describeRuleGroupMetadataResponse_lastModifiedTime,
    describeRuleGroupMetadataResponse_capacity,
    describeRuleGroupMetadataResponse_httpStatus,
    describeRuleGroupMetadataResponse_ruleGroupArn,
    describeRuleGroupMetadataResponse_ruleGroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRuleGroupMetadata' smart constructor.
data DescribeRuleGroupMetadata = DescribeRuleGroupMetadata'
  { -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    --
    -- This setting is required for requests that do not include the
    -- @RuleGroupARN@.
    type' :: Prelude.Maybe RuleGroupType,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupName', 'describeRuleGroupMetadata_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'type'', 'describeRuleGroupMetadata_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
--
-- 'ruleGroupArn', 'describeRuleGroupMetadata_ruleGroupArn' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDescribeRuleGroupMetadata ::
  DescribeRuleGroupMetadata
newDescribeRuleGroupMetadata =
  DescribeRuleGroupMetadata'
    { ruleGroupName =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      ruleGroupArn = Prelude.Nothing
    }

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroupMetadata_ruleGroupName :: Lens.Lens' DescribeRuleGroupMetadata (Prelude.Maybe Prelude.Text)
describeRuleGroupMetadata_ruleGroupName = Lens.lens (\DescribeRuleGroupMetadata' {ruleGroupName} -> ruleGroupName) (\s@DescribeRuleGroupMetadata' {} a -> s {ruleGroupName = a} :: DescribeRuleGroupMetadata)

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
describeRuleGroupMetadata_type :: Lens.Lens' DescribeRuleGroupMetadata (Prelude.Maybe RuleGroupType)
describeRuleGroupMetadata_type = Lens.lens (\DescribeRuleGroupMetadata' {type'} -> type') (\s@DescribeRuleGroupMetadata' {} a -> s {type' = a} :: DescribeRuleGroupMetadata)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroupMetadata_ruleGroupArn :: Lens.Lens' DescribeRuleGroupMetadata (Prelude.Maybe Prelude.Text)
describeRuleGroupMetadata_ruleGroupArn = Lens.lens (\DescribeRuleGroupMetadata' {ruleGroupArn} -> ruleGroupArn) (\s@DescribeRuleGroupMetadata' {} a -> s {ruleGroupArn = a} :: DescribeRuleGroupMetadata)

instance Core.AWSRequest DescribeRuleGroupMetadata where
  type
    AWSResponse DescribeRuleGroupMetadata =
      DescribeRuleGroupMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleGroupMetadataResponse'
            Prelude.<$> (x Data..?> "Type")
            Prelude.<*> (x Data..?> "StatefulRuleOptions")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Capacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RuleGroupArn")
            Prelude.<*> (x Data..:> "RuleGroupName")
      )

instance Prelude.Hashable DescribeRuleGroupMetadata where
  hashWithSalt _salt DescribeRuleGroupMetadata' {..} =
    _salt `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` ruleGroupArn

instance Prelude.NFData DescribeRuleGroupMetadata where
  rnf DescribeRuleGroupMetadata' {..} =
    Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf ruleGroupArn

instance Data.ToHeaders DescribeRuleGroupMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.DescribeRuleGroupMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRuleGroupMetadata where
  toJSON DescribeRuleGroupMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuleGroupName" Data..=) Prelude.<$> ruleGroupName,
            ("Type" Data..=) Prelude.<$> type',
            ("RuleGroupArn" Data..=) Prelude.<$> ruleGroupArn
          ]
      )

instance Data.ToPath DescribeRuleGroupMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRuleGroupMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRuleGroupMetadataResponse' smart constructor.
data DescribeRuleGroupMetadataResponse = DescribeRuleGroupMetadataResponse'
  { -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    --
    -- This setting is required for requests that do not include the
    -- @RuleGroupARN@.
    type' :: Prelude.Maybe RuleGroupType,
    statefulRuleOptions :: Prelude.Maybe StatefulRuleOptions,
    -- | Returns the metadata objects for the specified rule group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last time that the rule group was changed.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupArn :: Prelude.Text,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleGroupMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'describeRuleGroupMetadataResponse_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
--
-- 'statefulRuleOptions', 'describeRuleGroupMetadataResponse_statefulRuleOptions' - Undocumented member.
--
-- 'description', 'describeRuleGroupMetadataResponse_description' - Returns the metadata objects for the specified rule group.
--
-- 'lastModifiedTime', 'describeRuleGroupMetadataResponse_lastModifiedTime' - The last time that the rule group was changed.
--
-- 'capacity', 'describeRuleGroupMetadataResponse_capacity' - The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
--
-- 'httpStatus', 'describeRuleGroupMetadataResponse_httpStatus' - The response's http status code.
--
-- 'ruleGroupArn', 'describeRuleGroupMetadataResponse_ruleGroupArn' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'ruleGroupName', 'describeRuleGroupMetadataResponse_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newDescribeRuleGroupMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'ruleGroupArn'
  Prelude.Text ->
  -- | 'ruleGroupName'
  Prelude.Text ->
  DescribeRuleGroupMetadataResponse
newDescribeRuleGroupMetadataResponse
  pHttpStatus_
  pRuleGroupArn_
  pRuleGroupName_ =
    DescribeRuleGroupMetadataResponse'
      { type' =
          Prelude.Nothing,
        statefulRuleOptions = Prelude.Nothing,
        description = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        capacity = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        ruleGroupArn = pRuleGroupArn_,
        ruleGroupName = pRuleGroupName_
      }

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
describeRuleGroupMetadataResponse_type :: Lens.Lens' DescribeRuleGroupMetadataResponse (Prelude.Maybe RuleGroupType)
describeRuleGroupMetadataResponse_type = Lens.lens (\DescribeRuleGroupMetadataResponse' {type'} -> type') (\s@DescribeRuleGroupMetadataResponse' {} a -> s {type' = a} :: DescribeRuleGroupMetadataResponse)

-- | Undocumented member.
describeRuleGroupMetadataResponse_statefulRuleOptions :: Lens.Lens' DescribeRuleGroupMetadataResponse (Prelude.Maybe StatefulRuleOptions)
describeRuleGroupMetadataResponse_statefulRuleOptions = Lens.lens (\DescribeRuleGroupMetadataResponse' {statefulRuleOptions} -> statefulRuleOptions) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {statefulRuleOptions = a} :: DescribeRuleGroupMetadataResponse)

-- | Returns the metadata objects for the specified rule group.
describeRuleGroupMetadataResponse_description :: Lens.Lens' DescribeRuleGroupMetadataResponse (Prelude.Maybe Prelude.Text)
describeRuleGroupMetadataResponse_description = Lens.lens (\DescribeRuleGroupMetadataResponse' {description} -> description) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {description = a} :: DescribeRuleGroupMetadataResponse)

-- | The last time that the rule group was changed.
describeRuleGroupMetadataResponse_lastModifiedTime :: Lens.Lens' DescribeRuleGroupMetadataResponse (Prelude.Maybe Prelude.UTCTime)
describeRuleGroupMetadataResponse_lastModifiedTime = Lens.lens (\DescribeRuleGroupMetadataResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {lastModifiedTime = a} :: DescribeRuleGroupMetadataResponse) Prelude.. Lens.mapping Data._Time

-- | The maximum operating resources that this rule group can use. Rule group
-- capacity is fixed at creation. When you update a rule group, you are
-- limited to this capacity. When you reference a rule group from a
-- firewall policy, Network Firewall reserves this capacity for the rule
-- group.
--
-- You can retrieve the capacity that would be required for a rule group
-- before you create the rule group by calling CreateRuleGroup with
-- @DryRun@ set to @TRUE@.
describeRuleGroupMetadataResponse_capacity :: Lens.Lens' DescribeRuleGroupMetadataResponse (Prelude.Maybe Prelude.Int)
describeRuleGroupMetadataResponse_capacity = Lens.lens (\DescribeRuleGroupMetadataResponse' {capacity} -> capacity) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {capacity = a} :: DescribeRuleGroupMetadataResponse)

-- | The response's http status code.
describeRuleGroupMetadataResponse_httpStatus :: Lens.Lens' DescribeRuleGroupMetadataResponse Prelude.Int
describeRuleGroupMetadataResponse_httpStatus = Lens.lens (\DescribeRuleGroupMetadataResponse' {httpStatus} -> httpStatus) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {httpStatus = a} :: DescribeRuleGroupMetadataResponse)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroupMetadataResponse_ruleGroupArn :: Lens.Lens' DescribeRuleGroupMetadataResponse Prelude.Text
describeRuleGroupMetadataResponse_ruleGroupArn = Lens.lens (\DescribeRuleGroupMetadataResponse' {ruleGroupArn} -> ruleGroupArn) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {ruleGroupArn = a} :: DescribeRuleGroupMetadataResponse)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
describeRuleGroupMetadataResponse_ruleGroupName :: Lens.Lens' DescribeRuleGroupMetadataResponse Prelude.Text
describeRuleGroupMetadataResponse_ruleGroupName = Lens.lens (\DescribeRuleGroupMetadataResponse' {ruleGroupName} -> ruleGroupName) (\s@DescribeRuleGroupMetadataResponse' {} a -> s {ruleGroupName = a} :: DescribeRuleGroupMetadataResponse)

instance
  Prelude.NFData
    DescribeRuleGroupMetadataResponse
  where
  rnf DescribeRuleGroupMetadataResponse' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf statefulRuleOptions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleGroupName
