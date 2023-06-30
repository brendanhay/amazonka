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
-- Module      : Amazonka.Inspector2.Types.Ec2InstanceAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2InstanceAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Ec2InstanceSortBy
import Amazonka.Inspector2.Types.MapFilter
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on Amazon EC2 instances.
--
-- /See:/ 'newEc2InstanceAggregation' smart constructor.
data Ec2InstanceAggregation = Ec2InstanceAggregation'
  { -- | The AMI IDs associated with the Amazon EC2 instances to aggregate
    -- findings for.
    amis :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The Amazon EC2 instance IDs to aggregate findings for.
    instanceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The Amazon EC2 instance tags to aggregate findings for.
    instanceTags :: Prelude.Maybe (Prelude.NonEmpty MapFilter),
    -- | The operating system types to aggregate findings for. Valid values must
    -- be uppercase and underscore separated, examples are @ORACLE_LINUX_7@ and
    -- @ALPINE_LINUX_3_8@.
    operatingSystems :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe Ec2InstanceSortBy,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2InstanceAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amis', 'ec2InstanceAggregation_amis' - The AMI IDs associated with the Amazon EC2 instances to aggregate
-- findings for.
--
-- 'instanceIds', 'ec2InstanceAggregation_instanceIds' - The Amazon EC2 instance IDs to aggregate findings for.
--
-- 'instanceTags', 'ec2InstanceAggregation_instanceTags' - The Amazon EC2 instance tags to aggregate findings for.
--
-- 'operatingSystems', 'ec2InstanceAggregation_operatingSystems' - The operating system types to aggregate findings for. Valid values must
-- be uppercase and underscore separated, examples are @ORACLE_LINUX_7@ and
-- @ALPINE_LINUX_3_8@.
--
-- 'sortBy', 'ec2InstanceAggregation_sortBy' - The value to sort results by.
--
-- 'sortOrder', 'ec2InstanceAggregation_sortOrder' - The order to sort results by.
newEc2InstanceAggregation ::
  Ec2InstanceAggregation
newEc2InstanceAggregation =
  Ec2InstanceAggregation'
    { amis = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      instanceTags = Prelude.Nothing,
      operatingSystems = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The AMI IDs associated with the Amazon EC2 instances to aggregate
-- findings for.
ec2InstanceAggregation_amis :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
ec2InstanceAggregation_amis = Lens.lens (\Ec2InstanceAggregation' {amis} -> amis) (\s@Ec2InstanceAggregation' {} a -> s {amis = a} :: Ec2InstanceAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 instance IDs to aggregate findings for.
ec2InstanceAggregation_instanceIds :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
ec2InstanceAggregation_instanceIds = Lens.lens (\Ec2InstanceAggregation' {instanceIds} -> instanceIds) (\s@Ec2InstanceAggregation' {} a -> s {instanceIds = a} :: Ec2InstanceAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 instance tags to aggregate findings for.
ec2InstanceAggregation_instanceTags :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe (Prelude.NonEmpty MapFilter))
ec2InstanceAggregation_instanceTags = Lens.lens (\Ec2InstanceAggregation' {instanceTags} -> instanceTags) (\s@Ec2InstanceAggregation' {} a -> s {instanceTags = a} :: Ec2InstanceAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The operating system types to aggregate findings for. Valid values must
-- be uppercase and underscore separated, examples are @ORACLE_LINUX_7@ and
-- @ALPINE_LINUX_3_8@.
ec2InstanceAggregation_operatingSystems :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
ec2InstanceAggregation_operatingSystems = Lens.lens (\Ec2InstanceAggregation' {operatingSystems} -> operatingSystems) (\s@Ec2InstanceAggregation' {} a -> s {operatingSystems = a} :: Ec2InstanceAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort results by.
ec2InstanceAggregation_sortBy :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe Ec2InstanceSortBy)
ec2InstanceAggregation_sortBy = Lens.lens (\Ec2InstanceAggregation' {sortBy} -> sortBy) (\s@Ec2InstanceAggregation' {} a -> s {sortBy = a} :: Ec2InstanceAggregation)

-- | The order to sort results by.
ec2InstanceAggregation_sortOrder :: Lens.Lens' Ec2InstanceAggregation (Prelude.Maybe SortOrder)
ec2InstanceAggregation_sortOrder = Lens.lens (\Ec2InstanceAggregation' {sortOrder} -> sortOrder) (\s@Ec2InstanceAggregation' {} a -> s {sortOrder = a} :: Ec2InstanceAggregation)

instance Prelude.Hashable Ec2InstanceAggregation where
  hashWithSalt _salt Ec2InstanceAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` amis
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` instanceTags
      `Prelude.hashWithSalt` operatingSystems
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData Ec2InstanceAggregation where
  rnf Ec2InstanceAggregation' {..} =
    Prelude.rnf amis
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf instanceTags
      `Prelude.seq` Prelude.rnf operatingSystems
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON Ec2InstanceAggregation where
  toJSON Ec2InstanceAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amis" Data..=) Prelude.<$> amis,
            ("instanceIds" Data..=) Prelude.<$> instanceIds,
            ("instanceTags" Data..=) Prelude.<$> instanceTags,
            ("operatingSystems" Data..=)
              Prelude.<$> operatingSystems,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
