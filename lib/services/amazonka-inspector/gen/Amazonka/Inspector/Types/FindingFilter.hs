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
-- Module      : Amazonka.Inspector.Types.FindingFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.FindingFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.Attribute
import Amazonka.Inspector.Types.Severity
import Amazonka.Inspector.Types.TimestampRange
import qualified Amazonka.Prelude as Prelude

-- | This data type is used as a request parameter in the ListFindings
-- action.
--
-- /See:/ 'newFindingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __autoScalingGroup__ property of the Finding data type.
    autoScalingGroups :: Prelude.Maybe [Prelude.Text],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __severity__ property of the Finding data type.
    severities :: Prelude.Maybe [Severity],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __ruleName__ property of the Finding data type.
    ruleNames :: Prelude.Maybe [Prelude.Text],
    -- | The time range during which the finding is generated.
    creationTimeRange :: Prelude.Maybe TimestampRange,
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must be contained in the list of values of the
    -- __userAttributes__ property of the Finding data type.
    userAttributes :: Prelude.Maybe [Attribute],
    -- | For a record to match a filter, the list of values that are specified
    -- for this data type property must be contained in the list of values of
    -- the __attributes__ property of the Finding data type.
    attributes :: Prelude.Maybe [Attribute],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __rulesPackageArn__ property of the Finding data type.
    rulesPackageArns :: Prelude.Maybe [Prelude.Text],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __agentId__ property of the Finding data type.
    agentIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroups', 'findingFilter_autoScalingGroups' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __autoScalingGroup__ property of the Finding data type.
--
-- 'severities', 'findingFilter_severities' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __severity__ property of the Finding data type.
--
-- 'ruleNames', 'findingFilter_ruleNames' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __ruleName__ property of the Finding data type.
--
-- 'creationTimeRange', 'findingFilter_creationTimeRange' - The time range during which the finding is generated.
--
-- 'userAttributes', 'findingFilter_userAttributes' - For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __userAttributes__ property of the Finding data type.
--
-- 'attributes', 'findingFilter_attributes' - For a record to match a filter, the list of values that are specified
-- for this data type property must be contained in the list of values of
-- the __attributes__ property of the Finding data type.
--
-- 'rulesPackageArns', 'findingFilter_rulesPackageArns' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __rulesPackageArn__ property of the Finding data type.
--
-- 'agentIds', 'findingFilter_agentIds' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __agentId__ property of the Finding data type.
newFindingFilter ::
  FindingFilter
newFindingFilter =
  FindingFilter'
    { autoScalingGroups = Prelude.Nothing,
      severities = Prelude.Nothing,
      ruleNames = Prelude.Nothing,
      creationTimeRange = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      attributes = Prelude.Nothing,
      rulesPackageArns = Prelude.Nothing,
      agentIds = Prelude.Nothing
    }

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __autoScalingGroup__ property of the Finding data type.
findingFilter_autoScalingGroups :: Lens.Lens' FindingFilter (Prelude.Maybe [Prelude.Text])
findingFilter_autoScalingGroups = Lens.lens (\FindingFilter' {autoScalingGroups} -> autoScalingGroups) (\s@FindingFilter' {} a -> s {autoScalingGroups = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __severity__ property of the Finding data type.
findingFilter_severities :: Lens.Lens' FindingFilter (Prelude.Maybe [Severity])
findingFilter_severities = Lens.lens (\FindingFilter' {severities} -> severities) (\s@FindingFilter' {} a -> s {severities = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __ruleName__ property of the Finding data type.
findingFilter_ruleNames :: Lens.Lens' FindingFilter (Prelude.Maybe [Prelude.Text])
findingFilter_ruleNames = Lens.lens (\FindingFilter' {ruleNames} -> ruleNames) (\s@FindingFilter' {} a -> s {ruleNames = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | The time range during which the finding is generated.
findingFilter_creationTimeRange :: Lens.Lens' FindingFilter (Prelude.Maybe TimestampRange)
findingFilter_creationTimeRange = Lens.lens (\FindingFilter' {creationTimeRange} -> creationTimeRange) (\s@FindingFilter' {} a -> s {creationTimeRange = a} :: FindingFilter)

-- | For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __userAttributes__ property of the Finding data type.
findingFilter_userAttributes :: Lens.Lens' FindingFilter (Prelude.Maybe [Attribute])
findingFilter_userAttributes = Lens.lens (\FindingFilter' {userAttributes} -> userAttributes) (\s@FindingFilter' {} a -> s {userAttributes = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, the list of values that are specified
-- for this data type property must be contained in the list of values of
-- the __attributes__ property of the Finding data type.
findingFilter_attributes :: Lens.Lens' FindingFilter (Prelude.Maybe [Attribute])
findingFilter_attributes = Lens.lens (\FindingFilter' {attributes} -> attributes) (\s@FindingFilter' {} a -> s {attributes = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __rulesPackageArn__ property of the Finding data type.
findingFilter_rulesPackageArns :: Lens.Lens' FindingFilter (Prelude.Maybe [Prelude.Text])
findingFilter_rulesPackageArns = Lens.lens (\FindingFilter' {rulesPackageArns} -> rulesPackageArns) (\s@FindingFilter' {} a -> s {rulesPackageArns = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __agentId__ property of the Finding data type.
findingFilter_agentIds :: Lens.Lens' FindingFilter (Prelude.Maybe [Prelude.Text])
findingFilter_agentIds = Lens.lens (\FindingFilter' {agentIds} -> agentIds) (\s@FindingFilter' {} a -> s {agentIds = a} :: FindingFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable FindingFilter where
  hashWithSalt _salt FindingFilter' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroups
      `Prelude.hashWithSalt` severities
      `Prelude.hashWithSalt` ruleNames
      `Prelude.hashWithSalt` creationTimeRange
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` rulesPackageArns
      `Prelude.hashWithSalt` agentIds

instance Prelude.NFData FindingFilter where
  rnf FindingFilter' {..} =
    Prelude.rnf autoScalingGroups
      `Prelude.seq` Prelude.rnf severities
      `Prelude.seq` Prelude.rnf ruleNames
      `Prelude.seq` Prelude.rnf creationTimeRange
      `Prelude.seq` Prelude.rnf userAttributes
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf rulesPackageArns
      `Prelude.seq` Prelude.rnf agentIds

instance Data.ToJSON FindingFilter where
  toJSON FindingFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoScalingGroups" Data..=)
              Prelude.<$> autoScalingGroups,
            ("severities" Data..=) Prelude.<$> severities,
            ("ruleNames" Data..=) Prelude.<$> ruleNames,
            ("creationTimeRange" Data..=)
              Prelude.<$> creationTimeRange,
            ("userAttributes" Data..=)
              Prelude.<$> userAttributes,
            ("attributes" Data..=) Prelude.<$> attributes,
            ("rulesPackageArns" Data..=)
              Prelude.<$> rulesPackageArns,
            ("agentIds" Data..=) Prelude.<$> agentIds
          ]
      )
