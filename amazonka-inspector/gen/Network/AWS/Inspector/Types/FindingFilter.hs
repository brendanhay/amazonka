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
-- Module      : Network.AWS.Inspector.Types.FindingFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FindingFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Inspector.Types.TimestampRange
import qualified Network.AWS.Lens as Lens

-- | This data type is used as a request parameter in the ListFindings
-- action.
--
-- /See:/ 'newFindingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __agentId__ property of the Finding data type.
    agentIds :: Core.Maybe [Core.Text],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __rulesPackageArn__ property of the Finding data type.
    rulesPackageArns :: Core.Maybe [Core.Text],
    -- | The time range during which the finding is generated.
    creationTimeRange :: Core.Maybe TimestampRange,
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __severity__ property of the Finding data type.
    severities :: Core.Maybe [Severity],
    -- | For a record to match a filter, the list of values that are specified
    -- for this data type property must be contained in the list of values of
    -- the __attributes__ property of the Finding data type.
    attributes :: Core.Maybe [Attribute],
    -- | For a record to match a filter, the value that is specified for this
    -- data type property must be contained in the list of values of the
    -- __userAttributes__ property of the Finding data type.
    userAttributes :: Core.Maybe [Attribute],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __autoScalingGroup__ property of the Finding data type.
    autoScalingGroups :: Core.Maybe [Core.Text],
    -- | For a record to match a filter, one of the values that is specified for
    -- this data type property must be the exact match of the value of the
    -- __ruleName__ property of the Finding data type.
    ruleNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FindingFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentIds', 'findingFilter_agentIds' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __agentId__ property of the Finding data type.
--
-- 'rulesPackageArns', 'findingFilter_rulesPackageArns' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __rulesPackageArn__ property of the Finding data type.
--
-- 'creationTimeRange', 'findingFilter_creationTimeRange' - The time range during which the finding is generated.
--
-- 'severities', 'findingFilter_severities' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __severity__ property of the Finding data type.
--
-- 'attributes', 'findingFilter_attributes' - For a record to match a filter, the list of values that are specified
-- for this data type property must be contained in the list of values of
-- the __attributes__ property of the Finding data type.
--
-- 'userAttributes', 'findingFilter_userAttributes' - For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __userAttributes__ property of the Finding data type.
--
-- 'autoScalingGroups', 'findingFilter_autoScalingGroups' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __autoScalingGroup__ property of the Finding data type.
--
-- 'ruleNames', 'findingFilter_ruleNames' - For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __ruleName__ property of the Finding data type.
newFindingFilter ::
  FindingFilter
newFindingFilter =
  FindingFilter'
    { agentIds = Core.Nothing,
      rulesPackageArns = Core.Nothing,
      creationTimeRange = Core.Nothing,
      severities = Core.Nothing,
      attributes = Core.Nothing,
      userAttributes = Core.Nothing,
      autoScalingGroups = Core.Nothing,
      ruleNames = Core.Nothing
    }

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __agentId__ property of the Finding data type.
findingFilter_agentIds :: Lens.Lens' FindingFilter (Core.Maybe [Core.Text])
findingFilter_agentIds = Lens.lens (\FindingFilter' {agentIds} -> agentIds) (\s@FindingFilter' {} a -> s {agentIds = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __rulesPackageArn__ property of the Finding data type.
findingFilter_rulesPackageArns :: Lens.Lens' FindingFilter (Core.Maybe [Core.Text])
findingFilter_rulesPackageArns = Lens.lens (\FindingFilter' {rulesPackageArns} -> rulesPackageArns) (\s@FindingFilter' {} a -> s {rulesPackageArns = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | The time range during which the finding is generated.
findingFilter_creationTimeRange :: Lens.Lens' FindingFilter (Core.Maybe TimestampRange)
findingFilter_creationTimeRange = Lens.lens (\FindingFilter' {creationTimeRange} -> creationTimeRange) (\s@FindingFilter' {} a -> s {creationTimeRange = a} :: FindingFilter)

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __severity__ property of the Finding data type.
findingFilter_severities :: Lens.Lens' FindingFilter (Core.Maybe [Severity])
findingFilter_severities = Lens.lens (\FindingFilter' {severities} -> severities) (\s@FindingFilter' {} a -> s {severities = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, the list of values that are specified
-- for this data type property must be contained in the list of values of
-- the __attributes__ property of the Finding data type.
findingFilter_attributes :: Lens.Lens' FindingFilter (Core.Maybe [Attribute])
findingFilter_attributes = Lens.lens (\FindingFilter' {attributes} -> attributes) (\s@FindingFilter' {} a -> s {attributes = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, the value that is specified for this
-- data type property must be contained in the list of values of the
-- __userAttributes__ property of the Finding data type.
findingFilter_userAttributes :: Lens.Lens' FindingFilter (Core.Maybe [Attribute])
findingFilter_userAttributes = Lens.lens (\FindingFilter' {userAttributes} -> userAttributes) (\s@FindingFilter' {} a -> s {userAttributes = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __autoScalingGroup__ property of the Finding data type.
findingFilter_autoScalingGroups :: Lens.Lens' FindingFilter (Core.Maybe [Core.Text])
findingFilter_autoScalingGroups = Lens.lens (\FindingFilter' {autoScalingGroups} -> autoScalingGroups) (\s@FindingFilter' {} a -> s {autoScalingGroups = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

-- | For a record to match a filter, one of the values that is specified for
-- this data type property must be the exact match of the value of the
-- __ruleName__ property of the Finding data type.
findingFilter_ruleNames :: Lens.Lens' FindingFilter (Core.Maybe [Core.Text])
findingFilter_ruleNames = Lens.lens (\FindingFilter' {ruleNames} -> ruleNames) (\s@FindingFilter' {} a -> s {ruleNames = a} :: FindingFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable FindingFilter

instance Core.NFData FindingFilter

instance Core.ToJSON FindingFilter where
  toJSON FindingFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("agentIds" Core..=) Core.<$> agentIds,
            ("rulesPackageArns" Core..=)
              Core.<$> rulesPackageArns,
            ("creationTimeRange" Core..=)
              Core.<$> creationTimeRange,
            ("severities" Core..=) Core.<$> severities,
            ("attributes" Core..=) Core.<$> attributes,
            ("userAttributes" Core..=) Core.<$> userAttributes,
            ("autoScalingGroups" Core..=)
              Core.<$> autoScalingGroups,
            ("ruleNames" Core..=) Core.<$> ruleNames
          ]
      )
