-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FindingFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FindingFilter
  ( FindingFilter (..),

    -- * Smart constructor
    mkFindingFilter,

    -- * Lenses
    ffAgentIds,
    ffRuleNames,
    ffUserAttributes,
    ffRulesPackageARNs,
    ffAttributes,
    ffSeverities,
    ffCreationTimeRange,
    ffAutoScalingGroups,
  )
where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Inspector.Types.TimestampRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a request parameter in the 'ListFindings' action.
--
-- /See:/ 'mkFindingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { agentIds ::
      Lude.Maybe [Lude.Text],
    ruleNames :: Lude.Maybe [Lude.Text],
    userAttributes :: Lude.Maybe [Attribute],
    rulesPackageARNs :: Lude.Maybe [Lude.Text],
    attributes :: Lude.Maybe [Attribute],
    severities :: Lude.Maybe [Severity],
    creationTimeRange :: Lude.Maybe TimestampRange,
    autoScalingGroups :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FindingFilter' with the minimum fields required to make a request.
--
-- * 'agentIds' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
-- * 'attributes' - For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
-- * 'autoScalingGroups' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
-- * 'creationTimeRange' - The time range during which the finding is generated.
-- * 'ruleNames' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
-- * 'rulesPackageARNs' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
-- * 'severities' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
-- * 'userAttributes' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
mkFindingFilter ::
  FindingFilter
mkFindingFilter =
  FindingFilter'
    { agentIds = Lude.Nothing,
      ruleNames = Lude.Nothing,
      userAttributes = Lude.Nothing,
      rulesPackageARNs = Lude.Nothing,
      attributes = Lude.Nothing,
      severities = Lude.Nothing,
      creationTimeRange = Lude.Nothing,
      autoScalingGroups = Lude.Nothing
    }

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAgentIds :: Lens.Lens' FindingFilter (Lude.Maybe [Lude.Text])
ffAgentIds = Lens.lens (agentIds :: FindingFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {agentIds = a} :: FindingFilter)
{-# DEPRECATED ffAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffRuleNames :: Lens.Lens' FindingFilter (Lude.Maybe [Lude.Text])
ffRuleNames = Lens.lens (ruleNames :: FindingFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {ruleNames = a} :: FindingFilter)
{-# DEPRECATED ffRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffUserAttributes :: Lens.Lens' FindingFilter (Lude.Maybe [Attribute])
ffUserAttributes = Lens.lens (userAttributes :: FindingFilter -> Lude.Maybe [Attribute]) (\s a -> s {userAttributes = a} :: FindingFilter)
{-# DEPRECATED ffUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffRulesPackageARNs :: Lens.Lens' FindingFilter (Lude.Maybe [Lude.Text])
ffRulesPackageARNs = Lens.lens (rulesPackageARNs :: FindingFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: FindingFilter)
{-# DEPRECATED ffRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAttributes :: Lens.Lens' FindingFilter (Lude.Maybe [Attribute])
ffAttributes = Lens.lens (attributes :: FindingFilter -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: FindingFilter)
{-# DEPRECATED ffAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'severities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffSeverities :: Lens.Lens' FindingFilter (Lude.Maybe [Severity])
ffSeverities = Lens.lens (severities :: FindingFilter -> Lude.Maybe [Severity]) (\s a -> s {severities = a} :: FindingFilter)
{-# DEPRECATED ffSeverities "Use generic-lens or generic-optics with 'severities' instead." #-}

-- | The time range during which the finding is generated.
--
-- /Note:/ Consider using 'creationTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffCreationTimeRange :: Lens.Lens' FindingFilter (Lude.Maybe TimestampRange)
ffCreationTimeRange = Lens.lens (creationTimeRange :: FindingFilter -> Lude.Maybe TimestampRange) (\s a -> s {creationTimeRange = a} :: FindingFilter)
{-# DEPRECATED ffCreationTimeRange "Use generic-lens or generic-optics with 'creationTimeRange' instead." #-}

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ffAutoScalingGroups :: Lens.Lens' FindingFilter (Lude.Maybe [Lude.Text])
ffAutoScalingGroups = Lens.lens (autoScalingGroups :: FindingFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroups = a} :: FindingFilter)
{-# DEPRECATED ffAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

instance Lude.ToJSON FindingFilter where
  toJSON FindingFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("agentIds" Lude..=) Lude.<$> agentIds,
            ("ruleNames" Lude..=) Lude.<$> ruleNames,
            ("userAttributes" Lude..=) Lude.<$> userAttributes,
            ("rulesPackageArns" Lude..=) Lude.<$> rulesPackageARNs,
            ("attributes" Lude..=) Lude.<$> attributes,
            ("severities" Lude..=) Lude.<$> severities,
            ("creationTimeRange" Lude..=) Lude.<$> creationTimeRange,
            ("autoScalingGroups" Lude..=) Lude.<$> autoScalingGroups
          ]
      )
