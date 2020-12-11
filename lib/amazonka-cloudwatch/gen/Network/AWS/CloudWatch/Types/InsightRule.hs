-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRule
  ( InsightRule (..),

    -- * Smart constructor
    mkInsightRule,

    -- * Lenses
    irName,
    irState,
    irSchema,
    irDefinition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure contains the definition for a Contributor Insights rule.
--
-- /See:/ 'mkInsightRule' smart constructor.
data InsightRule = InsightRule'
  { name :: Lude.Text,
    state :: Lude.Text,
    schema :: Lude.Text,
    definition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightRule' with the minimum fields required to make a request.
--
-- * 'definition' - The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
-- * 'name' - The name of the rule.
-- * 'schema' - For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@
-- * 'state' - Indicates whether the rule is enabled or disabled.
mkInsightRule ::
  -- | 'name'
  Lude.Text ->
  -- | 'state'
  Lude.Text ->
  -- | 'schema'
  Lude.Text ->
  -- | 'definition'
  Lude.Text ->
  InsightRule
mkInsightRule pName_ pState_ pSchema_ pDefinition_ =
  InsightRule'
    { name = pName_,
      state = pState_,
      schema = pSchema_,
      definition = pDefinition_
    }

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irName :: Lens.Lens' InsightRule Lude.Text
irName = Lens.lens (name :: InsightRule -> Lude.Text) (\s a -> s {name = a} :: InsightRule)
{-# DEPRECATED irName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates whether the rule is enabled or disabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irState :: Lens.Lens' InsightRule Lude.Text
irState = Lens.lens (state :: InsightRule -> Lude.Text) (\s a -> s {state = a} :: InsightRule)
{-# DEPRECATED irState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irSchema :: Lens.Lens' InsightRule Lude.Text
irSchema = Lens.lens (schema :: InsightRule -> Lude.Text) (\s a -> s {schema = a} :: InsightRule)
{-# DEPRECATED irSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDefinition :: Lens.Lens' InsightRule Lude.Text
irDefinition = Lens.lens (definition :: InsightRule -> Lude.Text) (\s a -> s {definition = a} :: InsightRule)
{-# DEPRECATED irDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

instance Lude.FromXML InsightRule where
  parseXML x =
    InsightRule'
      Lude.<$> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "State")
      Lude.<*> (x Lude..@ "Schema")
      Lude.<*> (x Lude..@ "Definition")
