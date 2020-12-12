{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Scope
  ( Scope (..),

    -- * Smart constructor
    mkScope,

    -- * Lenses
    sComplianceResourceTypes,
    sComplianceResourceId,
    sTagValue,
    sTagKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.
--
-- /See:/ 'mkScope' smart constructor.
data Scope = Scope'
  { complianceResourceTypes ::
      Lude.Maybe [Lude.Text],
    complianceResourceId :: Lude.Maybe Lude.Text,
    tagValue :: Lude.Maybe Lude.Text,
    tagKey :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scope' with the minimum fields required to make a request.
--
-- * 'complianceResourceId' - The ID of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
-- * 'complianceResourceTypes' - The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
-- * 'tagKey' - The tag key that is applied to only those AWS resources that you want to trigger an evaluation for the rule.
-- * 'tagValue' - The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
mkScope ::
  Scope
mkScope =
  Scope'
    { complianceResourceTypes = Lude.Nothing,
      complianceResourceId = Lude.Nothing,
      tagValue = Lude.Nothing,
      tagKey = Lude.Nothing
    }

-- | The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
--
-- /Note:/ Consider using 'complianceResourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sComplianceResourceTypes :: Lens.Lens' Scope (Lude.Maybe [Lude.Text])
sComplianceResourceTypes = Lens.lens (complianceResourceTypes :: Scope -> Lude.Maybe [Lude.Text]) (\s a -> s {complianceResourceTypes = a} :: Scope)
{-# DEPRECATED sComplianceResourceTypes "Use generic-lens or generic-optics with 'complianceResourceTypes' instead." #-}

-- | The ID of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
--
-- /Note:/ Consider using 'complianceResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sComplianceResourceId :: Lens.Lens' Scope (Lude.Maybe Lude.Text)
sComplianceResourceId = Lens.lens (complianceResourceId :: Scope -> Lude.Maybe Lude.Text) (\s a -> s {complianceResourceId = a} :: Scope)
{-# DEPRECATED sComplianceResourceId "Use generic-lens or generic-optics with 'complianceResourceId' instead." #-}

-- | The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
--
-- /Note:/ Consider using 'tagValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTagValue :: Lens.Lens' Scope (Lude.Maybe Lude.Text)
sTagValue = Lens.lens (tagValue :: Scope -> Lude.Maybe Lude.Text) (\s a -> s {tagValue = a} :: Scope)
{-# DEPRECATED sTagValue "Use generic-lens or generic-optics with 'tagValue' instead." #-}

-- | The tag key that is applied to only those AWS resources that you want to trigger an evaluation for the rule.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTagKey :: Lens.Lens' Scope (Lude.Maybe Lude.Text)
sTagKey = Lens.lens (tagKey :: Scope -> Lude.Maybe Lude.Text) (\s a -> s {tagKey = a} :: Scope)
{-# DEPRECATED sTagKey "Use generic-lens or generic-optics with 'tagKey' instead." #-}

instance Lude.FromJSON Scope where
  parseJSON =
    Lude.withObject
      "Scope"
      ( \x ->
          Scope'
            Lude.<$> (x Lude..:? "ComplianceResourceTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ComplianceResourceId")
            Lude.<*> (x Lude..:? "TagValue")
            Lude.<*> (x Lude..:? "TagKey")
      )

instance Lude.ToJSON Scope where
  toJSON Scope' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComplianceResourceTypes" Lude..=)
              Lude.<$> complianceResourceTypes,
            ("ComplianceResourceId" Lude..=) Lude.<$> complianceResourceId,
            ("TagValue" Lude..=) Lude.<$> tagValue,
            ("TagKey" Lude..=) Lude.<$> tagKey
          ]
      )
