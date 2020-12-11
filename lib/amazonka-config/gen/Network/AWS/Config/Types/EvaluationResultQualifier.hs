-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultQualifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultQualifier
  ( EvaluationResultQualifier (..),

    -- * Smart constructor
    mkEvaluationResultQualifier,

    -- * Lenses
    erqResourceId,
    erqResourceType,
    erqConfigRuleName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.
--
-- /See:/ 'mkEvaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    configRuleName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResultQualifier' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule that was used in the evaluation.
-- * 'resourceId' - The ID of the evaluated AWS resource.
-- * 'resourceType' - The type of AWS resource that was evaluated.
mkEvaluationResultQualifier ::
  EvaluationResultQualifier
mkEvaluationResultQualifier =
  EvaluationResultQualifier'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      configRuleName = Lude.Nothing
    }

-- | The ID of the evaluated AWS resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqResourceId :: Lens.Lens' EvaluationResultQualifier (Lude.Maybe Lude.Text)
erqResourceId = Lens.lens (resourceId :: EvaluationResultQualifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: EvaluationResultQualifier)
{-# DEPRECATED erqResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqResourceType :: Lens.Lens' EvaluationResultQualifier (Lude.Maybe Lude.Text)
erqResourceType = Lens.lens (resourceType :: EvaluationResultQualifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: EvaluationResultQualifier)
{-# DEPRECATED erqResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the AWS Config rule that was used in the evaluation.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqConfigRuleName :: Lens.Lens' EvaluationResultQualifier (Lude.Maybe Lude.Text)
erqConfigRuleName = Lens.lens (configRuleName :: EvaluationResultQualifier -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: EvaluationResultQualifier)
{-# DEPRECATED erqConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Lude.FromJSON EvaluationResultQualifier where
  parseJSON =
    Lude.withObject
      "EvaluationResultQualifier"
      ( \x ->
          EvaluationResultQualifier'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "ConfigRuleName")
      )
