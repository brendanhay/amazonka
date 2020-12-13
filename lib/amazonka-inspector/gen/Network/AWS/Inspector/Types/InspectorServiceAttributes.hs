{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorServiceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorServiceAttributes
  ( InspectorServiceAttributes (..),

    -- * Smart constructor
    mkInspectorServiceAttributes,

    -- * Lenses
    isaSchemaVersion,
    isaRulesPackageARN,
    isaAssessmentRunARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used in the 'Finding' data type.
--
-- /See:/ 'mkInspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { -- | The schema version of this data type.
    schemaVersion :: Lude.Natural,
    -- | The ARN of the rules package that is used to generate the finding.
    rulesPackageARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the assessment run during which the finding is generated.
    assessmentRunARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InspectorServiceAttributes' with the minimum fields required to make a request.
--
-- * 'schemaVersion' - The schema version of this data type.
-- * 'rulesPackageARN' - The ARN of the rules package that is used to generate the finding.
-- * 'assessmentRunARN' - The ARN of the assessment run during which the finding is generated.
mkInspectorServiceAttributes ::
  -- | 'schemaVersion'
  Lude.Natural ->
  InspectorServiceAttributes
mkInspectorServiceAttributes pSchemaVersion_ =
  InspectorServiceAttributes'
    { schemaVersion = pSchemaVersion_,
      rulesPackageARN = Lude.Nothing,
      assessmentRunARN = Lude.Nothing
    }

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaSchemaVersion :: Lens.Lens' InspectorServiceAttributes Lude.Natural
isaSchemaVersion = Lens.lens (schemaVersion :: InspectorServiceAttributes -> Lude.Natural) (\s a -> s {schemaVersion = a} :: InspectorServiceAttributes)
{-# DEPRECATED isaSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The ARN of the rules package that is used to generate the finding.
--
-- /Note:/ Consider using 'rulesPackageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaRulesPackageARN :: Lens.Lens' InspectorServiceAttributes (Lude.Maybe Lude.Text)
isaRulesPackageARN = Lens.lens (rulesPackageARN :: InspectorServiceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {rulesPackageARN = a} :: InspectorServiceAttributes)
{-# DEPRECATED isaRulesPackageARN "Use generic-lens or generic-optics with 'rulesPackageARN' instead." #-}

-- | The ARN of the assessment run during which the finding is generated.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaAssessmentRunARN :: Lens.Lens' InspectorServiceAttributes (Lude.Maybe Lude.Text)
isaAssessmentRunARN = Lens.lens (assessmentRunARN :: InspectorServiceAttributes -> Lude.Maybe Lude.Text) (\s a -> s {assessmentRunARN = a} :: InspectorServiceAttributes)
{-# DEPRECATED isaAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

instance Lude.FromJSON InspectorServiceAttributes where
  parseJSON =
    Lude.withObject
      "InspectorServiceAttributes"
      ( \x ->
          InspectorServiceAttributes'
            Lude.<$> (x Lude..: "schemaVersion")
            Lude.<*> (x Lude..:? "rulesPackageArn")
            Lude.<*> (x Lude..:? "assessmentRunArn")
      )
