-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RegistrationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RegistrationConfig
  ( RegistrationConfig (..),

    -- * Smart constructor
    mkRegistrationConfig,

    -- * Lenses
    rcTemplateBody,
    rcRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The registration configuration.
--
-- /See:/ 'mkRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { templateBody ::
      Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegistrationConfig' with the minimum fields required to make a request.
--
-- * 'roleARN' - The ARN of the role.
-- * 'templateBody' - The template body.
mkRegistrationConfig ::
  RegistrationConfig
mkRegistrationConfig =
  RegistrationConfig'
    { templateBody = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The template body.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTemplateBody :: Lens.Lens' RegistrationConfig (Lude.Maybe Lude.Text)
rcTemplateBody = Lens.lens (templateBody :: RegistrationConfig -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: RegistrationConfig)
{-# DEPRECATED rcTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The ARN of the role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRoleARN :: Lens.Lens' RegistrationConfig (Lude.Maybe Lude.Text)
rcRoleARN = Lens.lens (roleARN :: RegistrationConfig -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: RegistrationConfig)
{-# DEPRECATED rcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON RegistrationConfig where
  parseJSON =
    Lude.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Lude.<$> (x Lude..:? "templateBody") Lude.<*> (x Lude..:? "roleArn")
      )

instance Lude.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("templateBody" Lude..=) Lude.<$> templateBody,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )
