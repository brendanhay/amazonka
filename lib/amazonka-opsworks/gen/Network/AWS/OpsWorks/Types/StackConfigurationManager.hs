{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackConfigurationManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackConfigurationManager
  ( StackConfigurationManager (..),

    -- * Smart constructor
    mkStackConfigurationManager,

    -- * Lenses
    scmName,
    scmVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration manager.
--
-- /See:/ 'mkStackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { -- | The name. This parameter must be set to "Chef".
    name :: Lude.Maybe Lude.Text,
    -- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackConfigurationManager' with the minimum fields required to make a request.
--
-- * 'name' - The name. This parameter must be set to "Chef".
-- * 'version' - The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
mkStackConfigurationManager ::
  StackConfigurationManager
mkStackConfigurationManager =
  StackConfigurationManager'
    { name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name. This parameter must be set to "Chef".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmName :: Lens.Lens' StackConfigurationManager (Lude.Maybe Lude.Text)
scmName = Lens.lens (name :: StackConfigurationManager -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StackConfigurationManager)
{-# DEPRECATED scmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmVersion :: Lens.Lens' StackConfigurationManager (Lude.Maybe Lude.Text)
scmVersion = Lens.lens (version :: StackConfigurationManager -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: StackConfigurationManager)
{-# DEPRECATED scmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON StackConfigurationManager where
  parseJSON =
    Lude.withObject
      "StackConfigurationManager"
      ( \x ->
          StackConfigurationManager'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Version")
      )

instance Lude.ToJSON StackConfigurationManager where
  toJSON StackConfigurationManager' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version
          ]
      )
