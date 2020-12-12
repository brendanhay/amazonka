{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
  ( ExportedEnvironmentVariable (..),

    -- * Smart constructor
    mkExportedEnvironmentVariable,

    -- * Lenses
    eevValue,
    eevName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an exported environment variable.
--
-- /See:/ 'mkExportedEnvironmentVariable' smart constructor.
data ExportedEnvironmentVariable = ExportedEnvironmentVariable'
  { value ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportedEnvironmentVariable' with the minimum fields required to make a request.
--
-- * 'name' - The name of this exported environment variable.
-- * 'value' - The value assigned to this exported environment variable.
mkExportedEnvironmentVariable ::
  ExportedEnvironmentVariable
mkExportedEnvironmentVariable =
  ExportedEnvironmentVariable'
    { value = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The value assigned to this exported environment variable.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eevValue :: Lens.Lens' ExportedEnvironmentVariable (Lude.Maybe Lude.Text)
eevValue = Lens.lens (value :: ExportedEnvironmentVariable -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ExportedEnvironmentVariable)
{-# DEPRECATED eevValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of this exported environment variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eevName :: Lens.Lens' ExportedEnvironmentVariable (Lude.Maybe Lude.Text)
eevName = Lens.lens (name :: ExportedEnvironmentVariable -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ExportedEnvironmentVariable)
{-# DEPRECATED eevName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ExportedEnvironmentVariable where
  parseJSON =
    Lude.withObject
      "ExportedEnvironmentVariable"
      ( \x ->
          ExportedEnvironmentVariable'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "name")
      )
