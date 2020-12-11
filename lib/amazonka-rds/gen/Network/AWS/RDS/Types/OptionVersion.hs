-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionVersion
  ( OptionVersion (..),

    -- * Smart constructor
    mkOptionVersion,

    -- * Lenses
    ovVersion,
    ovIsDefault,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The version for an option. Option group option versions are returned by the @DescribeOptionGroupOptions@ action.
--
-- /See:/ 'mkOptionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { version ::
      Lude.Maybe Lude.Text,
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionVersion' with the minimum fields required to make a request.
--
-- * 'isDefault' - True if the version is the default version of the option, and otherwise false.
-- * 'version' - The version of the option.
mkOptionVersion ::
  OptionVersion
mkOptionVersion =
  OptionVersion' {version = Lude.Nothing, isDefault = Lude.Nothing}

-- | The version of the option.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovVersion :: Lens.Lens' OptionVersion (Lude.Maybe Lude.Text)
ovVersion = Lens.lens (version :: OptionVersion -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: OptionVersion)
{-# DEPRECATED ovVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | True if the version is the default version of the option, and otherwise false.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovIsDefault :: Lens.Lens' OptionVersion (Lude.Maybe Lude.Bool)
ovIsDefault = Lens.lens (isDefault :: OptionVersion -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: OptionVersion)
{-# DEPRECATED ovIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML OptionVersion where
  parseXML x =
    OptionVersion'
      Lude.<$> (x Lude..@? "Version") Lude.<*> (x Lude..@? "IsDefault")
