{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SupportedProductConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SupportedProductConfig
  ( SupportedProductConfig (..),

    -- * Smart constructor
    mkSupportedProductConfig,

    -- * Lenses
    spcArgs,
    spcName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.
--
-- /See:/ 'mkSupportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
  { -- | The list of user-supplied arguments.
    args :: Lude.Maybe [Lude.Text],
    -- | The name of the product configuration.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SupportedProductConfig' with the minimum fields required to make a request.
--
-- * 'args' - The list of user-supplied arguments.
-- * 'name' - The name of the product configuration.
mkSupportedProductConfig ::
  SupportedProductConfig
mkSupportedProductConfig =
  SupportedProductConfig' {args = Lude.Nothing, name = Lude.Nothing}

-- | The list of user-supplied arguments.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcArgs :: Lens.Lens' SupportedProductConfig (Lude.Maybe [Lude.Text])
spcArgs = Lens.lens (args :: SupportedProductConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: SupportedProductConfig)
{-# DEPRECATED spcArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The name of the product configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcName :: Lens.Lens' SupportedProductConfig (Lude.Maybe Lude.Text)
spcName = Lens.lens (name :: SupportedProductConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SupportedProductConfig)
{-# DEPRECATED spcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON SupportedProductConfig where
  toJSON SupportedProductConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Args" Lude..=) Lude.<$> args, ("Name" Lude..=) Lude.<$> name]
      )
