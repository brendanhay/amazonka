{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SecurityConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SecurityConfigurationSummary
  ( SecurityConfigurationSummary (..),

    -- * Smart constructor
    mkSecurityConfigurationSummary,

    -- * Lenses
    scsName,
    scsCreationDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The creation date and time, and name, of a security configuration.
--
-- /See:/ 'mkSecurityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
  { name ::
      Lude.Maybe Lude.Text,
    creationDateTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityConfigurationSummary' with the minimum fields required to make a request.
--
-- * 'creationDateTime' - The date and time the security configuration was created.
-- * 'name' - The name of the security configuration.
mkSecurityConfigurationSummary ::
  SecurityConfigurationSummary
mkSecurityConfigurationSummary =
  SecurityConfigurationSummary'
    { name = Lude.Nothing,
      creationDateTime = Lude.Nothing
    }

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsName :: Lens.Lens' SecurityConfigurationSummary (Lude.Maybe Lude.Text)
scsName = Lens.lens (name :: SecurityConfigurationSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SecurityConfigurationSummary)
{-# DEPRECATED scsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time the security configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsCreationDateTime :: Lens.Lens' SecurityConfigurationSummary (Lude.Maybe Lude.Timestamp)
scsCreationDateTime = Lens.lens (creationDateTime :: SecurityConfigurationSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: SecurityConfigurationSummary)
{-# DEPRECATED scsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

instance Lude.FromJSON SecurityConfigurationSummary where
  parseJSON =
    Lude.withObject
      "SecurityConfigurationSummary"
      ( \x ->
          SecurityConfigurationSummary'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "CreationDateTime")
      )
