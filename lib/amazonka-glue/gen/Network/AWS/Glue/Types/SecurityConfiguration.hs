{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SecurityConfiguration
  ( SecurityConfiguration (..),

    -- * Smart constructor
    mkSecurityConfiguration,

    -- * Lenses
    secName,
    secEncryptionConfiguration,
    secCreatedTimeStamp,
  )
where

import Network.AWS.Glue.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a security configuration.
--
-- /See:/ 'mkSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { name ::
      Lude.Maybe Lude.Text,
    encryptionConfiguration ::
      Lude.Maybe EncryptionConfiguration,
    createdTimeStamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'createdTimeStamp' - The time at which this security configuration was created.
-- * 'encryptionConfiguration' - The encryption configuration associated with this security configuration.
-- * 'name' - The name of the security configuration.
mkSecurityConfiguration ::
  SecurityConfiguration
mkSecurityConfiguration =
  SecurityConfiguration'
    { name = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      createdTimeStamp = Lude.Nothing
    }

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secName :: Lens.Lens' SecurityConfiguration (Lude.Maybe Lude.Text)
secName = Lens.lens (name :: SecurityConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SecurityConfiguration)
{-# DEPRECATED secName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The encryption configuration associated with this security configuration.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secEncryptionConfiguration :: Lens.Lens' SecurityConfiguration (Lude.Maybe EncryptionConfiguration)
secEncryptionConfiguration = Lens.lens (encryptionConfiguration :: SecurityConfiguration -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: SecurityConfiguration)
{-# DEPRECATED secEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The time at which this security configuration was created.
--
-- /Note:/ Consider using 'createdTimeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secCreatedTimeStamp :: Lens.Lens' SecurityConfiguration (Lude.Maybe Lude.Timestamp)
secCreatedTimeStamp = Lens.lens (createdTimeStamp :: SecurityConfiguration -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimeStamp = a} :: SecurityConfiguration)
{-# DEPRECATED secCreatedTimeStamp "Use generic-lens or generic-optics with 'createdTimeStamp' instead." #-}

instance Lude.FromJSON SecurityConfiguration where
  parseJSON =
    Lude.withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "EncryptionConfiguration")
            Lude.<*> (x Lude..:? "CreatedTimeStamp")
      )
