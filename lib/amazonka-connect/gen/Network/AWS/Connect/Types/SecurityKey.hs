{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.SecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityKey
  ( SecurityKey (..),

    -- * Smart constructor
    mkSecurityKey,

    -- * Lenses
    skCreationTime,
    skAssociationId,
    skKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information of the security key.
--
-- /See:/ 'mkSecurityKey' smart constructor.
data SecurityKey = SecurityKey'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    associationId :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityKey' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'creationTime' - When the security key was created.
-- * 'key' - The key of the security key.
mkSecurityKey ::
  SecurityKey
mkSecurityKey =
  SecurityKey'
    { creationTime = Lude.Nothing,
      associationId = Lude.Nothing,
      key = Lude.Nothing
    }

-- | When the security key was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skCreationTime :: Lens.Lens' SecurityKey (Lude.Maybe Lude.Timestamp)
skCreationTime = Lens.lens (creationTime :: SecurityKey -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: SecurityKey)
{-# DEPRECATED skCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skAssociationId :: Lens.Lens' SecurityKey (Lude.Maybe Lude.Text)
skAssociationId = Lens.lens (associationId :: SecurityKey -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: SecurityKey)
{-# DEPRECATED skAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The key of the security key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skKey :: Lens.Lens' SecurityKey (Lude.Maybe Lude.Text)
skKey = Lens.lens (key :: SecurityKey -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: SecurityKey)
{-# DEPRECATED skKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON SecurityKey where
  parseJSON =
    Lude.withObject
      "SecurityKey"
      ( \x ->
          SecurityKey'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "Key")
      )
