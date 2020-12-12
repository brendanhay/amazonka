{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RegistryListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryListItem
  ( RegistryListItem (..),

    -- * Smart constructor
    mkRegistryListItem,

    -- * Lenses
    rliStatus,
    rliRegistryName,
    rliCreatedTime,
    rliRegistryARN,
    rliUpdatedTime,
    rliDescription,
  )
where

import Network.AWS.Glue.Types.RegistryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure containing the details for a registry.
--
-- /See:/ 'mkRegistryListItem' smart constructor.
data RegistryListItem = RegistryListItem'
  { status ::
      Lude.Maybe RegistryStatus,
    registryName :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text,
    updatedTime :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegistryListItem' with the minimum fields required to make a request.
--
-- * 'createdTime' - The data the registry was created.
-- * 'description' - A description of the registry.
-- * 'registryARN' - The Amazon Resource Name (ARN) of the registry.
-- * 'registryName' - The name of the registry.
-- * 'status' - The status of the registry.
-- * 'updatedTime' - The date the registry was updated.
mkRegistryListItem ::
  RegistryListItem
mkRegistryListItem =
  RegistryListItem'
    { status = Lude.Nothing,
      registryName = Lude.Nothing,
      createdTime = Lude.Nothing,
      registryARN = Lude.Nothing,
      updatedTime = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The status of the registry.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliStatus :: Lens.Lens' RegistryListItem (Lude.Maybe RegistryStatus)
rliStatus = Lens.lens (status :: RegistryListItem -> Lude.Maybe RegistryStatus) (\s a -> s {status = a} :: RegistryListItem)
{-# DEPRECATED rliStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliRegistryName :: Lens.Lens' RegistryListItem (Lude.Maybe Lude.Text)
rliRegistryName = Lens.lens (registryName :: RegistryListItem -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: RegistryListItem)
{-# DEPRECATED rliRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The data the registry was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliCreatedTime :: Lens.Lens' RegistryListItem (Lude.Maybe Lude.Text)
rliCreatedTime = Lens.lens (createdTime :: RegistryListItem -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: RegistryListItem)
{-# DEPRECATED rliCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliRegistryARN :: Lens.Lens' RegistryListItem (Lude.Maybe Lude.Text)
rliRegistryARN = Lens.lens (registryARN :: RegistryListItem -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: RegistryListItem)
{-# DEPRECATED rliRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The date the registry was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliUpdatedTime :: Lens.Lens' RegistryListItem (Lude.Maybe Lude.Text)
rliUpdatedTime = Lens.lens (updatedTime :: RegistryListItem -> Lude.Maybe Lude.Text) (\s a -> s {updatedTime = a} :: RegistryListItem)
{-# DEPRECATED rliUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | A description of the registry.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rliDescription :: Lens.Lens' RegistryListItem (Lude.Maybe Lude.Text)
rliDescription = Lens.lens (description :: RegistryListItem -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegistryListItem)
{-# DEPRECATED rliDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RegistryListItem where
  parseJSON =
    Lude.withObject
      "RegistryListItem"
      ( \x ->
          RegistryListItem'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "RegistryName")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "RegistryArn")
            Lude.<*> (x Lude..:? "UpdatedTime")
            Lude.<*> (x Lude..:? "Description")
      )
