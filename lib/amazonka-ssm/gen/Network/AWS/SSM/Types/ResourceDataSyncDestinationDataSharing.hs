{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
  ( ResourceDataSyncDestinationDataSharing (..),

    -- * Smart constructor
    mkResourceDataSyncDestinationDataSharing,

    -- * Lenses
    rdsddsDestinationDataSharingType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Synchronize Systems Manager Inventory data from multiple AWS accounts defined in AWS Organizations to a centralized S3 bucket. Data is synchronized to individual key prefixes in the central bucket. Each key prefix represents a different AWS account ID.
--
-- /See:/ 'mkResourceDataSyncDestinationDataSharing' smart constructor.
newtype ResourceDataSyncDestinationDataSharing = ResourceDataSyncDestinationDataSharing'
  { destinationDataSharingType ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncDestinationDataSharing' with the minimum fields required to make a request.
--
-- * 'destinationDataSharingType' - The sharing data type. Only @Organization@ is supported.
mkResourceDataSyncDestinationDataSharing ::
  ResourceDataSyncDestinationDataSharing
mkResourceDataSyncDestinationDataSharing =
  ResourceDataSyncDestinationDataSharing'
    { destinationDataSharingType =
        Lude.Nothing
    }

-- | The sharing data type. Only @Organization@ is supported.
--
-- /Note:/ Consider using 'destinationDataSharingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsddsDestinationDataSharingType :: Lens.Lens' ResourceDataSyncDestinationDataSharing (Lude.Maybe Lude.Text)
rdsddsDestinationDataSharingType = Lens.lens (destinationDataSharingType :: ResourceDataSyncDestinationDataSharing -> Lude.Maybe Lude.Text) (\s a -> s {destinationDataSharingType = a} :: ResourceDataSyncDestinationDataSharing)
{-# DEPRECATED rdsddsDestinationDataSharingType "Use generic-lens or generic-optics with 'destinationDataSharingType' instead." #-}

instance Lude.FromJSON ResourceDataSyncDestinationDataSharing where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncDestinationDataSharing"
      ( \x ->
          ResourceDataSyncDestinationDataSharing'
            Lude.<$> (x Lude..:? "DestinationDataSharingType")
      )

instance Lude.ToJSON ResourceDataSyncDestinationDataSharing where
  toJSON ResourceDataSyncDestinationDataSharing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DestinationDataSharingType" Lude..=)
              Lude.<$> destinationDataSharingType
          ]
      )
