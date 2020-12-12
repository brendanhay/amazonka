{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DestinationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DestinationInfo
  ( DestinationInfo (..),

    -- * Smart constructor
    mkDestinationInfo,

    -- * Lenses
    diService,
    diId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the destination of a record.
--
-- /See:/ 'mkDestinationInfo' smart constructor.
data DestinationInfo = DestinationInfo'
  { service ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationInfo' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the resource created at the destination.
-- * 'service' - The destination service of the record.
mkDestinationInfo ::
  DestinationInfo
mkDestinationInfo =
  DestinationInfo' {service = Lude.Nothing, id = Lude.Nothing}

-- | The destination service of the record.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diService :: Lens.Lens' DestinationInfo (Lude.Maybe Lude.Text)
diService = Lens.lens (service :: DestinationInfo -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: DestinationInfo)
{-# DEPRECATED diService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The ID of the resource created at the destination.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diId :: Lens.Lens' DestinationInfo (Lude.Maybe Lude.Text)
diId = Lens.lens (id :: DestinationInfo -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DestinationInfo)
{-# DEPRECATED diId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON DestinationInfo where
  parseJSON =
    Lude.withObject
      "DestinationInfo"
      ( \x ->
          DestinationInfo'
            Lude.<$> (x Lude..:? "service") Lude.<*> (x Lude..:? "id")
      )
