{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
  ( ValidDBInstanceModificationsMessage (..),

    -- * Smart constructor
    mkValidDBInstanceModificationsMessage,

    -- * Lenses
    vdimmValidProcessorFeatures,
    vdimmStorage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.AvailableProcessorFeature
import Network.AWS.RDS.Types.ValidStorageOptions

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action. You can use this information when you call @ModifyDBInstance@ .
--
-- /See:/ 'mkValidDBInstanceModificationsMessage' smart constructor.
data ValidDBInstanceModificationsMessage = ValidDBInstanceModificationsMessage'
  { validProcessorFeatures ::
      Lude.Maybe
        [AvailableProcessorFeature],
    storage ::
      Lude.Maybe
        [ValidStorageOptions]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidDBInstanceModificationsMessage' with the minimum fields required to make a request.
--
-- * 'storage' - Valid storage options for your DB instance.
-- * 'validProcessorFeatures' - Valid processor features for your DB instance.
mkValidDBInstanceModificationsMessage ::
  ValidDBInstanceModificationsMessage
mkValidDBInstanceModificationsMessage =
  ValidDBInstanceModificationsMessage'
    { validProcessorFeatures =
        Lude.Nothing,
      storage = Lude.Nothing
    }

-- | Valid processor features for your DB instance.
--
-- /Note:/ Consider using 'validProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdimmValidProcessorFeatures :: Lens.Lens' ValidDBInstanceModificationsMessage (Lude.Maybe [AvailableProcessorFeature])
vdimmValidProcessorFeatures = Lens.lens (validProcessorFeatures :: ValidDBInstanceModificationsMessage -> Lude.Maybe [AvailableProcessorFeature]) (\s a -> s {validProcessorFeatures = a} :: ValidDBInstanceModificationsMessage)
{-# DEPRECATED vdimmValidProcessorFeatures "Use generic-lens or generic-optics with 'validProcessorFeatures' instead." #-}

-- | Valid storage options for your DB instance.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdimmStorage :: Lens.Lens' ValidDBInstanceModificationsMessage (Lude.Maybe [ValidStorageOptions])
vdimmStorage = Lens.lens (storage :: ValidDBInstanceModificationsMessage -> Lude.Maybe [ValidStorageOptions]) (\s a -> s {storage = a} :: ValidDBInstanceModificationsMessage)
{-# DEPRECATED vdimmStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

instance Lude.FromXML ValidDBInstanceModificationsMessage where
  parseXML x =
    ValidDBInstanceModificationsMessage'
      Lude.<$> ( x Lude..@? "ValidProcessorFeatures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailableProcessorFeature")
               )
      Lude.<*> ( x Lude..@? "Storage" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ValidStorageOptions")
               )
