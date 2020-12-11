-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfigurationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NotificationConfigurationFilter
  ( NotificationConfigurationFilter (..),

    -- * Smart constructor
    mkNotificationConfigurationFilter,

    -- * Lenses
    ncfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.S3KeyFilter

-- | Specifies object key name filtering rules. For information about key name filtering, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkNotificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
  { key ::
      Lude.Maybe S3KeyFilter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationConfigurationFilter' with the minimum fields required to make a request.
--
-- * 'key' - Undocumented field.
mkNotificationConfigurationFilter ::
  NotificationConfigurationFilter
mkNotificationConfigurationFilter =
  NotificationConfigurationFilter' {key = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncfKey :: Lens.Lens' NotificationConfigurationFilter (Lude.Maybe S3KeyFilter)
ncfKey = Lens.lens (key :: NotificationConfigurationFilter -> Lude.Maybe S3KeyFilter) (\s a -> s {key = a} :: NotificationConfigurationFilter)
{-# DEPRECATED ncfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML NotificationConfigurationFilter where
  parseXML x =
    NotificationConfigurationFilter' Lude.<$> (x Lude..@? "S3Key")

instance Lude.ToXML NotificationConfigurationFilter where
  toXML NotificationConfigurationFilter' {..} =
    Lude.mconcat ["S3Key" Lude.@= key]
