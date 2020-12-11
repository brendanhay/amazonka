-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LogSubscription
  ( LogSubscription (..),

    -- * Smart constructor
    mkLogSubscription,

    -- * Lenses
    lsDirectoryId,
    lsLogGroupName,
    lsSubscriptionCreatedDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a log subscription, which tracks real-time data from a chosen log group to a specified destination.
--
-- /See:/ 'mkLogSubscription' smart constructor.
data LogSubscription = LogSubscription'
  { directoryId ::
      Lude.Maybe Lude.Text,
    logGroupName :: Lude.Maybe Lude.Text,
    subscriptionCreatedDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogSubscription' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier (ID) of the directory that you want to associate with the log subscription.
-- * 'logGroupName' - The name of the log group.
-- * 'subscriptionCreatedDateTime' - The date and time that the log subscription was created.
mkLogSubscription ::
  LogSubscription
mkLogSubscription =
  LogSubscription'
    { directoryId = Lude.Nothing,
      logGroupName = Lude.Nothing,
      subscriptionCreatedDateTime = Lude.Nothing
    }

-- | Identifier (ID) of the directory that you want to associate with the log subscription.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsDirectoryId :: Lens.Lens' LogSubscription (Lude.Maybe Lude.Text)
lsDirectoryId = Lens.lens (directoryId :: LogSubscription -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: LogSubscription)
{-# DEPRECATED lsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLogGroupName :: Lens.Lens' LogSubscription (Lude.Maybe Lude.Text)
lsLogGroupName = Lens.lens (logGroupName :: LogSubscription -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: LogSubscription)
{-# DEPRECATED lsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The date and time that the log subscription was created.
--
-- /Note:/ Consider using 'subscriptionCreatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSubscriptionCreatedDateTime :: Lens.Lens' LogSubscription (Lude.Maybe Lude.Timestamp)
lsSubscriptionCreatedDateTime = Lens.lens (subscriptionCreatedDateTime :: LogSubscription -> Lude.Maybe Lude.Timestamp) (\s a -> s {subscriptionCreatedDateTime = a} :: LogSubscription)
{-# DEPRECATED lsSubscriptionCreatedDateTime "Use generic-lens or generic-optics with 'subscriptionCreatedDateTime' instead." #-}

instance Lude.FromJSON LogSubscription where
  parseJSON =
    Lude.withObject
      "LogSubscription"
      ( \x ->
          LogSubscription'
            Lude.<$> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "LogGroupName")
            Lude.<*> (x Lude..:? "SubscriptionCreatedDateTime")
      )
