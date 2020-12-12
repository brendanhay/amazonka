{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
  ( LogPublishingOptionsStatus (..),

    -- * Smart constructor
    mkLogPublishingOptionsStatus,

    -- * Lenses
    lposStatus,
    lposOptions,
  )
where

import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configured log publishing options for the domain and their current status.
--
-- /See:/ 'mkLogPublishingOptionsStatus' smart constructor.
data LogPublishingOptionsStatus = LogPublishingOptionsStatus'
  { status ::
      Lude.Maybe OptionStatus,
    options ::
      Lude.Maybe
        ( Lude.HashMap
            LogType
            (LogPublishingOption)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogPublishingOptionsStatus' with the minimum fields required to make a request.
--
-- * 'options' - The log publishing options configured for the Elasticsearch domain.
-- * 'status' - The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
mkLogPublishingOptionsStatus ::
  LogPublishingOptionsStatus
mkLogPublishingOptionsStatus =
  LogPublishingOptionsStatus'
    { status = Lude.Nothing,
      options = Lude.Nothing
    }

-- | The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lposStatus :: Lens.Lens' LogPublishingOptionsStatus (Lude.Maybe OptionStatus)
lposStatus = Lens.lens (status :: LogPublishingOptionsStatus -> Lude.Maybe OptionStatus) (\s a -> s {status = a} :: LogPublishingOptionsStatus)
{-# DEPRECATED lposStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The log publishing options configured for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lposOptions :: Lens.Lens' LogPublishingOptionsStatus (Lude.Maybe (Lude.HashMap LogType (LogPublishingOption)))
lposOptions = Lens.lens (options :: LogPublishingOptionsStatus -> Lude.Maybe (Lude.HashMap LogType (LogPublishingOption))) (\s a -> s {options = a} :: LogPublishingOptionsStatus)
{-# DEPRECATED lposOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON LogPublishingOptionsStatus where
  parseJSON =
    Lude.withObject
      "LogPublishingOptionsStatus"
      ( \x ->
          LogPublishingOptionsStatus'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Options" Lude..!= Lude.mempty)
      )
