{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
  ( SnapshotOptionsStatus (..),

    -- * Smart constructor
    mkSnapshotOptionsStatus,

    -- * Lenses
    sosStatus,
    sosOptions,
  )
where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of a daily automated snapshot.
--
-- /See:/ 'mkSnapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { -- | Specifies the status of a daily automated snapshot.
    status :: OptionStatus,
    -- | Specifies the daily snapshot options specified for the Elasticsearch domain.
    options :: SnapshotOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotOptionsStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of a daily automated snapshot.
-- * 'options' - Specifies the daily snapshot options specified for the Elasticsearch domain.
mkSnapshotOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  SnapshotOptions ->
  SnapshotOptionsStatus
mkSnapshotOptionsStatus pStatus_ pOptions_ =
  SnapshotOptionsStatus' {status = pStatus_, options = pOptions_}

-- | Specifies the status of a daily automated snapshot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosStatus :: Lens.Lens' SnapshotOptionsStatus OptionStatus
sosStatus = Lens.lens (status :: SnapshotOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: SnapshotOptionsStatus)
{-# DEPRECATED sosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sosOptions :: Lens.Lens' SnapshotOptionsStatus SnapshotOptions
sosOptions = Lens.lens (options :: SnapshotOptionsStatus -> SnapshotOptions) (\s a -> s {options = a} :: SnapshotOptionsStatus)
{-# DEPRECATED sosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON SnapshotOptionsStatus where
  parseJSON =
    Lude.withObject
      "SnapshotOptionsStatus"
      ( \x ->
          SnapshotOptionsStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
