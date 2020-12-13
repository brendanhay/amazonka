{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptions
  ( SnapshotOptions (..),

    -- * Smart constructor
    mkSnapshotOptions,

    -- * Lenses
    soAutomatedSnapshotStartHour,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
-- /See:/ 'mkSnapshotOptions' smart constructor.
newtype SnapshotOptions = SnapshotOptions'
  { -- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
    automatedSnapshotStartHour :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotOptions' with the minimum fields required to make a request.
--
-- * 'automatedSnapshotStartHour' - Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
mkSnapshotOptions ::
  SnapshotOptions
mkSnapshotOptions =
  SnapshotOptions' {automatedSnapshotStartHour = Lude.Nothing}

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
-- /Note:/ Consider using 'automatedSnapshotStartHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soAutomatedSnapshotStartHour :: Lens.Lens' SnapshotOptions (Lude.Maybe Lude.Int)
soAutomatedSnapshotStartHour = Lens.lens (automatedSnapshotStartHour :: SnapshotOptions -> Lude.Maybe Lude.Int) (\s a -> s {automatedSnapshotStartHour = a} :: SnapshotOptions)
{-# DEPRECATED soAutomatedSnapshotStartHour "Use generic-lens or generic-optics with 'automatedSnapshotStartHour' instead." #-}

instance Lude.FromJSON SnapshotOptions where
  parseJSON =
    Lude.withObject
      "SnapshotOptions"
      ( \x ->
          SnapshotOptions'
            Lude.<$> (x Lude..:? "AutomatedSnapshotStartHour")
      )

instance Lude.ToJSON SnapshotOptions where
  toJSON SnapshotOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutomatedSnapshotStartHour" Lude..=)
              Lude.<$> automatedSnapshotStartHour
          ]
      )
