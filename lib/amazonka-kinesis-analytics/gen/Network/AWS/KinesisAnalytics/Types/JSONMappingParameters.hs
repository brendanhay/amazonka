-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
  ( JSONMappingParameters (..),

    -- * Smart constructor
    mkJSONMappingParameters,

    -- * Lenses
    jmpRecordRowPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
-- /See:/ 'mkJSONMappingParameters' smart constructor.
newtype JSONMappingParameters = JSONMappingParameters'
  { recordRowPath ::
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

-- | Creates a value of 'JSONMappingParameters' with the minimum fields required to make a request.
--
-- * 'recordRowPath' - Path to the top-level parent that contains the records.
mkJSONMappingParameters ::
  -- | 'recordRowPath'
  Lude.Text ->
  JSONMappingParameters
mkJSONMappingParameters pRecordRowPath_ =
  JSONMappingParameters' {recordRowPath = pRecordRowPath_}

-- | Path to the top-level parent that contains the records.
--
-- /Note:/ Consider using 'recordRowPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmpRecordRowPath :: Lens.Lens' JSONMappingParameters Lude.Text
jmpRecordRowPath = Lens.lens (recordRowPath :: JSONMappingParameters -> Lude.Text) (\s a -> s {recordRowPath = a} :: JSONMappingParameters)
{-# DEPRECATED jmpRecordRowPath "Use generic-lens or generic-optics with 'recordRowPath' instead." #-}

instance Lude.FromJSON JSONMappingParameters where
  parseJSON =
    Lude.withObject
      "JSONMappingParameters"
      ( \x ->
          JSONMappingParameters' Lude.<$> (x Lude..: "RecordRowPath")
      )

instance Lude.ToJSON JSONMappingParameters where
  toJSON JSONMappingParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RecordRowPath" Lude..= recordRowPath)]
      )
