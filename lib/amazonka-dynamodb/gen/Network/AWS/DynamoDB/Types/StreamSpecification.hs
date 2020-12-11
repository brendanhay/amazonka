-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.StreamSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.StreamSpecification
  ( StreamSpecification (..),

    -- * Smart constructor
    mkStreamSpecification,

    -- * Lenses
    ssStreamViewType,
    ssStreamEnabled,
  )
where

import Network.AWS.DynamoDB.Types.StreamViewType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the DynamoDB Streams configuration for a table in DynamoDB.
--
-- /See:/ 'mkStreamSpecification' smart constructor.
data StreamSpecification = StreamSpecification'
  { streamViewType ::
      Lude.Maybe StreamViewType,
    streamEnabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamSpecification' with the minimum fields required to make a request.
--
-- * 'streamEnabled' - Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
-- * 'streamViewType' - When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:
--
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
mkStreamSpecification ::
  -- | 'streamEnabled'
  Lude.Bool ->
  StreamSpecification
mkStreamSpecification pStreamEnabled_ =
  StreamSpecification'
    { streamViewType = Lude.Nothing,
      streamEnabled = pStreamEnabled_
    }

-- | When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:
--
--
--     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.
--
--
--     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.
--
--
--     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.
--
--
--     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
--
--
-- /Note:/ Consider using 'streamViewType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamViewType :: Lens.Lens' StreamSpecification (Lude.Maybe StreamViewType)
ssStreamViewType = Lens.lens (streamViewType :: StreamSpecification -> Lude.Maybe StreamViewType) (\s a -> s {streamViewType = a} :: StreamSpecification)
{-# DEPRECATED ssStreamViewType "Use generic-lens or generic-optics with 'streamViewType' instead." #-}

-- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'streamEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamEnabled :: Lens.Lens' StreamSpecification Lude.Bool
ssStreamEnabled = Lens.lens (streamEnabled :: StreamSpecification -> Lude.Bool) (\s a -> s {streamEnabled = a} :: StreamSpecification)
{-# DEPRECATED ssStreamEnabled "Use generic-lens or generic-optics with 'streamEnabled' instead." #-}

instance Lude.FromJSON StreamSpecification where
  parseJSON =
    Lude.withObject
      "StreamSpecification"
      ( \x ->
          StreamSpecification'
            Lude.<$> (x Lude..:? "StreamViewType") Lude.<*> (x Lude..: "StreamEnabled")
      )

instance Lude.ToJSON StreamSpecification where
  toJSON StreamSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamViewType" Lude..=) Lude.<$> streamViewType,
            Lude.Just ("StreamEnabled" Lude..= streamEnabled)
          ]
      )
