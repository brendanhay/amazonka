-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Stream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Stream
  ( Stream (..),

    -- * Smart constructor
    mkStream,

    -- * Lenses
    sStreamLabel,
    sStreamARN,
    sTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'mkStream' smart constructor.
data Stream = Stream'
  { streamLabel :: Lude.Maybe Lude.Text,
    streamARN :: Lude.Maybe Lude.Text,
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stream' with the minimum fields required to make a request.
--
-- * 'streamARN' - The Amazon Resource Name (ARN) for the stream.
-- * 'streamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * the AWS customer ID.
--
--
--     * the table name
--
--
--     * the @StreamLabel@
--
--
-- * 'tableName' - The DynamoDB table with which the stream is associated.
mkStream ::
  Stream
mkStream =
  Stream'
    { streamLabel = Lude.Nothing,
      streamARN = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * the AWS customer ID.
--
--
--     * the table name
--
--
--     * the @StreamLabel@
--
--
--
-- /Note:/ Consider using 'streamLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamLabel :: Lens.Lens' Stream (Lude.Maybe Lude.Text)
sStreamLabel = Lens.lens (streamLabel :: Stream -> Lude.Maybe Lude.Text) (\s a -> s {streamLabel = a} :: Stream)
{-# DEPRECATED sStreamLabel "Use generic-lens or generic-optics with 'streamLabel' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamARN :: Lens.Lens' Stream (Lude.Maybe Lude.Text)
sStreamARN = Lens.lens (streamARN :: Stream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: Stream)
{-# DEPRECATED sStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The DynamoDB table with which the stream is associated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTableName :: Lens.Lens' Stream (Lude.Maybe Lude.Text)
sTableName = Lens.lens (tableName :: Stream -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: Stream)
{-# DEPRECATED sTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON Stream where
  parseJSON =
    Lude.withObject
      "Stream"
      ( \x ->
          Stream'
            Lude.<$> (x Lude..:? "StreamLabel")
            Lude.<*> (x Lude..:? "StreamArn")
            Lude.<*> (x Lude..:? "TableName")
      )
