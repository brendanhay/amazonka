{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Stream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.Stream
  ( Stream (..)
  -- * Smart constructor
  , mkStream
  -- * Lenses
  , sStreamArn
  , sStreamLabel
  , sTableName
  ) where

import qualified Network.AWS.DynamoDBStreams.Types.StreamArn as Types
import qualified Network.AWS.DynamoDBStreams.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'mkStream' smart constructor.
data Stream = Stream'
  { streamArn :: Core.Maybe Types.StreamArn
    -- ^ The Amazon Resource Name (ARN) for the stream.
  , streamLabel :: Core.Maybe Core.Text
    -- ^ A timestamp, in ISO 8601 format, for this stream.
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
  , tableName :: Core.Maybe Types.TableName
    -- ^ The DynamoDB table with which the stream is associated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Stream' value with any optional fields omitted.
mkStream
    :: Stream
mkStream
  = Stream'{streamArn = Core.Nothing, streamLabel = Core.Nothing,
            tableName = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamArn :: Lens.Lens' Stream (Core.Maybe Types.StreamArn)
sStreamArn = Lens.field @"streamArn"
{-# INLINEABLE sStreamArn #-}
{-# DEPRECATED streamArn "Use generic-lens or generic-optics with 'streamArn' instead"  #-}

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
sStreamLabel :: Lens.Lens' Stream (Core.Maybe Core.Text)
sStreamLabel = Lens.field @"streamLabel"
{-# INLINEABLE sStreamLabel #-}
{-# DEPRECATED streamLabel "Use generic-lens or generic-optics with 'streamLabel' instead"  #-}

-- | The DynamoDB table with which the stream is associated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTableName :: Lens.Lens' Stream (Core.Maybe Types.TableName)
sTableName = Lens.field @"tableName"
{-# INLINEABLE sTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.FromJSON Stream where
        parseJSON
          = Core.withObject "Stream" Core.$
              \ x ->
                Stream' Core.<$>
                  (x Core..:? "StreamArn") Core.<*> x Core..:? "StreamLabel" Core.<*>
                    x Core..:? "TableName"
