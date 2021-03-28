{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Serializer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Serializer
  ( Serializer (..)
  -- * Smart constructor
  , mkSerializer
  -- * Lenses
  , sOrcSerDe
  , sParquetSerDe
  ) where

import qualified Network.AWS.Firehose.Types.OrcSerDe as Types
import qualified Network.AWS.Firehose.Types.ParquetSerDe as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The serializer that you want Kinesis Data Firehose to use to convert data to the target format before writing it to Amazon S3. Kinesis Data Firehose supports two types of serializers: the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/orc/OrcSerde.html ORC SerDe> and the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/parquet/serde/ParquetHiveSerDe.html Parquet SerDe> .
--
-- /See:/ 'mkSerializer' smart constructor.
data Serializer = Serializer'
  { orcSerDe :: Core.Maybe Types.OrcSerDe
    -- ^ A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
  , parquetSerDe :: Core.Maybe Types.ParquetSerDe
    -- ^ A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Serializer' value with any optional fields omitted.
mkSerializer
    :: Serializer
mkSerializer
  = Serializer'{orcSerDe = Core.Nothing, parquetSerDe = Core.Nothing}

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- /Note:/ Consider using 'orcSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOrcSerDe :: Lens.Lens' Serializer (Core.Maybe Types.OrcSerDe)
sOrcSerDe = Lens.field @"orcSerDe"
{-# INLINEABLE sOrcSerDe #-}
{-# DEPRECATED orcSerDe "Use generic-lens or generic-optics with 'orcSerDe' instead"  #-}

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
-- /Note:/ Consider using 'parquetSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParquetSerDe :: Lens.Lens' Serializer (Core.Maybe Types.ParquetSerDe)
sParquetSerDe = Lens.field @"parquetSerDe"
{-# INLINEABLE sParquetSerDe #-}
{-# DEPRECATED parquetSerDe "Use generic-lens or generic-optics with 'parquetSerDe' instead"  #-}

instance Core.FromJSON Serializer where
        toJSON Serializer{..}
          = Core.object
              (Core.catMaybes
                 [("OrcSerDe" Core..=) Core.<$> orcSerDe,
                  ("ParquetSerDe" Core..=) Core.<$> parquetSerDe])

instance Core.FromJSON Serializer where
        parseJSON
          = Core.withObject "Serializer" Core.$
              \ x ->
                Serializer' Core.<$>
                  (x Core..:? "OrcSerDe") Core.<*> x Core..:? "ParquetSerDe"
