{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Serializer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Serializer
  ( Serializer (..),

    -- * Smart constructor
    mkSerializer,

    -- * Lenses
    sOrcSerDe,
    sParquetSerDe,
  )
where

import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.ParquetSerDe
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The serializer that you want Kinesis Data Firehose to use to convert data to the target format before writing it to Amazon S3. Kinesis Data Firehose supports two types of serializers: the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/orc/OrcSerde.html ORC SerDe> and the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/parquet/serde/ParquetHiveSerDe.html Parquet SerDe> .
--
-- /See:/ 'mkSerializer' smart constructor.
data Serializer = Serializer'
  { -- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
    orcSerDe :: Lude.Maybe OrcSerDe,
    -- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
    parquetSerDe :: Lude.Maybe ParquetSerDe
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Serializer' with the minimum fields required to make a request.
--
-- * 'orcSerDe' - A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
-- * 'parquetSerDe' - A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
mkSerializer ::
  Serializer
mkSerializer =
  Serializer' {orcSerDe = Lude.Nothing, parquetSerDe = Lude.Nothing}

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- /Note:/ Consider using 'orcSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOrcSerDe :: Lens.Lens' Serializer (Lude.Maybe OrcSerDe)
sOrcSerDe = Lens.lens (orcSerDe :: Serializer -> Lude.Maybe OrcSerDe) (\s a -> s {orcSerDe = a} :: Serializer)
{-# DEPRECATED sOrcSerDe "Use generic-lens or generic-optics with 'orcSerDe' instead." #-}

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
-- /Note:/ Consider using 'parquetSerDe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParquetSerDe :: Lens.Lens' Serializer (Lude.Maybe ParquetSerDe)
sParquetSerDe = Lens.lens (parquetSerDe :: Serializer -> Lude.Maybe ParquetSerDe) (\s a -> s {parquetSerDe = a} :: Serializer)
{-# DEPRECATED sParquetSerDe "Use generic-lens or generic-optics with 'parquetSerDe' instead." #-}

instance Lude.FromJSON Serializer where
  parseJSON =
    Lude.withObject
      "Serializer"
      ( \x ->
          Serializer'
            Lude.<$> (x Lude..:? "OrcSerDe") Lude.<*> (x Lude..:? "ParquetSerDe")
      )

instance Lude.ToJSON Serializer where
  toJSON Serializer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrcSerDe" Lude..=) Lude.<$> orcSerDe,
            ("ParquetSerDe" Lude..=) Lude.<$> parquetSerDe
          ]
      )
