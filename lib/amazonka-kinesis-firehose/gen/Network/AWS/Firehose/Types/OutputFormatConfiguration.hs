{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OutputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OutputFormatConfiguration
  ( OutputFormatConfiguration (..),

    -- * Smart constructor
    mkOutputFormatConfiguration,

    -- * Lenses
    ofcSerializer,
  )
where

import Network.AWS.Firehose.Types.Serializer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data before it writes it to Amazon S3. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkOutputFormatConfiguration' smart constructor.
newtype OutputFormatConfiguration = OutputFormatConfiguration'
  { serializer ::
      Lude.Maybe Serializer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputFormatConfiguration' with the minimum fields required to make a request.
--
-- * 'serializer' - Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
mkOutputFormatConfiguration ::
  OutputFormatConfiguration
mkOutputFormatConfiguration =
  OutputFormatConfiguration' {serializer = Lude.Nothing}

-- | Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
--
-- /Note:/ Consider using 'serializer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofcSerializer :: Lens.Lens' OutputFormatConfiguration (Lude.Maybe Serializer)
ofcSerializer = Lens.lens (serializer :: OutputFormatConfiguration -> Lude.Maybe Serializer) (\s a -> s {serializer = a} :: OutputFormatConfiguration)
{-# DEPRECATED ofcSerializer "Use generic-lens or generic-optics with 'serializer' instead." #-}

instance Lude.FromJSON OutputFormatConfiguration where
  parseJSON =
    Lude.withObject
      "OutputFormatConfiguration"
      ( \x ->
          OutputFormatConfiguration' Lude.<$> (x Lude..:? "Serializer")
      )

instance Lude.ToJSON OutputFormatConfiguration where
  toJSON OutputFormatConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("Serializer" Lude..=) Lude.<$> serializer])
