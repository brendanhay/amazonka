{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SerDeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SerDeInfo
  ( SerDeInfo (..),

    -- * Smart constructor
    mkSerDeInfo,

    -- * Lenses
    sdiSerializationLibrary,
    sdiName,
    sdiParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a serialization/deserialization program (SerDe) that serves as an extractor and loader.
--
-- /See:/ 'mkSerDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { -- | Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
    serializationLibrary :: Lude.Maybe Lude.Text,
    -- | Name of the SerDe.
    name :: Lude.Maybe Lude.Text,
    -- | These key-value pairs define initialization parameters for the SerDe.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SerDeInfo' with the minimum fields required to make a request.
--
-- * 'serializationLibrary' - Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
-- * 'name' - Name of the SerDe.
-- * 'parameters' - These key-value pairs define initialization parameters for the SerDe.
mkSerDeInfo ::
  SerDeInfo
mkSerDeInfo =
  SerDeInfo'
    { serializationLibrary = Lude.Nothing,
      name = Lude.Nothing,
      parameters = Lude.Nothing
    }

-- | Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
--
-- /Note:/ Consider using 'serializationLibrary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiSerializationLibrary :: Lens.Lens' SerDeInfo (Lude.Maybe Lude.Text)
sdiSerializationLibrary = Lens.lens (serializationLibrary :: SerDeInfo -> Lude.Maybe Lude.Text) (\s a -> s {serializationLibrary = a} :: SerDeInfo)
{-# DEPRECATED sdiSerializationLibrary "Use generic-lens or generic-optics with 'serializationLibrary' instead." #-}

-- | Name of the SerDe.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiName :: Lens.Lens' SerDeInfo (Lude.Maybe Lude.Text)
sdiName = Lens.lens (name :: SerDeInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SerDeInfo)
{-# DEPRECATED sdiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | These key-value pairs define initialization parameters for the SerDe.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiParameters :: Lens.Lens' SerDeInfo (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sdiParameters = Lens.lens (parameters :: SerDeInfo -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: SerDeInfo)
{-# DEPRECATED sdiParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.FromJSON SerDeInfo where
  parseJSON =
    Lude.withObject
      "SerDeInfo"
      ( \x ->
          SerDeInfo'
            Lude.<$> (x Lude..:? "SerializationLibrary")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SerDeInfo where
  toJSON SerDeInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SerializationLibrary" Lude..=) Lude.<$> serializationLibrary,
            ("Name" Lude..=) Lude.<$> name,
            ("Parameters" Lude..=) Lude.<$> parameters
          ]
      )
