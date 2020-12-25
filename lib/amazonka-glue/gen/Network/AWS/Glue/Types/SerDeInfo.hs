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
    sdiName,
    sdiParameters,
    sdiSerializationLibrary,
  )
where

import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.SerializationLibrary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a serialization/deserialization program (SerDe) that serves as an extractor and loader.
--
-- /See:/ 'mkSerDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { -- | Name of the SerDe.
    name :: Core.Maybe Types.Name,
    -- | These key-value pairs define initialization parameters for the SerDe.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
    serializationLibrary :: Core.Maybe Types.SerializationLibrary
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SerDeInfo' value with any optional fields omitted.
mkSerDeInfo ::
  SerDeInfo
mkSerDeInfo =
  SerDeInfo'
    { name = Core.Nothing,
      parameters = Core.Nothing,
      serializationLibrary = Core.Nothing
    }

-- | Name of the SerDe.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiName :: Lens.Lens' SerDeInfo (Core.Maybe Types.Name)
sdiName = Lens.field @"name"
{-# DEPRECATED sdiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | These key-value pairs define initialization parameters for the SerDe.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiParameters :: Lens.Lens' SerDeInfo (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
sdiParameters = Lens.field @"parameters"
{-# DEPRECATED sdiParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Usually the class that implements the SerDe. An example is @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
--
-- /Note:/ Consider using 'serializationLibrary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiSerializationLibrary :: Lens.Lens' SerDeInfo (Core.Maybe Types.SerializationLibrary)
sdiSerializationLibrary = Lens.field @"serializationLibrary"
{-# DEPRECATED sdiSerializationLibrary "Use generic-lens or generic-optics with 'serializationLibrary' instead." #-}

instance Core.FromJSON SerDeInfo where
  toJSON SerDeInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Parameters" Core..=) Core.<$> parameters,
            ("SerializationLibrary" Core..=) Core.<$> serializationLibrary
          ]
      )

instance Core.FromJSON SerDeInfo where
  parseJSON =
    Core.withObject "SerDeInfo" Core.$
      \x ->
        SerDeInfo'
          Core.<$> (x Core..:? "Name")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "SerializationLibrary")
