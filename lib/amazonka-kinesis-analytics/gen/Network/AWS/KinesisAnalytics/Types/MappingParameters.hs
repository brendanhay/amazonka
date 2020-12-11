-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.MappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.MappingParameters
  ( MappingParameters (..),

    -- * Smart constructor
    mkMappingParameters,

    -- * Lenses
    mpCSVMappingParameters,
    mpJSONMappingParameters,
  )
where

import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- /See:/ 'mkMappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { csvMappingParameters ::
      Lude.Maybe CSVMappingParameters,
    jsonMappingParameters ::
      Lude.Maybe JSONMappingParameters
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MappingParameters' with the minimum fields required to make a request.
--
-- * 'csvMappingParameters' - Provides additional mapping information when the record format uses delimiters (for example, CSV).
-- * 'jsonMappingParameters' - Provides additional mapping information when JSON is the record format on the streaming source.
mkMappingParameters ::
  MappingParameters
mkMappingParameters =
  MappingParameters'
    { csvMappingParameters = Lude.Nothing,
      jsonMappingParameters = Lude.Nothing
    }

-- | Provides additional mapping information when the record format uses delimiters (for example, CSV).
--
-- /Note:/ Consider using 'csvMappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpCSVMappingParameters :: Lens.Lens' MappingParameters (Lude.Maybe CSVMappingParameters)
mpCSVMappingParameters = Lens.lens (csvMappingParameters :: MappingParameters -> Lude.Maybe CSVMappingParameters) (\s a -> s {csvMappingParameters = a} :: MappingParameters)
{-# DEPRECATED mpCSVMappingParameters "Use generic-lens or generic-optics with 'csvMappingParameters' instead." #-}

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
-- /Note:/ Consider using 'jsonMappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpJSONMappingParameters :: Lens.Lens' MappingParameters (Lude.Maybe JSONMappingParameters)
mpJSONMappingParameters = Lens.lens (jsonMappingParameters :: MappingParameters -> Lude.Maybe JSONMappingParameters) (\s a -> s {jsonMappingParameters = a} :: MappingParameters)
{-# DEPRECATED mpJSONMappingParameters "Use generic-lens or generic-optics with 'jsonMappingParameters' instead." #-}

instance Lude.FromJSON MappingParameters where
  parseJSON =
    Lude.withObject
      "MappingParameters"
      ( \x ->
          MappingParameters'
            Lude.<$> (x Lude..:? "CSVMappingParameters")
            Lude.<*> (x Lude..:? "JSONMappingParameters")
      )

instance Lude.ToJSON MappingParameters where
  toJSON MappingParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CSVMappingParameters" Lude..=) Lude.<$> csvMappingParameters,
            ("JSONMappingParameters" Lude..=) Lude.<$> jsonMappingParameters
          ]
      )
