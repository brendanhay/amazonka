{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputChannelMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputChannelMapping where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | OutputChannel mapping settings.
--
-- /See:/ 'newOutputChannelMapping' smart constructor.
data OutputChannelMapping = OutputChannelMapping'
  { -- | Use this setting to specify your remix values when they are integers,
    -- such as -10, 0, or 4.
    inputChannels :: Prelude.Maybe [Prelude.Int],
    -- | Use this setting to specify your remix values when they have a decimal
    -- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
    -- remixing values to the nearest thousandth.
    inputChannelsFineTune :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputChannelMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputChannels', 'outputChannelMapping_inputChannels' - Use this setting to specify your remix values when they are integers,
-- such as -10, 0, or 4.
--
-- 'inputChannelsFineTune', 'outputChannelMapping_inputChannelsFineTune' - Use this setting to specify your remix values when they have a decimal
-- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
-- remixing values to the nearest thousandth.
newOutputChannelMapping ::
  OutputChannelMapping
newOutputChannelMapping =
  OutputChannelMapping'
    { inputChannels =
        Prelude.Nothing,
      inputChannelsFineTune = Prelude.Nothing
    }

-- | Use this setting to specify your remix values when they are integers,
-- such as -10, 0, or 4.
outputChannelMapping_inputChannels :: Lens.Lens' OutputChannelMapping (Prelude.Maybe [Prelude.Int])
outputChannelMapping_inputChannels = Lens.lens (\OutputChannelMapping' {inputChannels} -> inputChannels) (\s@OutputChannelMapping' {} a -> s {inputChannels = a} :: OutputChannelMapping) Prelude.. Lens.mapping Prelude._Coerce

-- | Use this setting to specify your remix values when they have a decimal
-- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
-- remixing values to the nearest thousandth.
outputChannelMapping_inputChannelsFineTune :: Lens.Lens' OutputChannelMapping (Prelude.Maybe [Prelude.Double])
outputChannelMapping_inputChannelsFineTune = Lens.lens (\OutputChannelMapping' {inputChannelsFineTune} -> inputChannelsFineTune) (\s@OutputChannelMapping' {} a -> s {inputChannelsFineTune = a} :: OutputChannelMapping) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON OutputChannelMapping where
  parseJSON =
    Prelude.withObject
      "OutputChannelMapping"
      ( \x ->
          OutputChannelMapping'
            Prelude.<$> ( x Prelude..:? "inputChannels"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "inputChannelsFineTune"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OutputChannelMapping

instance Prelude.NFData OutputChannelMapping

instance Prelude.ToJSON OutputChannelMapping where
  toJSON OutputChannelMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputChannels" Prelude..=)
              Prelude.<$> inputChannels,
            ("inputChannelsFineTune" Prelude..=)
              Prelude.<$> inputChannelsFineTune
          ]
      )
