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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | OutputChannel mapping settings.
--
-- /See:/ 'newOutputChannelMapping' smart constructor.
data OutputChannelMapping = OutputChannelMapping'
  { -- | Use this setting to specify your remix values when they are integers,
    -- such as -10, 0, or 4.
    inputChannels :: Core.Maybe [Core.Int],
    -- | Use this setting to specify your remix values when they have a decimal
    -- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
    -- remixing values to the nearest thousandth.
    inputChannelsFineTune :: Core.Maybe [Core.Double]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { inputChannels = Core.Nothing,
      inputChannelsFineTune = Core.Nothing
    }

-- | Use this setting to specify your remix values when they are integers,
-- such as -10, 0, or 4.
outputChannelMapping_inputChannels :: Lens.Lens' OutputChannelMapping (Core.Maybe [Core.Int])
outputChannelMapping_inputChannels = Lens.lens (\OutputChannelMapping' {inputChannels} -> inputChannels) (\s@OutputChannelMapping' {} a -> s {inputChannels = a} :: OutputChannelMapping) Core.. Lens.mapping Lens._Coerce

-- | Use this setting to specify your remix values when they have a decimal
-- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
-- remixing values to the nearest thousandth.
outputChannelMapping_inputChannelsFineTune :: Lens.Lens' OutputChannelMapping (Core.Maybe [Core.Double])
outputChannelMapping_inputChannelsFineTune = Lens.lens (\OutputChannelMapping' {inputChannelsFineTune} -> inputChannelsFineTune) (\s@OutputChannelMapping' {} a -> s {inputChannelsFineTune = a} :: OutputChannelMapping) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON OutputChannelMapping where
  parseJSON =
    Core.withObject
      "OutputChannelMapping"
      ( \x ->
          OutputChannelMapping'
            Core.<$> (x Core..:? "inputChannels" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "inputChannelsFineTune"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable OutputChannelMapping

instance Core.NFData OutputChannelMapping

instance Core.ToJSON OutputChannelMapping where
  toJSON OutputChannelMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inputChannels" Core..=) Core.<$> inputChannels,
            ("inputChannelsFineTune" Core..=)
              Core.<$> inputChannelsFineTune
          ]
      )
