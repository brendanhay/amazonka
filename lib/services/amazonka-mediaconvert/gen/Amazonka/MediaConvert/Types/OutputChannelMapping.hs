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
-- Module      : Amazonka.MediaConvert.Types.OutputChannelMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputChannelMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | OutputChannel mapping settings.
--
-- /See:/ 'newOutputChannelMapping' smart constructor.
data OutputChannelMapping = OutputChannelMapping'
  { -- | Use this setting to specify your remix values when they have a decimal
    -- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
    -- remixing values to the nearest thousandth.
    inputChannelsFineTune :: Prelude.Maybe [Prelude.Double],
    -- | Use this setting to specify your remix values when they are integers,
    -- such as -10, 0, or 4.
    inputChannels :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputChannelMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputChannelsFineTune', 'outputChannelMapping_inputChannelsFineTune' - Use this setting to specify your remix values when they have a decimal
-- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
-- remixing values to the nearest thousandth.
--
-- 'inputChannels', 'outputChannelMapping_inputChannels' - Use this setting to specify your remix values when they are integers,
-- such as -10, 0, or 4.
newOutputChannelMapping ::
  OutputChannelMapping
newOutputChannelMapping =
  OutputChannelMapping'
    { inputChannelsFineTune =
        Prelude.Nothing,
      inputChannels = Prelude.Nothing
    }

-- | Use this setting to specify your remix values when they have a decimal
-- component, such as -10.312, 0.08, or 4.9. MediaConvert rounds your
-- remixing values to the nearest thousandth.
outputChannelMapping_inputChannelsFineTune :: Lens.Lens' OutputChannelMapping (Prelude.Maybe [Prelude.Double])
outputChannelMapping_inputChannelsFineTune = Lens.lens (\OutputChannelMapping' {inputChannelsFineTune} -> inputChannelsFineTune) (\s@OutputChannelMapping' {} a -> s {inputChannelsFineTune = a} :: OutputChannelMapping) Prelude.. Lens.mapping Lens.coerced

-- | Use this setting to specify your remix values when they are integers,
-- such as -10, 0, or 4.
outputChannelMapping_inputChannels :: Lens.Lens' OutputChannelMapping (Prelude.Maybe [Prelude.Int])
outputChannelMapping_inputChannels = Lens.lens (\OutputChannelMapping' {inputChannels} -> inputChannels) (\s@OutputChannelMapping' {} a -> s {inputChannels = a} :: OutputChannelMapping) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OutputChannelMapping where
  parseJSON =
    Data.withObject
      "OutputChannelMapping"
      ( \x ->
          OutputChannelMapping'
            Prelude.<$> ( x Data..:? "inputChannelsFineTune"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "inputChannels" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OutputChannelMapping where
  hashWithSalt _salt OutputChannelMapping' {..} =
    _salt `Prelude.hashWithSalt` inputChannelsFineTune
      `Prelude.hashWithSalt` inputChannels

instance Prelude.NFData OutputChannelMapping where
  rnf OutputChannelMapping' {..} =
    Prelude.rnf inputChannelsFineTune
      `Prelude.seq` Prelude.rnf inputChannels

instance Data.ToJSON OutputChannelMapping where
  toJSON OutputChannelMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputChannelsFineTune" Data..=)
              Prelude.<$> inputChannelsFineTune,
            ("inputChannels" Data..=) Prelude.<$> inputChannels
          ]
      )
