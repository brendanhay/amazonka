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
-- Module      : Amazonka.MediaConvert.Types.OutputGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AutomatedEncodingSettings
import Amazonka.MediaConvert.Types.Output
import Amazonka.MediaConvert.Types.OutputGroupSettings
import qualified Amazonka.Prelude as Prelude

-- | Group of outputs
--
-- /See:/ 'newOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | Name of the output group
    name :: Prelude.Maybe Prelude.Text,
    -- | Output Group settings, including type
    outputGroupSettings :: Prelude.Maybe OutputGroupSettings,
    -- | Use automated encoding to have MediaConvert choose your encoding
    -- settings for you, based on characteristics of your input video.
    automatedEncodingSettings :: Prelude.Maybe AutomatedEncodingSettings,
    -- | This object holds groups of encoding settings, one group of settings per
    -- output.
    outputs :: Prelude.Maybe [Output],
    -- | Use Custom Group Name (CustomName) to specify a name for the output
    -- group. This value is displayed on the console and can make your job
    -- settings JSON more human-readable. It does not affect your outputs. Use
    -- up to twelve characters that are either letters, numbers, spaces, or
    -- underscores.
    customName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'outputGroup_name' - Name of the output group
--
-- 'outputGroupSettings', 'outputGroup_outputGroupSettings' - Output Group settings, including type
--
-- 'automatedEncodingSettings', 'outputGroup_automatedEncodingSettings' - Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
--
-- 'outputs', 'outputGroup_outputs' - This object holds groups of encoding settings, one group of settings per
-- output.
--
-- 'customName', 'outputGroup_customName' - Use Custom Group Name (CustomName) to specify a name for the output
-- group. This value is displayed on the console and can make your job
-- settings JSON more human-readable. It does not affect your outputs. Use
-- up to twelve characters that are either letters, numbers, spaces, or
-- underscores.
newOutputGroup ::
  OutputGroup
newOutputGroup =
  OutputGroup'
    { name = Prelude.Nothing,
      outputGroupSettings = Prelude.Nothing,
      automatedEncodingSettings = Prelude.Nothing,
      outputs = Prelude.Nothing,
      customName = Prelude.Nothing
    }

-- | Name of the output group
outputGroup_name :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_name = Lens.lens (\OutputGroup' {name} -> name) (\s@OutputGroup' {} a -> s {name = a} :: OutputGroup)

-- | Output Group settings, including type
outputGroup_outputGroupSettings :: Lens.Lens' OutputGroup (Prelude.Maybe OutputGroupSettings)
outputGroup_outputGroupSettings = Lens.lens (\OutputGroup' {outputGroupSettings} -> outputGroupSettings) (\s@OutputGroup' {} a -> s {outputGroupSettings = a} :: OutputGroup)

-- | Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
outputGroup_automatedEncodingSettings :: Lens.Lens' OutputGroup (Prelude.Maybe AutomatedEncodingSettings)
outputGroup_automatedEncodingSettings = Lens.lens (\OutputGroup' {automatedEncodingSettings} -> automatedEncodingSettings) (\s@OutputGroup' {} a -> s {automatedEncodingSettings = a} :: OutputGroup)

-- | This object holds groups of encoding settings, one group of settings per
-- output.
outputGroup_outputs :: Lens.Lens' OutputGroup (Prelude.Maybe [Output])
outputGroup_outputs = Lens.lens (\OutputGroup' {outputs} -> outputs) (\s@OutputGroup' {} a -> s {outputs = a} :: OutputGroup) Prelude.. Lens.mapping Lens.coerced

-- | Use Custom Group Name (CustomName) to specify a name for the output
-- group. This value is displayed on the console and can make your job
-- settings JSON more human-readable. It does not affect your outputs. Use
-- up to twelve characters that are either letters, numbers, spaces, or
-- underscores.
outputGroup_customName :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_customName = Lens.lens (\OutputGroup' {customName} -> customName) (\s@OutputGroup' {} a -> s {customName = a} :: OutputGroup)

instance Data.FromJSON OutputGroup where
  parseJSON =
    Data.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "outputGroupSettings")
            Prelude.<*> (x Data..:? "automatedEncodingSettings")
            Prelude.<*> (x Data..:? "outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "customName")
      )

instance Prelude.Hashable OutputGroup where
  hashWithSalt _salt OutputGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputGroupSettings
      `Prelude.hashWithSalt` automatedEncodingSettings
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` customName

instance Prelude.NFData OutputGroup where
  rnf OutputGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputGroupSettings
      `Prelude.seq` Prelude.rnf automatedEncodingSettings
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf customName

instance Data.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("outputGroupSettings" Data..=)
              Prelude.<$> outputGroupSettings,
            ("automatedEncodingSettings" Data..=)
              Prelude.<$> automatedEncodingSettings,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("customName" Data..=) Prelude.<$> customName
          ]
      )
