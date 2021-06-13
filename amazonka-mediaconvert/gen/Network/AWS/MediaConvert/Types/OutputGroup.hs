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
-- Module      : Network.AWS.MediaConvert.Types.OutputGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
import Network.AWS.MediaConvert.Types.Output
import Network.AWS.MediaConvert.Types.OutputGroupSettings
import qualified Network.AWS.Prelude as Prelude

-- | Group of outputs
--
-- /See:/ 'newOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | This object holds groups of encoding settings, one group of settings per
    -- output.
    outputs :: Prelude.Maybe [Output],
    -- | Use automated encoding to have MediaConvert choose your encoding
    -- settings for you, based on characteristics of your input video.
    automatedEncodingSettings :: Prelude.Maybe AutomatedEncodingSettings,
    -- | Output Group settings, including type
    outputGroupSettings :: Prelude.Maybe OutputGroupSettings,
    -- | Name of the output group
    name :: Prelude.Maybe Prelude.Text,
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
-- 'outputs', 'outputGroup_outputs' - This object holds groups of encoding settings, one group of settings per
-- output.
--
-- 'automatedEncodingSettings', 'outputGroup_automatedEncodingSettings' - Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
--
-- 'outputGroupSettings', 'outputGroup_outputGroupSettings' - Output Group settings, including type
--
-- 'name', 'outputGroup_name' - Name of the output group
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
    { outputs = Prelude.Nothing,
      automatedEncodingSettings = Prelude.Nothing,
      outputGroupSettings = Prelude.Nothing,
      name = Prelude.Nothing,
      customName = Prelude.Nothing
    }

-- | This object holds groups of encoding settings, one group of settings per
-- output.
outputGroup_outputs :: Lens.Lens' OutputGroup (Prelude.Maybe [Output])
outputGroup_outputs = Lens.lens (\OutputGroup' {outputs} -> outputs) (\s@OutputGroup' {} a -> s {outputs = a} :: OutputGroup) Prelude.. Lens.mapping Lens._Coerce

-- | Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
outputGroup_automatedEncodingSettings :: Lens.Lens' OutputGroup (Prelude.Maybe AutomatedEncodingSettings)
outputGroup_automatedEncodingSettings = Lens.lens (\OutputGroup' {automatedEncodingSettings} -> automatedEncodingSettings) (\s@OutputGroup' {} a -> s {automatedEncodingSettings = a} :: OutputGroup)

-- | Output Group settings, including type
outputGroup_outputGroupSettings :: Lens.Lens' OutputGroup (Prelude.Maybe OutputGroupSettings)
outputGroup_outputGroupSettings = Lens.lens (\OutputGroup' {outputGroupSettings} -> outputGroupSettings) (\s@OutputGroup' {} a -> s {outputGroupSettings = a} :: OutputGroup)

-- | Name of the output group
outputGroup_name :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_name = Lens.lens (\OutputGroup' {name} -> name) (\s@OutputGroup' {} a -> s {name = a} :: OutputGroup)

-- | Use Custom Group Name (CustomName) to specify a name for the output
-- group. This value is displayed on the console and can make your job
-- settings JSON more human-readable. It does not affect your outputs. Use
-- up to twelve characters that are either letters, numbers, spaces, or
-- underscores.
outputGroup_customName :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_customName = Lens.lens (\OutputGroup' {customName} -> customName) (\s@OutputGroup' {} a -> s {customName = a} :: OutputGroup)

instance Core.FromJSON OutputGroup where
  parseJSON =
    Core.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Prelude.<$> (x Core..:? "outputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "automatedEncodingSettings")
            Prelude.<*> (x Core..:? "outputGroupSettings")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "customName")
      )

instance Prelude.Hashable OutputGroup

instance Prelude.NFData OutputGroup

instance Core.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("outputs" Core..=) Prelude.<$> outputs,
            ("automatedEncodingSettings" Core..=)
              Prelude.<$> automatedEncodingSettings,
            ("outputGroupSettings" Core..=)
              Prelude.<$> outputGroupSettings,
            ("name" Core..=) Prelude.<$> name,
            ("customName" Core..=) Prelude.<$> customName
          ]
      )
