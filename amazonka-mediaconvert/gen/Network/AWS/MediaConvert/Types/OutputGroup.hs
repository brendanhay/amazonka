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

-- | Group of outputs
--
-- /See:/ 'newOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | This object holds groups of encoding settings, one group of settings per
    -- output.
    outputs :: Core.Maybe [Output],
    -- | Use automated encoding to have MediaConvert choose your encoding
    -- settings for you, based on characteristics of your input video.
    automatedEncodingSettings :: Core.Maybe AutomatedEncodingSettings,
    -- | Output Group settings, including type
    outputGroupSettings :: Core.Maybe OutputGroupSettings,
    -- | Name of the output group
    name :: Core.Maybe Core.Text,
    -- | Use Custom Group Name (CustomName) to specify a name for the output
    -- group. This value is displayed on the console and can make your job
    -- settings JSON more human-readable. It does not affect your outputs. Use
    -- up to twelve characters that are either letters, numbers, spaces, or
    -- underscores.
    customName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { outputs = Core.Nothing,
      automatedEncodingSettings = Core.Nothing,
      outputGroupSettings = Core.Nothing,
      name = Core.Nothing,
      customName = Core.Nothing
    }

-- | This object holds groups of encoding settings, one group of settings per
-- output.
outputGroup_outputs :: Lens.Lens' OutputGroup (Core.Maybe [Output])
outputGroup_outputs = Lens.lens (\OutputGroup' {outputs} -> outputs) (\s@OutputGroup' {} a -> s {outputs = a} :: OutputGroup) Core.. Lens.mapping Lens._Coerce

-- | Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
outputGroup_automatedEncodingSettings :: Lens.Lens' OutputGroup (Core.Maybe AutomatedEncodingSettings)
outputGroup_automatedEncodingSettings = Lens.lens (\OutputGroup' {automatedEncodingSettings} -> automatedEncodingSettings) (\s@OutputGroup' {} a -> s {automatedEncodingSettings = a} :: OutputGroup)

-- | Output Group settings, including type
outputGroup_outputGroupSettings :: Lens.Lens' OutputGroup (Core.Maybe OutputGroupSettings)
outputGroup_outputGroupSettings = Lens.lens (\OutputGroup' {outputGroupSettings} -> outputGroupSettings) (\s@OutputGroup' {} a -> s {outputGroupSettings = a} :: OutputGroup)

-- | Name of the output group
outputGroup_name :: Lens.Lens' OutputGroup (Core.Maybe Core.Text)
outputGroup_name = Lens.lens (\OutputGroup' {name} -> name) (\s@OutputGroup' {} a -> s {name = a} :: OutputGroup)

-- | Use Custom Group Name (CustomName) to specify a name for the output
-- group. This value is displayed on the console and can make your job
-- settings JSON more human-readable. It does not affect your outputs. Use
-- up to twelve characters that are either letters, numbers, spaces, or
-- underscores.
outputGroup_customName :: Lens.Lens' OutputGroup (Core.Maybe Core.Text)
outputGroup_customName = Lens.lens (\OutputGroup' {customName} -> customName) (\s@OutputGroup' {} a -> s {customName = a} :: OutputGroup)

instance Core.FromJSON OutputGroup where
  parseJSON =
    Core.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Core.<$> (x Core..:? "outputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "automatedEncodingSettings")
            Core.<*> (x Core..:? "outputGroupSettings")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "customName")
      )

instance Core.Hashable OutputGroup

instance Core.NFData OutputGroup

instance Core.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputs" Core..=) Core.<$> outputs,
            ("automatedEncodingSettings" Core..=)
              Core.<$> automatedEncodingSettings,
            ("outputGroupSettings" Core..=)
              Core.<$> outputGroupSettings,
            ("name" Core..=) Core.<$> name,
            ("customName" Core..=) Core.<$> customName
          ]
      )
