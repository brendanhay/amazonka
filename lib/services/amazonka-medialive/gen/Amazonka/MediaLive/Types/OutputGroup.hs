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
-- Module      : Amazonka.MediaLive.Types.OutputGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Output
import Amazonka.MediaLive.Types.OutputGroupSettings
import qualified Amazonka.Prelude as Prelude

-- | Output groups for this Live Event. Output groups contain information
-- about where streams should be distributed.
--
-- /See:/ 'newOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | Custom output group name optionally defined by the user.
    name :: Prelude.Maybe Prelude.Text,
    outputs :: [Output],
    -- | Settings associated with the output group.
    outputGroupSettings :: OutputGroupSettings
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
-- 'name', 'outputGroup_name' - Custom output group name optionally defined by the user.
--
-- 'outputs', 'outputGroup_outputs' - Undocumented member.
--
-- 'outputGroupSettings', 'outputGroup_outputGroupSettings' - Settings associated with the output group.
newOutputGroup ::
  -- | 'outputGroupSettings'
  OutputGroupSettings ->
  OutputGroup
newOutputGroup pOutputGroupSettings_ =
  OutputGroup'
    { name = Prelude.Nothing,
      outputs = Prelude.mempty,
      outputGroupSettings = pOutputGroupSettings_
    }

-- | Custom output group name optionally defined by the user.
outputGroup_name :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_name = Lens.lens (\OutputGroup' {name} -> name) (\s@OutputGroup' {} a -> s {name = a} :: OutputGroup)

-- | Undocumented member.
outputGroup_outputs :: Lens.Lens' OutputGroup [Output]
outputGroup_outputs = Lens.lens (\OutputGroup' {outputs} -> outputs) (\s@OutputGroup' {} a -> s {outputs = a} :: OutputGroup) Prelude.. Lens.coerced

-- | Settings associated with the output group.
outputGroup_outputGroupSettings :: Lens.Lens' OutputGroup OutputGroupSettings
outputGroup_outputGroupSettings = Lens.lens (\OutputGroup' {outputGroupSettings} -> outputGroupSettings) (\s@OutputGroup' {} a -> s {outputGroupSettings = a} :: OutputGroup)

instance Data.FromJSON OutputGroup where
  parseJSON =
    Data.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "outputGroupSettings")
      )

instance Prelude.Hashable OutputGroup where
  hashWithSalt _salt OutputGroup' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` outputGroupSettings

instance Prelude.NFData OutputGroup where
  rnf OutputGroup' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf outputs `Prelude.seq`
        Prelude.rnf outputGroupSettings

instance Data.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("outputs" Data..= outputs),
            Prelude.Just
              ("outputGroupSettings" Data..= outputGroupSettings)
          ]
      )
