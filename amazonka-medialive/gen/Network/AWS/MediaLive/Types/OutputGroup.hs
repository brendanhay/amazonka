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
-- Module      : Network.AWS.MediaLive.Types.OutputGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroup where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Output
import Network.AWS.MediaLive.Types.OutputGroupSettings
import qualified Network.AWS.Prelude as Prelude

-- | Output groups for this Live Event. Output groups contain information
-- about where streams should be distributed.
--
-- /See:/ 'newOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | Custom output group name optionally defined by the user. Only letters,
    -- numbers, and the underscore character allowed; only 32 characters
    -- allowed.
    name :: Prelude.Maybe Prelude.Text,
    outputs :: [Output],
    -- | Settings associated with the output group.
    outputGroupSettings :: OutputGroupSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'outputGroup_name' - Custom output group name optionally defined by the user. Only letters,
-- numbers, and the underscore character allowed; only 32 characters
-- allowed.
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

-- | Custom output group name optionally defined by the user. Only letters,
-- numbers, and the underscore character allowed; only 32 characters
-- allowed.
outputGroup_name :: Lens.Lens' OutputGroup (Prelude.Maybe Prelude.Text)
outputGroup_name = Lens.lens (\OutputGroup' {name} -> name) (\s@OutputGroup' {} a -> s {name = a} :: OutputGroup)

-- | Undocumented member.
outputGroup_outputs :: Lens.Lens' OutputGroup [Output]
outputGroup_outputs = Lens.lens (\OutputGroup' {outputs} -> outputs) (\s@OutputGroup' {} a -> s {outputs = a} :: OutputGroup) Prelude.. Prelude._Coerce

-- | Settings associated with the output group.
outputGroup_outputGroupSettings :: Lens.Lens' OutputGroup OutputGroupSettings
outputGroup_outputGroupSettings = Lens.lens (\OutputGroup' {outputGroupSettings} -> outputGroupSettings) (\s@OutputGroup' {} a -> s {outputGroupSettings = a} :: OutputGroup)

instance Prelude.FromJSON OutputGroup where
  parseJSON =
    Prelude.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Prelude.<$> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "outputs" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "outputGroupSettings")
      )

instance Prelude.Hashable OutputGroup

instance Prelude.NFData OutputGroup

instance Prelude.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("name" Prelude..=) Prelude.<$> name,
            Prelude.Just ("outputs" Prelude..= outputs),
            Prelude.Just
              ( "outputGroupSettings"
                  Prelude..= outputGroupSettings
              )
          ]
      )
