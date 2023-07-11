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
-- Module      : Amazonka.CodePipeline.Types.InputArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.InputArtifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an artifact to be worked on, such as a test
-- or build artifact.
--
-- /See:/ 'newInputArtifact' smart constructor.
data InputArtifact = InputArtifact'
  { -- | The name of the artifact to be worked on (for example, \"My App\").
    --
    -- The input artifact of an action must exactly match the output artifact
    -- declared in a preceding action, but the input artifact does not have to
    -- be the next action in strict sequence from the action that provided the
    -- output artifact. Actions in parallel can declare different output
    -- artifacts, which are in turn consumed by different following actions.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inputArtifact_name' - The name of the artifact to be worked on (for example, \"My App\").
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
newInputArtifact ::
  -- | 'name'
  Prelude.Text ->
  InputArtifact
newInputArtifact pName_ =
  InputArtifact' {name = pName_}

-- | The name of the artifact to be worked on (for example, \"My App\").
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
inputArtifact_name :: Lens.Lens' InputArtifact Prelude.Text
inputArtifact_name = Lens.lens (\InputArtifact' {name} -> name) (\s@InputArtifact' {} a -> s {name = a} :: InputArtifact)

instance Data.FromJSON InputArtifact where
  parseJSON =
    Data.withObject
      "InputArtifact"
      ( \x ->
          InputArtifact' Prelude.<$> (x Data..: "name")
      )

instance Prelude.Hashable InputArtifact where
  hashWithSalt _salt InputArtifact' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData InputArtifact where
  rnf InputArtifact' {..} = Prelude.rnf name

instance Data.ToJSON InputArtifact where
  toJSON InputArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )
