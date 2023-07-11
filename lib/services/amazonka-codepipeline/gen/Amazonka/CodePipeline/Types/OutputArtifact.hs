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
-- Module      : Amazonka.CodePipeline.Types.OutputArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.OutputArtifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the output of an action.
--
-- /See:/ 'newOutputArtifact' smart constructor.
data OutputArtifact = OutputArtifact'
  { -- | The name of the output of an artifact, such as \"My App\".
    --
    -- The input artifact of an action must exactly match the output artifact
    -- declared in a preceding action, but the input artifact does not have to
    -- be the next action in strict sequence from the action that provided the
    -- output artifact. Actions in parallel can declare different output
    -- artifacts, which are in turn consumed by different following actions.
    --
    -- Output artifact names must be unique within a pipeline.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'outputArtifact_name' - The name of the output of an artifact, such as \"My App\".
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
--
-- Output artifact names must be unique within a pipeline.
newOutputArtifact ::
  -- | 'name'
  Prelude.Text ->
  OutputArtifact
newOutputArtifact pName_ =
  OutputArtifact' {name = pName_}

-- | The name of the output of an artifact, such as \"My App\".
--
-- The input artifact of an action must exactly match the output artifact
-- declared in a preceding action, but the input artifact does not have to
-- be the next action in strict sequence from the action that provided the
-- output artifact. Actions in parallel can declare different output
-- artifacts, which are in turn consumed by different following actions.
--
-- Output artifact names must be unique within a pipeline.
outputArtifact_name :: Lens.Lens' OutputArtifact Prelude.Text
outputArtifact_name = Lens.lens (\OutputArtifact' {name} -> name) (\s@OutputArtifact' {} a -> s {name = a} :: OutputArtifact)

instance Data.FromJSON OutputArtifact where
  parseJSON =
    Data.withObject
      "OutputArtifact"
      ( \x ->
          OutputArtifact' Prelude.<$> (x Data..: "name")
      )

instance Prelude.Hashable OutputArtifact where
  hashWithSalt _salt OutputArtifact' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData OutputArtifact where
  rnf OutputArtifact' {..} = Prelude.rnf name

instance Data.ToJSON OutputArtifact where
  toJSON OutputArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )
