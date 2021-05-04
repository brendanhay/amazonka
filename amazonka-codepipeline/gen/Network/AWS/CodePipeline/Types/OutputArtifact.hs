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
-- Module      : Network.AWS.CodePipeline.Types.OutputArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.OutputArtifact where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON OutputArtifact where
  parseJSON =
    Prelude.withObject
      "OutputArtifact"
      ( \x ->
          OutputArtifact' Prelude.<$> (x Prelude..: "name")
      )

instance Prelude.Hashable OutputArtifact

instance Prelude.NFData OutputArtifact

instance Prelude.ToJSON OutputArtifact where
  toJSON OutputArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )
