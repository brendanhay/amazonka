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
-- Module      : Network.AWS.CodePipeline.Types.InputArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.InputArtifact where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON InputArtifact where
  parseJSON =
    Prelude.withObject
      "InputArtifact"
      ( \x ->
          InputArtifact' Prelude.<$> (x Prelude..: "name")
      )

instance Prelude.Hashable InputArtifact

instance Prelude.NFData InputArtifact

instance Prelude.ToJSON InputArtifact where
  toJSON InputArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )
