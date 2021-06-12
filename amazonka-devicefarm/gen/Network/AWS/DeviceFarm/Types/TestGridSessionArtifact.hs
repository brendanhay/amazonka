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
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifact where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
import qualified Network.AWS.Lens as Lens

-- | Artifacts are video and other files that are produced in the process of
-- running a browser in an automated context.
--
-- Video elements might be broken up into multiple artifacts as they grow
-- in size during creation.
--
-- /See:/ 'newTestGridSessionArtifact' smart constructor.
data TestGridSessionArtifact = TestGridSessionArtifact'
  { -- | The file name of the artifact.
    filename :: Core.Maybe Core.Text,
    -- | A semi-stable URL to the content of the object.
    url :: Core.Maybe Core.Text,
    -- | The kind of artifact.
    type' :: Core.Maybe TestGridSessionArtifactType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestGridSessionArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filename', 'testGridSessionArtifact_filename' - The file name of the artifact.
--
-- 'url', 'testGridSessionArtifact_url' - A semi-stable URL to the content of the object.
--
-- 'type'', 'testGridSessionArtifact_type' - The kind of artifact.
newTestGridSessionArtifact ::
  TestGridSessionArtifact
newTestGridSessionArtifact =
  TestGridSessionArtifact'
    { filename = Core.Nothing,
      url = Core.Nothing,
      type' = Core.Nothing
    }

-- | The file name of the artifact.
testGridSessionArtifact_filename :: Lens.Lens' TestGridSessionArtifact (Core.Maybe Core.Text)
testGridSessionArtifact_filename = Lens.lens (\TestGridSessionArtifact' {filename} -> filename) (\s@TestGridSessionArtifact' {} a -> s {filename = a} :: TestGridSessionArtifact)

-- | A semi-stable URL to the content of the object.
testGridSessionArtifact_url :: Lens.Lens' TestGridSessionArtifact (Core.Maybe Core.Text)
testGridSessionArtifact_url = Lens.lens (\TestGridSessionArtifact' {url} -> url) (\s@TestGridSessionArtifact' {} a -> s {url = a} :: TestGridSessionArtifact)

-- | The kind of artifact.
testGridSessionArtifact_type :: Lens.Lens' TestGridSessionArtifact (Core.Maybe TestGridSessionArtifactType)
testGridSessionArtifact_type = Lens.lens (\TestGridSessionArtifact' {type'} -> type') (\s@TestGridSessionArtifact' {} a -> s {type' = a} :: TestGridSessionArtifact)

instance Core.FromJSON TestGridSessionArtifact where
  parseJSON =
    Core.withObject
      "TestGridSessionArtifact"
      ( \x ->
          TestGridSessionArtifact'
            Core.<$> (x Core..:? "filename")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable TestGridSessionArtifact

instance Core.NFData TestGridSessionArtifact
