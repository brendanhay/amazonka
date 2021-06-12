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
-- Module      : Network.AWS.Greengrass.Types.DefinitionInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DefinitionInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a definition.
--
-- /See:/ 'newDefinitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefinitionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'definitionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'definitionInformation_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'definitionInformation_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'definitionInformation_arn' - The ARN of the definition.
--
-- 'id', 'definitionInformation_id' - The ID of the definition.
--
-- 'name', 'definitionInformation_name' - The name of the definition.
--
-- 'tags', 'definitionInformation_tags' - Tag(s) attached to the resource arn.
--
-- 'lastUpdatedTimestamp', 'definitionInformation_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
newDefinitionInformation ::
  DefinitionInformation
newDefinitionInformation =
  DefinitionInformation'
    { creationTimestamp =
        Core.Nothing,
      latestVersionArn = Core.Nothing,
      latestVersion = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
definitionInformation_creationTimestamp :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_creationTimestamp = Lens.lens (\DefinitionInformation' {creationTimestamp} -> creationTimestamp) (\s@DefinitionInformation' {} a -> s {creationTimestamp = a} :: DefinitionInformation)

-- | The ARN of the latest version associated with the definition.
definitionInformation_latestVersionArn :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_latestVersionArn = Lens.lens (\DefinitionInformation' {latestVersionArn} -> latestVersionArn) (\s@DefinitionInformation' {} a -> s {latestVersionArn = a} :: DefinitionInformation)

-- | The ID of the latest version associated with the definition.
definitionInformation_latestVersion :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_latestVersion = Lens.lens (\DefinitionInformation' {latestVersion} -> latestVersion) (\s@DefinitionInformation' {} a -> s {latestVersion = a} :: DefinitionInformation)

-- | The ARN of the definition.
definitionInformation_arn :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_arn = Lens.lens (\DefinitionInformation' {arn} -> arn) (\s@DefinitionInformation' {} a -> s {arn = a} :: DefinitionInformation)

-- | The ID of the definition.
definitionInformation_id :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_id = Lens.lens (\DefinitionInformation' {id} -> id) (\s@DefinitionInformation' {} a -> s {id = a} :: DefinitionInformation)

-- | The name of the definition.
definitionInformation_name :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_name = Lens.lens (\DefinitionInformation' {name} -> name) (\s@DefinitionInformation' {} a -> s {name = a} :: DefinitionInformation)

-- | Tag(s) attached to the resource arn.
definitionInformation_tags :: Lens.Lens' DefinitionInformation (Core.Maybe (Core.HashMap Core.Text Core.Text))
definitionInformation_tags = Lens.lens (\DefinitionInformation' {tags} -> tags) (\s@DefinitionInformation' {} a -> s {tags = a} :: DefinitionInformation) Core.. Lens.mapping Lens._Coerce

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
definitionInformation_lastUpdatedTimestamp :: Lens.Lens' DefinitionInformation (Core.Maybe Core.Text)
definitionInformation_lastUpdatedTimestamp = Lens.lens (\DefinitionInformation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DefinitionInformation' {} a -> s {lastUpdatedTimestamp = a} :: DefinitionInformation)

instance Core.FromJSON DefinitionInformation where
  parseJSON =
    Core.withObject
      "DefinitionInformation"
      ( \x ->
          DefinitionInformation'
            Core.<$> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
      )

instance Core.Hashable DefinitionInformation

instance Core.NFData DefinitionInformation
