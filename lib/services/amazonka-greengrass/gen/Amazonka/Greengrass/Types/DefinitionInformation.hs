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
-- Module      : Amazonka.Greengrass.Types.DefinitionInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.DefinitionInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a definition.
--
-- /See:/ 'newDefinitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefinitionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'definitionInformation_tags' - Tag(s) attached to the resource arn.
--
-- 'lastUpdatedTimestamp', 'definitionInformation_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'name', 'definitionInformation_name' - The name of the definition.
--
-- 'arn', 'definitionInformation_arn' - The ARN of the definition.
--
-- 'latestVersion', 'definitionInformation_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'definitionInformation_id' - The ID of the definition.
--
-- 'creationTimestamp', 'definitionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'definitionInformation_latestVersionArn' - The ARN of the latest version associated with the definition.
newDefinitionInformation ::
  DefinitionInformation
newDefinitionInformation =
  DefinitionInformation'
    { tags = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing
    }

-- | Tag(s) attached to the resource arn.
definitionInformation_tags :: Lens.Lens' DefinitionInformation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
definitionInformation_tags = Lens.lens (\DefinitionInformation' {tags} -> tags) (\s@DefinitionInformation' {} a -> s {tags = a} :: DefinitionInformation) Prelude.. Lens.mapping Lens.coerced

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
definitionInformation_lastUpdatedTimestamp :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_lastUpdatedTimestamp = Lens.lens (\DefinitionInformation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DefinitionInformation' {} a -> s {lastUpdatedTimestamp = a} :: DefinitionInformation)

-- | The name of the definition.
definitionInformation_name :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_name = Lens.lens (\DefinitionInformation' {name} -> name) (\s@DefinitionInformation' {} a -> s {name = a} :: DefinitionInformation)

-- | The ARN of the definition.
definitionInformation_arn :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_arn = Lens.lens (\DefinitionInformation' {arn} -> arn) (\s@DefinitionInformation' {} a -> s {arn = a} :: DefinitionInformation)

-- | The ID of the latest version associated with the definition.
definitionInformation_latestVersion :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_latestVersion = Lens.lens (\DefinitionInformation' {latestVersion} -> latestVersion) (\s@DefinitionInformation' {} a -> s {latestVersion = a} :: DefinitionInformation)

-- | The ID of the definition.
definitionInformation_id :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_id = Lens.lens (\DefinitionInformation' {id} -> id) (\s@DefinitionInformation' {} a -> s {id = a} :: DefinitionInformation)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
definitionInformation_creationTimestamp :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_creationTimestamp = Lens.lens (\DefinitionInformation' {creationTimestamp} -> creationTimestamp) (\s@DefinitionInformation' {} a -> s {creationTimestamp = a} :: DefinitionInformation)

-- | The ARN of the latest version associated with the definition.
definitionInformation_latestVersionArn :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_latestVersionArn = Lens.lens (\DefinitionInformation' {latestVersionArn} -> latestVersionArn) (\s@DefinitionInformation' {} a -> s {latestVersionArn = a} :: DefinitionInformation)

instance Core.FromJSON DefinitionInformation where
  parseJSON =
    Core.withObject
      "DefinitionInformation"
      ( \x ->
          DefinitionInformation'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "LatestVersion")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "LatestVersionArn")
      )

instance Prelude.Hashable DefinitionInformation where
  hashWithSalt _salt DefinitionInformation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` latestVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` latestVersionArn

instance Prelude.NFData DefinitionInformation where
  rnf DefinitionInformation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
