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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a definition.
--
-- /See:/ 'newDefinitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'arn', 'definitionInformation_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'definitionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'definitionInformation_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'definitionInformation_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'definitionInformation_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'definitionInformation_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'definitionInformation_name' - The name of the definition.
--
-- 'tags', 'definitionInformation_tags' - Tag(s) attached to the resource arn.
newDefinitionInformation ::
  DefinitionInformation
newDefinitionInformation =
  DefinitionInformation'
    { arn = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the definition.
definitionInformation_arn :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_arn = Lens.lens (\DefinitionInformation' {arn} -> arn) (\s@DefinitionInformation' {} a -> s {arn = a} :: DefinitionInformation)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
definitionInformation_creationTimestamp :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_creationTimestamp = Lens.lens (\DefinitionInformation' {creationTimestamp} -> creationTimestamp) (\s@DefinitionInformation' {} a -> s {creationTimestamp = a} :: DefinitionInformation)

-- | The ID of the definition.
definitionInformation_id :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_id = Lens.lens (\DefinitionInformation' {id} -> id) (\s@DefinitionInformation' {} a -> s {id = a} :: DefinitionInformation)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
definitionInformation_lastUpdatedTimestamp :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_lastUpdatedTimestamp = Lens.lens (\DefinitionInformation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DefinitionInformation' {} a -> s {lastUpdatedTimestamp = a} :: DefinitionInformation)

-- | The ID of the latest version associated with the definition.
definitionInformation_latestVersion :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_latestVersion = Lens.lens (\DefinitionInformation' {latestVersion} -> latestVersion) (\s@DefinitionInformation' {} a -> s {latestVersion = a} :: DefinitionInformation)

-- | The ARN of the latest version associated with the definition.
definitionInformation_latestVersionArn :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_latestVersionArn = Lens.lens (\DefinitionInformation' {latestVersionArn} -> latestVersionArn) (\s@DefinitionInformation' {} a -> s {latestVersionArn = a} :: DefinitionInformation)

-- | The name of the definition.
definitionInformation_name :: Lens.Lens' DefinitionInformation (Prelude.Maybe Prelude.Text)
definitionInformation_name = Lens.lens (\DefinitionInformation' {name} -> name) (\s@DefinitionInformation' {} a -> s {name = a} :: DefinitionInformation)

-- | Tag(s) attached to the resource arn.
definitionInformation_tags :: Lens.Lens' DefinitionInformation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
definitionInformation_tags = Lens.lens (\DefinitionInformation' {tags} -> tags) (\s@DefinitionInformation' {} a -> s {tags = a} :: DefinitionInformation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DefinitionInformation where
  parseJSON =
    Data.withObject
      "DefinitionInformation"
      ( \x ->
          DefinitionInformation'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "LatestVersion")
            Prelude.<*> (x Data..:? "LatestVersionArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DefinitionInformation where
  hashWithSalt _salt DefinitionInformation' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` latestVersion
      `Prelude.hashWithSalt` latestVersionArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DefinitionInformation where
  rnf DefinitionInformation' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
