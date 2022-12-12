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
-- Module      : Amazonka.ImageBuilder.Types.ComponentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ComponentState
import Amazonka.ImageBuilder.Types.ComponentType
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Prelude as Prelude

-- | A high-level summary of a component.
--
-- /See:/ 'newComponentSummary' smart constructor.
data ComponentSummary = ComponentSummary'
  { -- | The Amazon Resource Name (ARN) of the component.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The change description of the component.
    changeDescription :: Prelude.Maybe Prelude.Text,
    -- | The date that the component was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The description of the component.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the component.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The platform of the component.
    platform :: Prelude.Maybe Platform,
    -- | Describes the current status of the component.
    state :: Prelude.Maybe ComponentState,
    -- | The operating system (OS) version supported by the component. If the OS
    -- information is available, a prefix match is performed against the base
    -- image OS version during image recipe creation.
    supportedOsVersions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The tags associated with the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the component denotes whether the component is used to build
    -- the image or only to test it.
    type' :: Prelude.Maybe ComponentType,
    -- | The version of the component.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'componentSummary_arn' - The Amazon Resource Name (ARN) of the component.
--
-- 'changeDescription', 'componentSummary_changeDescription' - The change description of the component.
--
-- 'dateCreated', 'componentSummary_dateCreated' - The date that the component was created.
--
-- 'description', 'componentSummary_description' - The description of the component.
--
-- 'name', 'componentSummary_name' - The name of the component.
--
-- 'owner', 'componentSummary_owner' - The owner of the component.
--
-- 'platform', 'componentSummary_platform' - The platform of the component.
--
-- 'state', 'componentSummary_state' - Describes the current status of the component.
--
-- 'supportedOsVersions', 'componentSummary_supportedOsVersions' - The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
--
-- 'tags', 'componentSummary_tags' - The tags associated with the component.
--
-- 'type'', 'componentSummary_type' - The type of the component denotes whether the component is used to build
-- the image or only to test it.
--
-- 'version', 'componentSummary_version' - The version of the component.
newComponentSummary ::
  ComponentSummary
newComponentSummary =
  ComponentSummary'
    { arn = Prelude.Nothing,
      changeDescription = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      platform = Prelude.Nothing,
      state = Prelude.Nothing,
      supportedOsVersions = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the component.
componentSummary_arn :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_arn = Lens.lens (\ComponentSummary' {arn} -> arn) (\s@ComponentSummary' {} a -> s {arn = a} :: ComponentSummary)

-- | The change description of the component.
componentSummary_changeDescription :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_changeDescription = Lens.lens (\ComponentSummary' {changeDescription} -> changeDescription) (\s@ComponentSummary' {} a -> s {changeDescription = a} :: ComponentSummary)

-- | The date that the component was created.
componentSummary_dateCreated :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_dateCreated = Lens.lens (\ComponentSummary' {dateCreated} -> dateCreated) (\s@ComponentSummary' {} a -> s {dateCreated = a} :: ComponentSummary)

-- | The description of the component.
componentSummary_description :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_description = Lens.lens (\ComponentSummary' {description} -> description) (\s@ComponentSummary' {} a -> s {description = a} :: ComponentSummary)

-- | The name of the component.
componentSummary_name :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_name = Lens.lens (\ComponentSummary' {name} -> name) (\s@ComponentSummary' {} a -> s {name = a} :: ComponentSummary)

-- | The owner of the component.
componentSummary_owner :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_owner = Lens.lens (\ComponentSummary' {owner} -> owner) (\s@ComponentSummary' {} a -> s {owner = a} :: ComponentSummary)

-- | The platform of the component.
componentSummary_platform :: Lens.Lens' ComponentSummary (Prelude.Maybe Platform)
componentSummary_platform = Lens.lens (\ComponentSummary' {platform} -> platform) (\s@ComponentSummary' {} a -> s {platform = a} :: ComponentSummary)

-- | Describes the current status of the component.
componentSummary_state :: Lens.Lens' ComponentSummary (Prelude.Maybe ComponentState)
componentSummary_state = Lens.lens (\ComponentSummary' {state} -> state) (\s@ComponentSummary' {} a -> s {state = a} :: ComponentSummary)

-- | The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
componentSummary_supportedOsVersions :: Lens.Lens' ComponentSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
componentSummary_supportedOsVersions = Lens.lens (\ComponentSummary' {supportedOsVersions} -> supportedOsVersions) (\s@ComponentSummary' {} a -> s {supportedOsVersions = a} :: ComponentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the component.
componentSummary_tags :: Lens.Lens' ComponentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentSummary_tags = Lens.lens (\ComponentSummary' {tags} -> tags) (\s@ComponentSummary' {} a -> s {tags = a} :: ComponentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The type of the component denotes whether the component is used to build
-- the image or only to test it.
componentSummary_type :: Lens.Lens' ComponentSummary (Prelude.Maybe ComponentType)
componentSummary_type = Lens.lens (\ComponentSummary' {type'} -> type') (\s@ComponentSummary' {} a -> s {type' = a} :: ComponentSummary)

-- | The version of the component.
componentSummary_version :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_version = Lens.lens (\ComponentSummary' {version} -> version) (\s@ComponentSummary' {} a -> s {version = a} :: ComponentSummary)

instance Data.FromJSON ComponentSummary where
  parseJSON =
    Data.withObject
      "ComponentSummary"
      ( \x ->
          ComponentSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "changeDescription")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "supportedOsVersions")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable ComponentSummary where
  hashWithSalt _salt ComponentSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` changeDescription
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` supportedOsVersions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData ComponentSummary where
  rnf ComponentSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf changeDescription
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf supportedOsVersions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
