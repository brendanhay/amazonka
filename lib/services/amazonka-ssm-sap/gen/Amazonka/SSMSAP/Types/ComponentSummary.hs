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
-- Module      : Amazonka.SSMSAP.Types.ComponentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.ComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.ComponentType

-- | The summary of the component.
--
-- /See:/ 'newComponentSummary' smart constructor.
data ComponentSummary = ComponentSummary'
  { -- | The ID of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The type of the component.
    componentType :: Prelude.Maybe ComponentType,
    -- | The tags of the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'applicationId', 'componentSummary_applicationId' - The ID of the application.
--
-- 'componentId', 'componentSummary_componentId' - The ID of the component.
--
-- 'componentType', 'componentSummary_componentType' - The type of the component.
--
-- 'tags', 'componentSummary_tags' - The tags of the component.
newComponentSummary ::
  ComponentSummary
newComponentSummary =
  ComponentSummary'
    { applicationId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      componentType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the application.
componentSummary_applicationId :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_applicationId = Lens.lens (\ComponentSummary' {applicationId} -> applicationId) (\s@ComponentSummary' {} a -> s {applicationId = a} :: ComponentSummary)

-- | The ID of the component.
componentSummary_componentId :: Lens.Lens' ComponentSummary (Prelude.Maybe Prelude.Text)
componentSummary_componentId = Lens.lens (\ComponentSummary' {componentId} -> componentId) (\s@ComponentSummary' {} a -> s {componentId = a} :: ComponentSummary)

-- | The type of the component.
componentSummary_componentType :: Lens.Lens' ComponentSummary (Prelude.Maybe ComponentType)
componentSummary_componentType = Lens.lens (\ComponentSummary' {componentType} -> componentType) (\s@ComponentSummary' {} a -> s {componentType = a} :: ComponentSummary)

-- | The tags of the component.
componentSummary_tags :: Lens.Lens' ComponentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentSummary_tags = Lens.lens (\ComponentSummary' {tags} -> tags) (\s@ComponentSummary' {} a -> s {tags = a} :: ComponentSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ComponentSummary where
  parseJSON =
    Data.withObject
      "ComponentSummary"
      ( \x ->
          ComponentSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "ComponentId")
            Prelude.<*> (x Data..:? "ComponentType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ComponentSummary where
  hashWithSalt _salt ComponentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ComponentSummary where
  rnf ComponentSummary' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf tags
