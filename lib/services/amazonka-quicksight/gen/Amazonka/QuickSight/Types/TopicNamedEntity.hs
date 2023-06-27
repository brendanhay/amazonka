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
-- Module      : Amazonka.QuickSight.Types.TopicNamedEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicNamedEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamedEntityDefinition
import Amazonka.QuickSight.Types.SemanticEntityType

-- | A structure that represents a named entity.
--
-- /See:/ 'newTopicNamedEntity' smart constructor.
data TopicNamedEntity = TopicNamedEntity'
  { -- | The definition of a named entity.
    definition :: Prelude.Maybe [NamedEntityDefinition],
    -- | The description of the named entity.
    entityDescription :: Prelude.Maybe Prelude.Text,
    -- | The other names or aliases for the named entity.
    entitySynonyms :: Prelude.Maybe [Prelude.Text],
    -- | The type of named entity that a topic represents.
    semanticEntityType :: Prelude.Maybe SemanticEntityType,
    -- | The name of the named entity.
    entityName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicNamedEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'topicNamedEntity_definition' - The definition of a named entity.
--
-- 'entityDescription', 'topicNamedEntity_entityDescription' - The description of the named entity.
--
-- 'entitySynonyms', 'topicNamedEntity_entitySynonyms' - The other names or aliases for the named entity.
--
-- 'semanticEntityType', 'topicNamedEntity_semanticEntityType' - The type of named entity that a topic represents.
--
-- 'entityName', 'topicNamedEntity_entityName' - The name of the named entity.
newTopicNamedEntity ::
  -- | 'entityName'
  Prelude.Text ->
  TopicNamedEntity
newTopicNamedEntity pEntityName_ =
  TopicNamedEntity'
    { definition = Prelude.Nothing,
      entityDescription = Prelude.Nothing,
      entitySynonyms = Prelude.Nothing,
      semanticEntityType = Prelude.Nothing,
      entityName = pEntityName_
    }

-- | The definition of a named entity.
topicNamedEntity_definition :: Lens.Lens' TopicNamedEntity (Prelude.Maybe [NamedEntityDefinition])
topicNamedEntity_definition = Lens.lens (\TopicNamedEntity' {definition} -> definition) (\s@TopicNamedEntity' {} a -> s {definition = a} :: TopicNamedEntity) Prelude.. Lens.mapping Lens.coerced

-- | The description of the named entity.
topicNamedEntity_entityDescription :: Lens.Lens' TopicNamedEntity (Prelude.Maybe Prelude.Text)
topicNamedEntity_entityDescription = Lens.lens (\TopicNamedEntity' {entityDescription} -> entityDescription) (\s@TopicNamedEntity' {} a -> s {entityDescription = a} :: TopicNamedEntity)

-- | The other names or aliases for the named entity.
topicNamedEntity_entitySynonyms :: Lens.Lens' TopicNamedEntity (Prelude.Maybe [Prelude.Text])
topicNamedEntity_entitySynonyms = Lens.lens (\TopicNamedEntity' {entitySynonyms} -> entitySynonyms) (\s@TopicNamedEntity' {} a -> s {entitySynonyms = a} :: TopicNamedEntity) Prelude.. Lens.mapping Lens.coerced

-- | The type of named entity that a topic represents.
topicNamedEntity_semanticEntityType :: Lens.Lens' TopicNamedEntity (Prelude.Maybe SemanticEntityType)
topicNamedEntity_semanticEntityType = Lens.lens (\TopicNamedEntity' {semanticEntityType} -> semanticEntityType) (\s@TopicNamedEntity' {} a -> s {semanticEntityType = a} :: TopicNamedEntity)

-- | The name of the named entity.
topicNamedEntity_entityName :: Lens.Lens' TopicNamedEntity Prelude.Text
topicNamedEntity_entityName = Lens.lens (\TopicNamedEntity' {entityName} -> entityName) (\s@TopicNamedEntity' {} a -> s {entityName = a} :: TopicNamedEntity)

instance Data.FromJSON TopicNamedEntity where
  parseJSON =
    Data.withObject
      "TopicNamedEntity"
      ( \x ->
          TopicNamedEntity'
            Prelude.<$> (x Data..:? "Definition" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EntityDescription")
            Prelude.<*> (x Data..:? "EntitySynonyms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SemanticEntityType")
            Prelude.<*> (x Data..: "EntityName")
      )

instance Prelude.Hashable TopicNamedEntity where
  hashWithSalt _salt TopicNamedEntity' {..} =
    _salt
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` entityDescription
      `Prelude.hashWithSalt` entitySynonyms
      `Prelude.hashWithSalt` semanticEntityType
      `Prelude.hashWithSalt` entityName

instance Prelude.NFData TopicNamedEntity where
  rnf TopicNamedEntity' {..} =
    Prelude.rnf definition
      `Prelude.seq` Prelude.rnf entityDescription
      `Prelude.seq` Prelude.rnf entitySynonyms
      `Prelude.seq` Prelude.rnf semanticEntityType
      `Prelude.seq` Prelude.rnf entityName

instance Data.ToJSON TopicNamedEntity where
  toJSON TopicNamedEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Definition" Data..=) Prelude.<$> definition,
            ("EntityDescription" Data..=)
              Prelude.<$> entityDescription,
            ("EntitySynonyms" Data..=)
              Prelude.<$> entitySynonyms,
            ("SemanticEntityType" Data..=)
              Prelude.<$> semanticEntityType,
            Prelude.Just ("EntityName" Data..= entityName)
          ]
      )
