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
-- Module      : Amazonka.Comprehend.Types.EntityRecognitionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognitionConfig where

import Amazonka.Comprehend.Types.EntityTypesListItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration required for an entity recognition model.
--
-- /See:/ 'newEntityRecognitionConfig' smart constructor.
data EntityRecognitionConfig = EntityRecognitionConfig'
  { -- | Up to 25 entity types that the model is trained to recognize.
    entityTypes :: [EntityTypesListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognitionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityTypes', 'entityRecognitionConfig_entityTypes' - Up to 25 entity types that the model is trained to recognize.
newEntityRecognitionConfig ::
  EntityRecognitionConfig
newEntityRecognitionConfig =
  EntityRecognitionConfig'
    { entityTypes =
        Prelude.mempty
    }

-- | Up to 25 entity types that the model is trained to recognize.
entityRecognitionConfig_entityTypes :: Lens.Lens' EntityRecognitionConfig [EntityTypesListItem]
entityRecognitionConfig_entityTypes = Lens.lens (\EntityRecognitionConfig' {entityTypes} -> entityTypes) (\s@EntityRecognitionConfig' {} a -> s {entityTypes = a} :: EntityRecognitionConfig) Prelude.. Lens.coerced

instance Data.FromJSON EntityRecognitionConfig where
  parseJSON =
    Data.withObject
      "EntityRecognitionConfig"
      ( \x ->
          EntityRecognitionConfig'
            Prelude.<$> (x Data..:? "EntityTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EntityRecognitionConfig where
  hashWithSalt _salt EntityRecognitionConfig' {..} =
    _salt `Prelude.hashWithSalt` entityTypes

instance Prelude.NFData EntityRecognitionConfig where
  rnf EntityRecognitionConfig' {..} =
    Prelude.rnf entityTypes

instance Data.ToJSON EntityRecognitionConfig where
  toJSON EntityRecognitionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EntityTypes" Data..= entityTypes)]
      )
