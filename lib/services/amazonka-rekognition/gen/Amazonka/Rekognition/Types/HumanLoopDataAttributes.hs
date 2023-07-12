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
-- Module      : Amazonka.Rekognition.Types.HumanLoopDataAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.HumanLoopDataAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ContentClassifier

-- | Allows you to set attributes of the image. Currently, you can declare an
-- image as free of personally identifiable information.
--
-- /See:/ 'newHumanLoopDataAttributes' smart constructor.
data HumanLoopDataAttributes = HumanLoopDataAttributes'
  { -- | Sets whether the input image is free of personally identifiable
    -- information.
    contentClassifiers :: Prelude.Maybe [ContentClassifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopDataAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentClassifiers', 'humanLoopDataAttributes_contentClassifiers' - Sets whether the input image is free of personally identifiable
-- information.
newHumanLoopDataAttributes ::
  HumanLoopDataAttributes
newHumanLoopDataAttributes =
  HumanLoopDataAttributes'
    { contentClassifiers =
        Prelude.Nothing
    }

-- | Sets whether the input image is free of personally identifiable
-- information.
humanLoopDataAttributes_contentClassifiers :: Lens.Lens' HumanLoopDataAttributes (Prelude.Maybe [ContentClassifier])
humanLoopDataAttributes_contentClassifiers = Lens.lens (\HumanLoopDataAttributes' {contentClassifiers} -> contentClassifiers) (\s@HumanLoopDataAttributes' {} a -> s {contentClassifiers = a} :: HumanLoopDataAttributes) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable HumanLoopDataAttributes where
  hashWithSalt _salt HumanLoopDataAttributes' {..} =
    _salt `Prelude.hashWithSalt` contentClassifiers

instance Prelude.NFData HumanLoopDataAttributes where
  rnf HumanLoopDataAttributes' {..} =
    Prelude.rnf contentClassifiers

instance Data.ToJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentClassifiers" Data..=)
              Prelude.<$> contentClassifiers
          ]
      )
