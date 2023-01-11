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
-- Module      : Amazonka.Comprehend.Types.EntityLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityLabel where

import Amazonka.Comprehend.Types.PiiEntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies one of the label or labels that categorize the personally
-- identifiable information (PII) entity being analyzed.
--
-- /See:/ 'newEntityLabel' smart constructor.
data EntityLabel = EntityLabel'
  { -- | The name of the label.
    name :: Prelude.Maybe PiiEntityType,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'entityLabel_name' - The name of the label.
--
-- 'score', 'entityLabel_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
newEntityLabel ::
  EntityLabel
newEntityLabel =
  EntityLabel'
    { name = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The name of the label.
entityLabel_name :: Lens.Lens' EntityLabel (Prelude.Maybe PiiEntityType)
entityLabel_name = Lens.lens (\EntityLabel' {name} -> name) (\s@EntityLabel' {} a -> s {name = a} :: EntityLabel)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
entityLabel_score :: Lens.Lens' EntityLabel (Prelude.Maybe Prelude.Double)
entityLabel_score = Lens.lens (\EntityLabel' {score} -> score) (\s@EntityLabel' {} a -> s {score = a} :: EntityLabel)

instance Data.FromJSON EntityLabel where
  parseJSON =
    Data.withObject
      "EntityLabel"
      ( \x ->
          EntityLabel'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable EntityLabel where
  hashWithSalt _salt EntityLabel' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` score

instance Prelude.NFData EntityLabel where
  rnf EntityLabel' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf score
