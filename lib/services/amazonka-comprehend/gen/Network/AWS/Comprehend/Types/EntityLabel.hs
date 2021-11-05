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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityLabel where

import Amazonka.Comprehend.Types.PiiEntityType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies one of the label or labels that categorize the personally
-- identifiable information (PII) entity being analyzed.
--
-- /See:/ 'newEntityLabel' smart constructor.
data EntityLabel = EntityLabel'
  { -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | The name of the label.
    name :: Prelude.Maybe PiiEntityType
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
-- 'score', 'entityLabel_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'name', 'entityLabel_name' - The name of the label.
newEntityLabel ::
  EntityLabel
newEntityLabel =
  EntityLabel'
    { score = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
entityLabel_score :: Lens.Lens' EntityLabel (Prelude.Maybe Prelude.Double)
entityLabel_score = Lens.lens (\EntityLabel' {score} -> score) (\s@EntityLabel' {} a -> s {score = a} :: EntityLabel)

-- | The name of the label.
entityLabel_name :: Lens.Lens' EntityLabel (Prelude.Maybe PiiEntityType)
entityLabel_name = Lens.lens (\EntityLabel' {name} -> name) (\s@EntityLabel' {} a -> s {name = a} :: EntityLabel)

instance Core.FromJSON EntityLabel where
  parseJSON =
    Core.withObject
      "EntityLabel"
      ( \x ->
          EntityLabel'
            Prelude.<$> (x Core..:? "Score") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable EntityLabel

instance Prelude.NFData EntityLabel
