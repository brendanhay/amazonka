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
-- Module      : Amazonka.SageMaker.Types.LabelingJobDataAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobDataAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ContentClassifier

-- | Attributes of the data specified by the customer. Use these to describe
-- the data to be labeled.
--
-- /See:/ 'newLabelingJobDataAttributes' smart constructor.
data LabelingJobDataAttributes = LabelingJobDataAttributes'
  { -- | Declares that your content is free of personally identifiable
    -- information or adult content. SageMaker may restrict the Amazon
    -- Mechanical Turk workers that can view your task based on this
    -- information.
    contentClassifiers :: Prelude.Maybe [ContentClassifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobDataAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentClassifiers', 'labelingJobDataAttributes_contentClassifiers' - Declares that your content is free of personally identifiable
-- information or adult content. SageMaker may restrict the Amazon
-- Mechanical Turk workers that can view your task based on this
-- information.
newLabelingJobDataAttributes ::
  LabelingJobDataAttributes
newLabelingJobDataAttributes =
  LabelingJobDataAttributes'
    { contentClassifiers =
        Prelude.Nothing
    }

-- | Declares that your content is free of personally identifiable
-- information or adult content. SageMaker may restrict the Amazon
-- Mechanical Turk workers that can view your task based on this
-- information.
labelingJobDataAttributes_contentClassifiers :: Lens.Lens' LabelingJobDataAttributes (Prelude.Maybe [ContentClassifier])
labelingJobDataAttributes_contentClassifiers = Lens.lens (\LabelingJobDataAttributes' {contentClassifiers} -> contentClassifiers) (\s@LabelingJobDataAttributes' {} a -> s {contentClassifiers = a} :: LabelingJobDataAttributes) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LabelingJobDataAttributes where
  parseJSON =
    Core.withObject
      "LabelingJobDataAttributes"
      ( \x ->
          LabelingJobDataAttributes'
            Prelude.<$> ( x Core..:? "ContentClassifiers"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LabelingJobDataAttributes where
  hashWithSalt _salt LabelingJobDataAttributes' {..} =
    _salt `Prelude.hashWithSalt` contentClassifiers

instance Prelude.NFData LabelingJobDataAttributes where
  rnf LabelingJobDataAttributes' {..} =
    Prelude.rnf contentClassifiers

instance Core.ToJSON LabelingJobDataAttributes where
  toJSON LabelingJobDataAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContentClassifiers" Core..=)
              Prelude.<$> contentClassifiers
          ]
      )
