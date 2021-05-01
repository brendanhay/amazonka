{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ContentClassifier

-- | Attributes of the data specified by the customer. Use these to describe
-- the data to be labeled.
--
-- /See:/ 'newLabelingJobDataAttributes' smart constructor.
data LabelingJobDataAttributes = LabelingJobDataAttributes'
  { -- | Declares that your content is free of personally identifiable
    -- information or adult content. Amazon SageMaker may restrict the Amazon
    -- Mechanical Turk workers that can view your task based on this
    -- information.
    contentClassifiers :: Prelude.Maybe [ContentClassifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobDataAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentClassifiers', 'labelingJobDataAttributes_contentClassifiers' - Declares that your content is free of personally identifiable
-- information or adult content. Amazon SageMaker may restrict the Amazon
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
-- information or adult content. Amazon SageMaker may restrict the Amazon
-- Mechanical Turk workers that can view your task based on this
-- information.
labelingJobDataAttributes_contentClassifiers :: Lens.Lens' LabelingJobDataAttributes (Prelude.Maybe [ContentClassifier])
labelingJobDataAttributes_contentClassifiers = Lens.lens (\LabelingJobDataAttributes' {contentClassifiers} -> contentClassifiers) (\s@LabelingJobDataAttributes' {} a -> s {contentClassifiers = a} :: LabelingJobDataAttributes) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON LabelingJobDataAttributes where
  parseJSON =
    Prelude.withObject
      "LabelingJobDataAttributes"
      ( \x ->
          LabelingJobDataAttributes'
            Prelude.<$> ( x Prelude..:? "ContentClassifiers"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LabelingJobDataAttributes

instance Prelude.NFData LabelingJobDataAttributes

instance Prelude.ToJSON LabelingJobDataAttributes where
  toJSON LabelingJobDataAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ContentClassifiers" Prelude..=)
              Prelude.<$> contentClassifiers
          ]
      )
