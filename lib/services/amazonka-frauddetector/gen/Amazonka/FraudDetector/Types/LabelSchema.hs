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
-- Module      : Amazonka.FraudDetector.Types.LabelSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.LabelSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
import qualified Amazonka.Prelude as Prelude

-- | The label schema.
--
-- /See:/ 'newLabelSchema' smart constructor.
data LabelSchema = LabelSchema'
  { -- | The label mapper maps the Amazon Fraud Detector supported model
    -- classification labels (@FRAUD@, @LEGIT@) to the appropriate event type
    -- labels. For example, if \"@FRAUD@\" and \"@LEGIT@\" are Amazon Fraud
    -- Detector supported labels, this mapper could be:
    -- @{\"FRAUD\" => [\"0\"]@, @\"LEGIT\" => [\"1\"]}@ or
    -- @{\"FRAUD\" => [\"false\"]@, @\"LEGIT\" => [\"true\"]}@ or
    -- @{\"FRAUD\" => [\"fraud\", \"abuse\"]@,
    -- @\"LEGIT\" => [\"legit\", \"safe\"]}@. The value part of the mapper is a
    -- list, because you may have multiple label variants from your event type
    -- for a single Amazon Fraud Detector label.
    labelMapper :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The action to take for unlabeled events.
    --
    -- -   Use @IGNORE@ if you want the unlabeled events to be ignored. This is
    --     recommended when the majority of the events in the dataset are
    --     labeled.
    --
    -- -   Use @FRAUD@ if you want to categorize all unlabeled events as
    --     “Fraud”. This is recommended when most of the events in your dataset
    --     are fraudulent.
    --
    -- -   Use @LEGIT@ if you want to categorize all unlabeled events as
    --     “Legit”. This is recommended when most of the events in your dataset
    --     are legitimate.
    --
    -- -   Use @AUTO@ if you want Amazon Fraud Detector to decide how to use
    --     the unlabeled data. This is recommended when there is significant
    --     unlabeled events in the dataset.
    --
    -- By default, Amazon Fraud Detector ignores the unlabeled data.
    unlabeledEventsTreatment :: Prelude.Maybe UnlabeledEventsTreatment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelMapper', 'labelSchema_labelMapper' - The label mapper maps the Amazon Fraud Detector supported model
-- classification labels (@FRAUD@, @LEGIT@) to the appropriate event type
-- labels. For example, if \"@FRAUD@\" and \"@LEGIT@\" are Amazon Fraud
-- Detector supported labels, this mapper could be:
-- @{\"FRAUD\" => [\"0\"]@, @\"LEGIT\" => [\"1\"]}@ or
-- @{\"FRAUD\" => [\"false\"]@, @\"LEGIT\" => [\"true\"]}@ or
-- @{\"FRAUD\" => [\"fraud\", \"abuse\"]@,
-- @\"LEGIT\" => [\"legit\", \"safe\"]}@. The value part of the mapper is a
-- list, because you may have multiple label variants from your event type
-- for a single Amazon Fraud Detector label.
--
-- 'unlabeledEventsTreatment', 'labelSchema_unlabeledEventsTreatment' - The action to take for unlabeled events.
--
-- -   Use @IGNORE@ if you want the unlabeled events to be ignored. This is
--     recommended when the majority of the events in the dataset are
--     labeled.
--
-- -   Use @FRAUD@ if you want to categorize all unlabeled events as
--     “Fraud”. This is recommended when most of the events in your dataset
--     are fraudulent.
--
-- -   Use @LEGIT@ if you want to categorize all unlabeled events as
--     “Legit”. This is recommended when most of the events in your dataset
--     are legitimate.
--
-- -   Use @AUTO@ if you want Amazon Fraud Detector to decide how to use
--     the unlabeled data. This is recommended when there is significant
--     unlabeled events in the dataset.
--
-- By default, Amazon Fraud Detector ignores the unlabeled data.
newLabelSchema ::
  LabelSchema
newLabelSchema =
  LabelSchema'
    { labelMapper = Prelude.Nothing,
      unlabeledEventsTreatment = Prelude.Nothing
    }

-- | The label mapper maps the Amazon Fraud Detector supported model
-- classification labels (@FRAUD@, @LEGIT@) to the appropriate event type
-- labels. For example, if \"@FRAUD@\" and \"@LEGIT@\" are Amazon Fraud
-- Detector supported labels, this mapper could be:
-- @{\"FRAUD\" => [\"0\"]@, @\"LEGIT\" => [\"1\"]}@ or
-- @{\"FRAUD\" => [\"false\"]@, @\"LEGIT\" => [\"true\"]}@ or
-- @{\"FRAUD\" => [\"fraud\", \"abuse\"]@,
-- @\"LEGIT\" => [\"legit\", \"safe\"]}@. The value part of the mapper is a
-- list, because you may have multiple label variants from your event type
-- for a single Amazon Fraud Detector label.
labelSchema_labelMapper :: Lens.Lens' LabelSchema (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
labelSchema_labelMapper = Lens.lens (\LabelSchema' {labelMapper} -> labelMapper) (\s@LabelSchema' {} a -> s {labelMapper = a} :: LabelSchema) Prelude.. Lens.mapping Lens.coerced

-- | The action to take for unlabeled events.
--
-- -   Use @IGNORE@ if you want the unlabeled events to be ignored. This is
--     recommended when the majority of the events in the dataset are
--     labeled.
--
-- -   Use @FRAUD@ if you want to categorize all unlabeled events as
--     “Fraud”. This is recommended when most of the events in your dataset
--     are fraudulent.
--
-- -   Use @LEGIT@ if you want to categorize all unlabeled events as
--     “Legit”. This is recommended when most of the events in your dataset
--     are legitimate.
--
-- -   Use @AUTO@ if you want Amazon Fraud Detector to decide how to use
--     the unlabeled data. This is recommended when there is significant
--     unlabeled events in the dataset.
--
-- By default, Amazon Fraud Detector ignores the unlabeled data.
labelSchema_unlabeledEventsTreatment :: Lens.Lens' LabelSchema (Prelude.Maybe UnlabeledEventsTreatment)
labelSchema_unlabeledEventsTreatment = Lens.lens (\LabelSchema' {unlabeledEventsTreatment} -> unlabeledEventsTreatment) (\s@LabelSchema' {} a -> s {unlabeledEventsTreatment = a} :: LabelSchema)

instance Data.FromJSON LabelSchema where
  parseJSON =
    Data.withObject
      "LabelSchema"
      ( \x ->
          LabelSchema'
            Prelude.<$> (x Data..:? "labelMapper" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "unlabeledEventsTreatment")
      )

instance Prelude.Hashable LabelSchema where
  hashWithSalt _salt LabelSchema' {..} =
    _salt
      `Prelude.hashWithSalt` labelMapper
      `Prelude.hashWithSalt` unlabeledEventsTreatment

instance Prelude.NFData LabelSchema where
  rnf LabelSchema' {..} =
    Prelude.rnf labelMapper
      `Prelude.seq` Prelude.rnf unlabeledEventsTreatment

instance Data.ToJSON LabelSchema where
  toJSON LabelSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("labelMapper" Data..=) Prelude.<$> labelMapper,
            ("unlabeledEventsTreatment" Data..=)
              Prelude.<$> unlabeledEventsTreatment
          ]
      )
