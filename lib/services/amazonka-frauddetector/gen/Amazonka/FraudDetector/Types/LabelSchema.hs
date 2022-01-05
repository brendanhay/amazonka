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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.LabelSchema where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The label schema.
--
-- /See:/ 'newLabelSchema' smart constructor.
data LabelSchema = LabelSchema'
  { -- | The action to take for unlabeled events.
    unlabeledEventsTreatment :: Prelude.Maybe UnlabeledEventsTreatment,
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
    labelMapper :: Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)
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
-- 'unlabeledEventsTreatment', 'labelSchema_unlabeledEventsTreatment' - The action to take for unlabeled events.
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
newLabelSchema ::
  LabelSchema
newLabelSchema =
  LabelSchema'
    { unlabeledEventsTreatment =
        Prelude.Nothing,
      labelMapper = Prelude.mempty
    }

-- | The action to take for unlabeled events.
labelSchema_unlabeledEventsTreatment :: Lens.Lens' LabelSchema (Prelude.Maybe UnlabeledEventsTreatment)
labelSchema_unlabeledEventsTreatment = Lens.lens (\LabelSchema' {unlabeledEventsTreatment} -> unlabeledEventsTreatment) (\s@LabelSchema' {} a -> s {unlabeledEventsTreatment = a} :: LabelSchema)

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
labelSchema_labelMapper :: Lens.Lens' LabelSchema (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))
labelSchema_labelMapper = Lens.lens (\LabelSchema' {labelMapper} -> labelMapper) (\s@LabelSchema' {} a -> s {labelMapper = a} :: LabelSchema) Prelude.. Lens.coerced

instance Core.FromJSON LabelSchema where
  parseJSON =
    Core.withObject
      "LabelSchema"
      ( \x ->
          LabelSchema'
            Prelude.<$> (x Core..:? "unlabeledEventsTreatment")
            Prelude.<*> (x Core..:? "labelMapper" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable LabelSchema where
  hashWithSalt _salt LabelSchema' {..} =
    _salt
      `Prelude.hashWithSalt` unlabeledEventsTreatment
      `Prelude.hashWithSalt` labelMapper

instance Prelude.NFData LabelSchema where
  rnf LabelSchema' {..} =
    Prelude.rnf unlabeledEventsTreatment
      `Prelude.seq` Prelude.rnf labelMapper

instance Core.ToJSON LabelSchema where
  toJSON LabelSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unlabeledEventsTreatment" Core..=)
              Prelude.<$> unlabeledEventsTreatment,
            Prelude.Just ("labelMapper" Core..= labelMapper)
          ]
      )
