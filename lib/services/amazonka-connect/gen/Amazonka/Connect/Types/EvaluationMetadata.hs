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
-- Module      : Amazonka.Connect.Types.EvaluationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationMetadata where

import Amazonka.Connect.Types.EvaluationScore
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata information about a contact evaluation.
--
-- /See:/ 'newEvaluationMetadata' smart constructor.
data EvaluationMetadata = EvaluationMetadata'
  { -- | The identifier of the agent who performed the contact.
    contactAgentId :: Prelude.Maybe Prelude.Text,
    -- | The overall score of the contact evaluation.
    score :: Prelude.Maybe EvaluationScore,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who last updated the
    -- evaluation.
    evaluatorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactAgentId', 'evaluationMetadata_contactAgentId' - The identifier of the agent who performed the contact.
--
-- 'score', 'evaluationMetadata_score' - The overall score of the contact evaluation.
--
-- 'contactId', 'evaluationMetadata_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'evaluatorArn', 'evaluationMetadata_evaluatorArn' - The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation.
newEvaluationMetadata ::
  -- | 'contactId'
  Prelude.Text ->
  -- | 'evaluatorArn'
  Prelude.Text ->
  EvaluationMetadata
newEvaluationMetadata pContactId_ pEvaluatorArn_ =
  EvaluationMetadata'
    { contactAgentId =
        Prelude.Nothing,
      score = Prelude.Nothing,
      contactId = pContactId_,
      evaluatorArn = pEvaluatorArn_
    }

-- | The identifier of the agent who performed the contact.
evaluationMetadata_contactAgentId :: Lens.Lens' EvaluationMetadata (Prelude.Maybe Prelude.Text)
evaluationMetadata_contactAgentId = Lens.lens (\EvaluationMetadata' {contactAgentId} -> contactAgentId) (\s@EvaluationMetadata' {} a -> s {contactAgentId = a} :: EvaluationMetadata)

-- | The overall score of the contact evaluation.
evaluationMetadata_score :: Lens.Lens' EvaluationMetadata (Prelude.Maybe EvaluationScore)
evaluationMetadata_score = Lens.lens (\EvaluationMetadata' {score} -> score) (\s@EvaluationMetadata' {} a -> s {score = a} :: EvaluationMetadata)

-- | The identifier of the contact in this instance of Amazon Connect.
evaluationMetadata_contactId :: Lens.Lens' EvaluationMetadata Prelude.Text
evaluationMetadata_contactId = Lens.lens (\EvaluationMetadata' {contactId} -> contactId) (\s@EvaluationMetadata' {} a -> s {contactId = a} :: EvaluationMetadata)

-- | The Amazon Resource Name (ARN) of the user who last updated the
-- evaluation.
evaluationMetadata_evaluatorArn :: Lens.Lens' EvaluationMetadata Prelude.Text
evaluationMetadata_evaluatorArn = Lens.lens (\EvaluationMetadata' {evaluatorArn} -> evaluatorArn) (\s@EvaluationMetadata' {} a -> s {evaluatorArn = a} :: EvaluationMetadata)

instance Data.FromJSON EvaluationMetadata where
  parseJSON =
    Data.withObject
      "EvaluationMetadata"
      ( \x ->
          EvaluationMetadata'
            Prelude.<$> (x Data..:? "ContactAgentId")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..: "ContactId")
            Prelude.<*> (x Data..: "EvaluatorArn")
      )

instance Prelude.Hashable EvaluationMetadata where
  hashWithSalt _salt EvaluationMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` contactAgentId
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` evaluatorArn

instance Prelude.NFData EvaluationMetadata where
  rnf EvaluationMetadata' {..} =
    Prelude.rnf contactAgentId
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf evaluatorArn
