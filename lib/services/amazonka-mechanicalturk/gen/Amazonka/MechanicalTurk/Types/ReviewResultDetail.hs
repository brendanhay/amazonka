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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewResultDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewResultDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data structure is returned multiple times for each result specified
-- in the Review Policy.
--
-- /See:/ 'newReviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { -- | A unique identifier of the Review action result.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | Key identifies the particular piece of reviewed information.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specifies the QuestionId the result is describing. Depending on whether
    -- the TargetType is a HIT or Assignment this results could specify
    -- multiple values. If TargetType is HIT and QuestionId is absent, then the
    -- result describes results of the HIT, including the HIT agreement score.
    -- If ObjectType is Assignment and QuestionId is absent, then the result
    -- describes the Worker\'s performance on the HIT.
    questionId :: Prelude.Maybe Prelude.Text,
    -- | The HITID or AssignmentId about which this result was taken. Note that
    -- HIT-level Review Policies will often emit results about both the HIT
    -- itself and its Assignments, while Assignment-level review policies
    -- generally only emit results about the Assignment itself.
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The type of the object from the SubjectId field.
    subjectType :: Prelude.Maybe Prelude.Text,
    -- | The values of Key provided by the review policies you have selected.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReviewResultDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'reviewResultDetail_actionId' - A unique identifier of the Review action result.
--
-- 'key', 'reviewResultDetail_key' - Key identifies the particular piece of reviewed information.
--
-- 'questionId', 'reviewResultDetail_questionId' - Specifies the QuestionId the result is describing. Depending on whether
-- the TargetType is a HIT or Assignment this results could specify
-- multiple values. If TargetType is HIT and QuestionId is absent, then the
-- result describes results of the HIT, including the HIT agreement score.
-- If ObjectType is Assignment and QuestionId is absent, then the result
-- describes the Worker\'s performance on the HIT.
--
-- 'subjectId', 'reviewResultDetail_subjectId' - The HITID or AssignmentId about which this result was taken. Note that
-- HIT-level Review Policies will often emit results about both the HIT
-- itself and its Assignments, while Assignment-level review policies
-- generally only emit results about the Assignment itself.
--
-- 'subjectType', 'reviewResultDetail_subjectType' - The type of the object from the SubjectId field.
--
-- 'value', 'reviewResultDetail_value' - The values of Key provided by the review policies you have selected.
newReviewResultDetail ::
  ReviewResultDetail
newReviewResultDetail =
  ReviewResultDetail'
    { actionId = Prelude.Nothing,
      key = Prelude.Nothing,
      questionId = Prelude.Nothing,
      subjectId = Prelude.Nothing,
      subjectType = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A unique identifier of the Review action result.
reviewResultDetail_actionId :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_actionId = Lens.lens (\ReviewResultDetail' {actionId} -> actionId) (\s@ReviewResultDetail' {} a -> s {actionId = a} :: ReviewResultDetail)

-- | Key identifies the particular piece of reviewed information.
reviewResultDetail_key :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_key = Lens.lens (\ReviewResultDetail' {key} -> key) (\s@ReviewResultDetail' {} a -> s {key = a} :: ReviewResultDetail)

-- | Specifies the QuestionId the result is describing. Depending on whether
-- the TargetType is a HIT or Assignment this results could specify
-- multiple values. If TargetType is HIT and QuestionId is absent, then the
-- result describes results of the HIT, including the HIT agreement score.
-- If ObjectType is Assignment and QuestionId is absent, then the result
-- describes the Worker\'s performance on the HIT.
reviewResultDetail_questionId :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_questionId = Lens.lens (\ReviewResultDetail' {questionId} -> questionId) (\s@ReviewResultDetail' {} a -> s {questionId = a} :: ReviewResultDetail)

-- | The HITID or AssignmentId about which this result was taken. Note that
-- HIT-level Review Policies will often emit results about both the HIT
-- itself and its Assignments, while Assignment-level review policies
-- generally only emit results about the Assignment itself.
reviewResultDetail_subjectId :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_subjectId = Lens.lens (\ReviewResultDetail' {subjectId} -> subjectId) (\s@ReviewResultDetail' {} a -> s {subjectId = a} :: ReviewResultDetail)

-- | The type of the object from the SubjectId field.
reviewResultDetail_subjectType :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_subjectType = Lens.lens (\ReviewResultDetail' {subjectType} -> subjectType) (\s@ReviewResultDetail' {} a -> s {subjectType = a} :: ReviewResultDetail)

-- | The values of Key provided by the review policies you have selected.
reviewResultDetail_value :: Lens.Lens' ReviewResultDetail (Prelude.Maybe Prelude.Text)
reviewResultDetail_value = Lens.lens (\ReviewResultDetail' {value} -> value) (\s@ReviewResultDetail' {} a -> s {value = a} :: ReviewResultDetail)

instance Data.FromJSON ReviewResultDetail where
  parseJSON =
    Data.withObject
      "ReviewResultDetail"
      ( \x ->
          ReviewResultDetail'
            Prelude.<$> (x Data..:? "ActionId")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "SubjectId")
            Prelude.<*> (x Data..:? "SubjectType")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ReviewResultDetail where
  hashWithSalt _salt ReviewResultDetail' {..} =
    _salt
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` subjectType
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReviewResultDetail where
  rnf ReviewResultDetail' {..} =
    Prelude.rnf actionId `Prelude.seq`
      Prelude.rnf key `Prelude.seq`
        Prelude.rnf questionId `Prelude.seq`
          Prelude.rnf subjectId `Prelude.seq`
            Prelude.rnf subjectType `Prelude.seq`
              Prelude.rnf value
