{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EvaluationFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.EvaluationFilterVariable
  ( EvaluationFilterVariable
      ( EvaluationFilterVariable',
        EvalCreatedAt,
        EvalDataSourceId,
        EvalDataURI,
        EvalIAMUser,
        EvalLastUpdatedAt,
        EvalMLModelId,
        EvalName,
        EvalStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | A list of the variables to use in searching or filtering @Evaluation@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @Evaluation@ creation date.
--
--     * @Status@ - Sets the search criteria to @Evaluation@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked an evaluation.
--
--     * @MLModelId@ - Sets the search criteria to the @Predictor@ that was evaluated.
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in evaluation.
--
--     * @DataUri@ - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
newtype EvaluationFilterVariable = EvaluationFilterVariable' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern EvalCreatedAt :: EvaluationFilterVariable
pattern EvalCreatedAt = EvaluationFilterVariable' "CreatedAt"

pattern EvalDataSourceId :: EvaluationFilterVariable
pattern EvalDataSourceId = EvaluationFilterVariable' "DataSourceId"

pattern EvalDataURI :: EvaluationFilterVariable
pattern EvalDataURI = EvaluationFilterVariable' "DataURI"

pattern EvalIAMUser :: EvaluationFilterVariable
pattern EvalIAMUser = EvaluationFilterVariable' "IAMUser"

pattern EvalLastUpdatedAt :: EvaluationFilterVariable
pattern EvalLastUpdatedAt = EvaluationFilterVariable' "LastUpdatedAt"

pattern EvalMLModelId :: EvaluationFilterVariable
pattern EvalMLModelId = EvaluationFilterVariable' "MLModelId"

pattern EvalName :: EvaluationFilterVariable
pattern EvalName = EvaluationFilterVariable' "Name"

pattern EvalStatus :: EvaluationFilterVariable
pattern EvalStatus = EvaluationFilterVariable' "Status"

{-# COMPLETE
  EvalCreatedAt,
  EvalDataSourceId,
  EvalDataURI,
  EvalIAMUser,
  EvalLastUpdatedAt,
  EvalMLModelId,
  EvalName,
  EvalStatus,
  EvaluationFilterVariable'
  #-}
