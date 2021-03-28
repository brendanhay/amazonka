{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EvaluationFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.EvaluationFilterVariable
  ( EvaluationFilterVariable
    ( EvaluationFilterVariable'
    , EvaluationFilterVariableEvalCreatedAt
    , EvaluationFilterVariableEvalLastUpdatedAt
    , EvaluationFilterVariableEvalStatus
    , EvaluationFilterVariableEvalName
    , EvaluationFilterVariableEvalIAMUser
    , EvaluationFilterVariableEvalMLModelId
    , EvaluationFilterVariableEvalDataSourceId
    , EvaluationFilterVariableEvalDataURI
    , fromEvaluationFilterVariable
    )
  ) where

import qualified Network.AWS.Prelude as Core

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
--
newtype EvaluationFilterVariable = EvaluationFilterVariable'{fromEvaluationFilterVariable
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern EvaluationFilterVariableEvalCreatedAt :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalCreatedAt = EvaluationFilterVariable' "CreatedAt"

pattern EvaluationFilterVariableEvalLastUpdatedAt :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalLastUpdatedAt = EvaluationFilterVariable' "LastUpdatedAt"

pattern EvaluationFilterVariableEvalStatus :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalStatus = EvaluationFilterVariable' "Status"

pattern EvaluationFilterVariableEvalName :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalName = EvaluationFilterVariable' "Name"

pattern EvaluationFilterVariableEvalIAMUser :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalIAMUser = EvaluationFilterVariable' "IAMUser"

pattern EvaluationFilterVariableEvalMLModelId :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalMLModelId = EvaluationFilterVariable' "MLModelId"

pattern EvaluationFilterVariableEvalDataSourceId :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalDataSourceId = EvaluationFilterVariable' "DataSourceId"

pattern EvaluationFilterVariableEvalDataURI :: EvaluationFilterVariable
pattern EvaluationFilterVariableEvalDataURI = EvaluationFilterVariable' "DataURI"

{-# COMPLETE 
  EvaluationFilterVariableEvalCreatedAt,

  EvaluationFilterVariableEvalLastUpdatedAt,

  EvaluationFilterVariableEvalStatus,

  EvaluationFilterVariableEvalName,

  EvaluationFilterVariableEvalIAMUser,

  EvaluationFilterVariableEvalMLModelId,

  EvaluationFilterVariableEvalDataSourceId,

  EvaluationFilterVariableEvalDataURI,
  EvaluationFilterVariable'
  #-}
