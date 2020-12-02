{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResult where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related time stamps, and supplementary information.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erEvaluationResultIdentifier ::
      !(Maybe EvaluationResultIdentifier),
    _erAnnotation :: !(Maybe Text),
    _erConfigRuleInvokedTime :: !(Maybe POSIX),
    _erResultRecordedTime :: !(Maybe POSIX),
    _erResultToken :: !(Maybe Text),
    _erComplianceType :: !(Maybe ComplianceType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erEvaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- * 'erAnnotation' - Supplementary information about how the evaluation determined the compliance.
--
-- * 'erConfigRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
--
-- * 'erResultRecordedTime' - The time when AWS Config recorded the evaluation result.
--
-- * 'erResultToken' - An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
--
-- * 'erComplianceType' - Indicates whether the AWS resource complies with the AWS Config rule that evaluated it. For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
evaluationResult ::
  EvaluationResult
evaluationResult =
  EvaluationResult'
    { _erEvaluationResultIdentifier = Nothing,
      _erAnnotation = Nothing,
      _erConfigRuleInvokedTime = Nothing,
      _erResultRecordedTime = Nothing,
      _erResultToken = Nothing,
      _erComplianceType = Nothing
    }

-- | Uniquely identifies the evaluation result.
erEvaluationResultIdentifier :: Lens' EvaluationResult (Maybe EvaluationResultIdentifier)
erEvaluationResultIdentifier = lens _erEvaluationResultIdentifier (\s a -> s {_erEvaluationResultIdentifier = a})

-- | Supplementary information about how the evaluation determined the compliance.
erAnnotation :: Lens' EvaluationResult (Maybe Text)
erAnnotation = lens _erAnnotation (\s a -> s {_erAnnotation = a})

-- | The time when the AWS Config rule evaluated the AWS resource.
erConfigRuleInvokedTime :: Lens' EvaluationResult (Maybe UTCTime)
erConfigRuleInvokedTime = lens _erConfigRuleInvokedTime (\s a -> s {_erConfigRuleInvokedTime = a}) . mapping _Time

-- | The time when AWS Config recorded the evaluation result.
erResultRecordedTime :: Lens' EvaluationResult (Maybe UTCTime)
erResultRecordedTime = lens _erResultRecordedTime (\s a -> s {_erResultRecordedTime = a}) . mapping _Time

-- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
erResultToken :: Lens' EvaluationResult (Maybe Text)
erResultToken = lens _erResultToken (\s a -> s {_erResultToken = a})

-- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it. For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
erComplianceType :: Lens' EvaluationResult (Maybe ComplianceType)
erComplianceType = lens _erComplianceType (\s a -> s {_erComplianceType = a})

instance FromJSON EvaluationResult where
  parseJSON =
    withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            <$> (x .:? "EvaluationResultIdentifier")
            <*> (x .:? "Annotation")
            <*> (x .:? "ConfigRuleInvokedTime")
            <*> (x .:? "ResultRecordedTime")
            <*> (x .:? "ResultToken")
            <*> (x .:? "ComplianceType")
      )

instance Hashable EvaluationResult

instance NFData EvaluationResult
