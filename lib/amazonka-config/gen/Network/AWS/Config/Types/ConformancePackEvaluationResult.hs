{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationResult where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a conformance pack evaluation. Provides AWS Config rule and AWS resource type that was evaluated, the compliance of the conformance pack, related time stamps, and supplementary information.
--
--
--
-- /See:/ 'conformancePackEvaluationResult' smart constructor.
data ConformancePackEvaluationResult = ConformancePackEvaluationResult'
  { _cperAnnotation ::
      !(Maybe Text),
    _cperComplianceType ::
      !ConformancePackComplianceType,
    _cperEvaluationResultIdentifier ::
      !EvaluationResultIdentifier,
    _cperConfigRuleInvokedTime ::
      !POSIX,
    _cperResultRecordedTime ::
      !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackEvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cperAnnotation' - Supplementary information about how the evaluation determined the compliance.
--
-- * 'cperComplianceType' - The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- * 'cperEvaluationResultIdentifier' - Undocumented member.
--
-- * 'cperConfigRuleInvokedTime' - The time when AWS Config rule evaluated AWS resource.
--
-- * 'cperResultRecordedTime' - The time when AWS Config recorded the evaluation result.
conformancePackEvaluationResult ::
  -- | 'cperComplianceType'
  ConformancePackComplianceType ->
  -- | 'cperEvaluationResultIdentifier'
  EvaluationResultIdentifier ->
  -- | 'cperConfigRuleInvokedTime'
  UTCTime ->
  -- | 'cperResultRecordedTime'
  UTCTime ->
  ConformancePackEvaluationResult
conformancePackEvaluationResult
  pComplianceType_
  pEvaluationResultIdentifier_
  pConfigRuleInvokedTime_
  pResultRecordedTime_ =
    ConformancePackEvaluationResult'
      { _cperAnnotation = Nothing,
        _cperComplianceType = pComplianceType_,
        _cperEvaluationResultIdentifier = pEvaluationResultIdentifier_,
        _cperConfigRuleInvokedTime = _Time # pConfigRuleInvokedTime_,
        _cperResultRecordedTime = _Time # pResultRecordedTime_
      }

-- | Supplementary information about how the evaluation determined the compliance.
cperAnnotation :: Lens' ConformancePackEvaluationResult (Maybe Text)
cperAnnotation = lens _cperAnnotation (\s a -> s {_cperAnnotation = a})

-- | The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
cperComplianceType :: Lens' ConformancePackEvaluationResult ConformancePackComplianceType
cperComplianceType = lens _cperComplianceType (\s a -> s {_cperComplianceType = a})

-- | Undocumented member.
cperEvaluationResultIdentifier :: Lens' ConformancePackEvaluationResult EvaluationResultIdentifier
cperEvaluationResultIdentifier = lens _cperEvaluationResultIdentifier (\s a -> s {_cperEvaluationResultIdentifier = a})

-- | The time when AWS Config rule evaluated AWS resource.
cperConfigRuleInvokedTime :: Lens' ConformancePackEvaluationResult UTCTime
cperConfigRuleInvokedTime = lens _cperConfigRuleInvokedTime (\s a -> s {_cperConfigRuleInvokedTime = a}) . _Time

-- | The time when AWS Config recorded the evaluation result.
cperResultRecordedTime :: Lens' ConformancePackEvaluationResult UTCTime
cperResultRecordedTime = lens _cperResultRecordedTime (\s a -> s {_cperResultRecordedTime = a}) . _Time

instance FromJSON ConformancePackEvaluationResult where
  parseJSON =
    withObject
      "ConformancePackEvaluationResult"
      ( \x ->
          ConformancePackEvaluationResult'
            <$> (x .:? "Annotation")
            <*> (x .: "ComplianceType")
            <*> (x .: "EvaluationResultIdentifier")
            <*> (x .: "ConfigRuleInvokedTime")
            <*> (x .: "ResultRecordedTime")
      )

instance Hashable ConformancePackEvaluationResult

instance NFData ConformancePackEvaluationResult
