{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Evaluation where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.
--
--
--
-- /See:/ 'evaluation' smart constructor.
data Evaluation = Evaluation'
  { _eAnnotation :: !(Maybe Text),
    _eComplianceResourceType :: !Text,
    _eComplianceResourceId :: !Text,
    _eComplianceType :: !ComplianceType,
    _eOrderingTimestamp :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAnnotation' - Supplementary information about how the evaluation determined the compliance.
--
-- * 'eComplianceResourceType' - The type of AWS resource that was evaluated.
--
-- * 'eComplianceResourceId' - The ID of the AWS resource that was evaluated.
--
-- * 'eComplianceType' - Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against. For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type. Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
--
-- * 'eOrderingTimestamp' - The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
evaluation ::
  -- | 'eComplianceResourceType'
  Text ->
  -- | 'eComplianceResourceId'
  Text ->
  -- | 'eComplianceType'
  ComplianceType ->
  -- | 'eOrderingTimestamp'
  UTCTime ->
  Evaluation
evaluation
  pComplianceResourceType_
  pComplianceResourceId_
  pComplianceType_
  pOrderingTimestamp_ =
    Evaluation'
      { _eAnnotation = Nothing,
        _eComplianceResourceType = pComplianceResourceType_,
        _eComplianceResourceId = pComplianceResourceId_,
        _eComplianceType = pComplianceType_,
        _eOrderingTimestamp = _Time # pOrderingTimestamp_
      }

-- | Supplementary information about how the evaluation determined the compliance.
eAnnotation :: Lens' Evaluation (Maybe Text)
eAnnotation = lens _eAnnotation (\s a -> s {_eAnnotation = a})

-- | The type of AWS resource that was evaluated.
eComplianceResourceType :: Lens' Evaluation Text
eComplianceResourceType = lens _eComplianceResourceType (\s a -> s {_eComplianceResourceType = a})

-- | The ID of the AWS resource that was evaluated.
eComplianceResourceId :: Lens' Evaluation Text
eComplianceResourceId = lens _eComplianceResourceId (\s a -> s {_eComplianceResourceId = a})

-- | Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against. For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type. Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
eComplianceType :: Lens' Evaluation ComplianceType
eComplianceType = lens _eComplianceType (\s a -> s {_eComplianceType = a})

-- | The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
eOrderingTimestamp :: Lens' Evaluation UTCTime
eOrderingTimestamp = lens _eOrderingTimestamp (\s a -> s {_eOrderingTimestamp = a}) . _Time

instance FromJSON Evaluation where
  parseJSON =
    withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            <$> (x .:? "Annotation")
            <*> (x .: "ComplianceResourceType")
            <*> (x .: "ComplianceResourceId")
            <*> (x .: "ComplianceType")
            <*> (x .: "OrderingTimestamp")
      )

instance Hashable Evaluation

instance NFData Evaluation

instance ToJSON Evaluation where
  toJSON Evaluation' {..} =
    object
      ( catMaybes
          [ ("Annotation" .=) <$> _eAnnotation,
            Just ("ComplianceResourceType" .= _eComplianceResourceType),
            Just ("ComplianceResourceId" .= _eComplianceResourceId),
            Just ("ComplianceType" .= _eComplianceType),
            Just ("OrderingTimestamp" .= _eOrderingTimestamp)
          ]
      )
