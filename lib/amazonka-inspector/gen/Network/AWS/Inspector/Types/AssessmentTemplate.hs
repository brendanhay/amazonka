{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTemplate where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector assessment template. This data type is used as the response element in the 'DescribeAssessmentTemplates' action.
--
--
--
-- /See:/ 'assessmentTemplate' smart constructor.
data AssessmentTemplate = AssessmentTemplate'
  { _atLastAssessmentRunARN ::
      !(Maybe Text),
    _atArn :: !Text,
    _atName :: !Text,
    _atAssessmentTargetARN :: !Text,
    _atDurationInSeconds :: !Nat,
    _atRulesPackageARNs :: ![Text],
    _atUserAttributesForFindings :: ![Attribute],
    _atAssessmentRunCount :: !Int,
    _atCreatedAt :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atLastAssessmentRunARN' - The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
--
-- * 'atArn' - The ARN of the assessment template.
--
-- * 'atName' - The name of the assessment template.
--
-- * 'atAssessmentTargetARN' - The ARN of the assessment target that corresponds to this assessment template.
--
-- * 'atDurationInSeconds' - The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
--
-- * 'atRulesPackageARNs' - The rules packages that are specified for this assessment template.
--
-- * 'atUserAttributesForFindings' - The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
--
-- * 'atAssessmentRunCount' - The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
--
-- * 'atCreatedAt' - The time at which the assessment template is created.
assessmentTemplate ::
  -- | 'atArn'
  Text ->
  -- | 'atName'
  Text ->
  -- | 'atAssessmentTargetARN'
  Text ->
  -- | 'atDurationInSeconds'
  Natural ->
  -- | 'atAssessmentRunCount'
  Int ->
  -- | 'atCreatedAt'
  UTCTime ->
  AssessmentTemplate
assessmentTemplate
  pArn_
  pName_
  pAssessmentTargetARN_
  pDurationInSeconds_
  pAssessmentRunCount_
  pCreatedAt_ =
    AssessmentTemplate'
      { _atLastAssessmentRunARN = Nothing,
        _atArn = pArn_,
        _atName = pName_,
        _atAssessmentTargetARN = pAssessmentTargetARN_,
        _atDurationInSeconds = _Nat # pDurationInSeconds_,
        _atRulesPackageARNs = mempty,
        _atUserAttributesForFindings = mempty,
        _atAssessmentRunCount = pAssessmentRunCount_,
        _atCreatedAt = _Time # pCreatedAt_
      }

-- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
atLastAssessmentRunARN :: Lens' AssessmentTemplate (Maybe Text)
atLastAssessmentRunARN = lens _atLastAssessmentRunARN (\s a -> s {_atLastAssessmentRunARN = a})

-- | The ARN of the assessment template.
atArn :: Lens' AssessmentTemplate Text
atArn = lens _atArn (\s a -> s {_atArn = a})

-- | The name of the assessment template.
atName :: Lens' AssessmentTemplate Text
atName = lens _atName (\s a -> s {_atName = a})

-- | The ARN of the assessment target that corresponds to this assessment template.
atAssessmentTargetARN :: Lens' AssessmentTemplate Text
atAssessmentTargetARN = lens _atAssessmentTargetARN (\s a -> s {_atAssessmentTargetARN = a})

-- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
atDurationInSeconds :: Lens' AssessmentTemplate Natural
atDurationInSeconds = lens _atDurationInSeconds (\s a -> s {_atDurationInSeconds = a}) . _Nat

-- | The rules packages that are specified for this assessment template.
atRulesPackageARNs :: Lens' AssessmentTemplate [Text]
atRulesPackageARNs = lens _atRulesPackageARNs (\s a -> s {_atRulesPackageARNs = a}) . _Coerce

-- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
atUserAttributesForFindings :: Lens' AssessmentTemplate [Attribute]
atUserAttributesForFindings = lens _atUserAttributesForFindings (\s a -> s {_atUserAttributesForFindings = a}) . _Coerce

-- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
atAssessmentRunCount :: Lens' AssessmentTemplate Int
atAssessmentRunCount = lens _atAssessmentRunCount (\s a -> s {_atAssessmentRunCount = a})

-- | The time at which the assessment template is created.
atCreatedAt :: Lens' AssessmentTemplate UTCTime
atCreatedAt = lens _atCreatedAt (\s a -> s {_atCreatedAt = a}) . _Time

instance FromJSON AssessmentTemplate where
  parseJSON =
    withObject
      "AssessmentTemplate"
      ( \x ->
          AssessmentTemplate'
            <$> (x .:? "lastAssessmentRunArn")
            <*> (x .: "arn")
            <*> (x .: "name")
            <*> (x .: "assessmentTargetArn")
            <*> (x .: "durationInSeconds")
            <*> (x .:? "rulesPackageArns" .!= mempty)
            <*> (x .:? "userAttributesForFindings" .!= mempty)
            <*> (x .: "assessmentRunCount")
            <*> (x .: "createdAt")
      )

instance Hashable AssessmentTemplate

instance NFData AssessmentTemplate
