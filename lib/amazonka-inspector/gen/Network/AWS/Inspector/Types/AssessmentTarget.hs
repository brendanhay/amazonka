{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector application. This data type is used as the response element in the 'DescribeAssessmentTargets' action.
--
--
--
-- /See:/ 'assessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { _aResourceGroupARN ::
      !(Maybe Text),
    _aArn :: !Text,
    _aName :: !Text,
    _aCreatedAt :: !POSIX,
    _aUpdatedAt :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aResourceGroupARN' - The ARN that specifies the resource group that is associated with the assessment target.
--
-- * 'aArn' - The ARN that specifies the Amazon Inspector assessment target.
--
-- * 'aName' - The name of the Amazon Inspector assessment target.
--
-- * 'aCreatedAt' - The time at which the assessment target is created.
--
-- * 'aUpdatedAt' - The time at which 'UpdateAssessmentTarget' is called.
assessmentTarget ::
  -- | 'aArn'
  Text ->
  -- | 'aName'
  Text ->
  -- | 'aCreatedAt'
  UTCTime ->
  -- | 'aUpdatedAt'
  UTCTime ->
  AssessmentTarget
assessmentTarget pArn_ pName_ pCreatedAt_ pUpdatedAt_ =
  AssessmentTarget'
    { _aResourceGroupARN = Nothing,
      _aArn = pArn_,
      _aName = pName_,
      _aCreatedAt = _Time # pCreatedAt_,
      _aUpdatedAt = _Time # pUpdatedAt_
    }

-- | The ARN that specifies the resource group that is associated with the assessment target.
aResourceGroupARN :: Lens' AssessmentTarget (Maybe Text)
aResourceGroupARN = lens _aResourceGroupARN (\s a -> s {_aResourceGroupARN = a})

-- | The ARN that specifies the Amazon Inspector assessment target.
aArn :: Lens' AssessmentTarget Text
aArn = lens _aArn (\s a -> s {_aArn = a})

-- | The name of the Amazon Inspector assessment target.
aName :: Lens' AssessmentTarget Text
aName = lens _aName (\s a -> s {_aName = a})

-- | The time at which the assessment target is created.
aCreatedAt :: Lens' AssessmentTarget UTCTime
aCreatedAt = lens _aCreatedAt (\s a -> s {_aCreatedAt = a}) . _Time

-- | The time at which 'UpdateAssessmentTarget' is called.
aUpdatedAt :: Lens' AssessmentTarget UTCTime
aUpdatedAt = lens _aUpdatedAt (\s a -> s {_aUpdatedAt = a}) . _Time

instance FromJSON AssessmentTarget where
  parseJSON =
    withObject
      "AssessmentTarget"
      ( \x ->
          AssessmentTarget'
            <$> (x .:? "resourceGroupArn")
            <*> (x .: "arn")
            <*> (x .: "name")
            <*> (x .: "createdAt")
            <*> (x .: "updatedAt")
      )

instance Hashable AssessmentTarget

instance NFData AssessmentTarget
