{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes an individual assessment from a premigration assessment run.
--
--
--
-- /See:/ 'replicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { _rtiaStatus ::
      !(Maybe Text),
    _rtiaReplicationTaskIndividualAssessmentStartDate ::
      !(Maybe POSIX),
    _rtiaIndividualAssessmentName ::
      !(Maybe Text),
    _rtiaReplicationTaskIndividualAssessmentARN ::
      !(Maybe Text),
    _rtiaReplicationTaskAssessmentRunARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTaskIndividualAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtiaStatus' - Individual assessment status. This status can have one of the following values:     * @"cancelled"@      * @"error"@      * @"failed"@      * @"passed"@      * @"pending"@      * @"running"@
--
-- * 'rtiaReplicationTaskIndividualAssessmentStartDate' - Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
--
-- * 'rtiaIndividualAssessmentName' - Name of this individual assessment.
--
-- * 'rtiaReplicationTaskIndividualAssessmentARN' - Amazon Resource Name (ARN) of this individual assessment.
--
-- * 'rtiaReplicationTaskAssessmentRunARN' - ARN of the premigration assessment run that is created to run this individual assessment.
replicationTaskIndividualAssessment ::
  ReplicationTaskIndividualAssessment
replicationTaskIndividualAssessment =
  ReplicationTaskIndividualAssessment'
    { _rtiaStatus = Nothing,
      _rtiaReplicationTaskIndividualAssessmentStartDate =
        Nothing,
      _rtiaIndividualAssessmentName = Nothing,
      _rtiaReplicationTaskIndividualAssessmentARN = Nothing,
      _rtiaReplicationTaskAssessmentRunARN = Nothing
    }

-- | Individual assessment status. This status can have one of the following values:     * @"cancelled"@      * @"error"@      * @"failed"@      * @"passed"@      * @"pending"@      * @"running"@
rtiaStatus :: Lens' ReplicationTaskIndividualAssessment (Maybe Text)
rtiaStatus = lens _rtiaStatus (\s a -> s {_rtiaStatus = a})

-- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
rtiaReplicationTaskIndividualAssessmentStartDate :: Lens' ReplicationTaskIndividualAssessment (Maybe UTCTime)
rtiaReplicationTaskIndividualAssessmentStartDate = lens _rtiaReplicationTaskIndividualAssessmentStartDate (\s a -> s {_rtiaReplicationTaskIndividualAssessmentStartDate = a}) . mapping _Time

-- | Name of this individual assessment.
rtiaIndividualAssessmentName :: Lens' ReplicationTaskIndividualAssessment (Maybe Text)
rtiaIndividualAssessmentName = lens _rtiaIndividualAssessmentName (\s a -> s {_rtiaIndividualAssessmentName = a})

-- | Amazon Resource Name (ARN) of this individual assessment.
rtiaReplicationTaskIndividualAssessmentARN :: Lens' ReplicationTaskIndividualAssessment (Maybe Text)
rtiaReplicationTaskIndividualAssessmentARN = lens _rtiaReplicationTaskIndividualAssessmentARN (\s a -> s {_rtiaReplicationTaskIndividualAssessmentARN = a})

-- | ARN of the premigration assessment run that is created to run this individual assessment.
rtiaReplicationTaskAssessmentRunARN :: Lens' ReplicationTaskIndividualAssessment (Maybe Text)
rtiaReplicationTaskAssessmentRunARN = lens _rtiaReplicationTaskAssessmentRunARN (\s a -> s {_rtiaReplicationTaskAssessmentRunARN = a})

instance FromJSON ReplicationTaskIndividualAssessment where
  parseJSON =
    withObject
      "ReplicationTaskIndividualAssessment"
      ( \x ->
          ReplicationTaskIndividualAssessment'
            <$> (x .:? "Status")
            <*> (x .:? "ReplicationTaskIndividualAssessmentStartDate")
            <*> (x .:? "IndividualAssessmentName")
            <*> (x .:? "ReplicationTaskIndividualAssessmentArn")
            <*> (x .:? "ReplicationTaskAssessmentRunArn")
      )

instance Hashable ReplicationTaskIndividualAssessment

instance NFData ReplicationTaskIndividualAssessment
