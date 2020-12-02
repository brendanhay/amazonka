{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplate where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.JobTemplateSettings
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
import Network.AWS.MediaConvert.Types.Type
import Network.AWS.Prelude

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /See:/ 'jobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { _jtAccelerationSettings ::
      !(Maybe AccelerationSettings),
    _jtLastUpdated :: !(Maybe POSIX),
    _jtPriority :: !(Maybe Int),
    _jtStatusUpdateInterval :: !(Maybe StatusUpdateInterval),
    _jtARN :: !(Maybe Text),
    _jtCreatedAt :: !(Maybe POSIX),
    _jtCategory :: !(Maybe Text),
    _jtHopDestinations :: !(Maybe [HopDestination]),
    _jtQueue :: !(Maybe Text),
    _jtType :: !(Maybe Type),
    _jtDescription :: !(Maybe Text),
    _jtSettings :: !JobTemplateSettings,
    _jtName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtAccelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- * 'jtLastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
--
-- * 'jtPriority' - Relative priority on the job.
--
-- * 'jtStatusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'jtARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jtCreatedAt' - The timestamp in epoch seconds for Job template creation.
--
-- * 'jtCategory' - An optional category you create to organize your job templates.
--
-- * 'jtHopDestinations' - Optional list of hop destinations.
--
-- * 'jtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'jtType' - A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
--
-- * 'jtDescription' - An optional description you create for each job template.
--
-- * 'jtSettings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- * 'jtName' - A name you create for each job template. Each name must be unique within your account.
jobTemplate ::
  -- | 'jtSettings'
  JobTemplateSettings ->
  -- | 'jtName'
  Text ->
  JobTemplate
jobTemplate pSettings_ pName_ =
  JobTemplate'
    { _jtAccelerationSettings = Nothing,
      _jtLastUpdated = Nothing,
      _jtPriority = Nothing,
      _jtStatusUpdateInterval = Nothing,
      _jtARN = Nothing,
      _jtCreatedAt = Nothing,
      _jtCategory = Nothing,
      _jtHopDestinations = Nothing,
      _jtQueue = Nothing,
      _jtType = Nothing,
      _jtDescription = Nothing,
      _jtSettings = pSettings_,
      _jtName = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
jtAccelerationSettings :: Lens' JobTemplate (Maybe AccelerationSettings)
jtAccelerationSettings = lens _jtAccelerationSettings (\s a -> s {_jtAccelerationSettings = a})

-- | The timestamp in epoch seconds when the Job template was last updated.
jtLastUpdated :: Lens' JobTemplate (Maybe UTCTime)
jtLastUpdated = lens _jtLastUpdated (\s a -> s {_jtLastUpdated = a}) . mapping _Time

-- | Relative priority on the job.
jtPriority :: Lens' JobTemplate (Maybe Int)
jtPriority = lens _jtPriority (\s a -> s {_jtPriority = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
jtStatusUpdateInterval :: Lens' JobTemplate (Maybe StatusUpdateInterval)
jtStatusUpdateInterval = lens _jtStatusUpdateInterval (\s a -> s {_jtStatusUpdateInterval = a})

-- | An identifier for this resource that is unique within all of AWS.
jtARN :: Lens' JobTemplate (Maybe Text)
jtARN = lens _jtARN (\s a -> s {_jtARN = a})

-- | The timestamp in epoch seconds for Job template creation.
jtCreatedAt :: Lens' JobTemplate (Maybe UTCTime)
jtCreatedAt = lens _jtCreatedAt (\s a -> s {_jtCreatedAt = a}) . mapping _Time

-- | An optional category you create to organize your job templates.
jtCategory :: Lens' JobTemplate (Maybe Text)
jtCategory = lens _jtCategory (\s a -> s {_jtCategory = a})

-- | Optional list of hop destinations.
jtHopDestinations :: Lens' JobTemplate [HopDestination]
jtHopDestinations = lens _jtHopDestinations (\s a -> s {_jtHopDestinations = a}) . _Default . _Coerce

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
jtQueue :: Lens' JobTemplate (Maybe Text)
jtQueue = lens _jtQueue (\s a -> s {_jtQueue = a})

-- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
jtType :: Lens' JobTemplate (Maybe Type)
jtType = lens _jtType (\s a -> s {_jtType = a})

-- | An optional description you create for each job template.
jtDescription :: Lens' JobTemplate (Maybe Text)
jtDescription = lens _jtDescription (\s a -> s {_jtDescription = a})

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
jtSettings :: Lens' JobTemplate JobTemplateSettings
jtSettings = lens _jtSettings (\s a -> s {_jtSettings = a})

-- | A name you create for each job template. Each name must be unique within your account.
jtName :: Lens' JobTemplate Text
jtName = lens _jtName (\s a -> s {_jtName = a})

instance FromJSON JobTemplate where
  parseJSON =
    withObject
      "JobTemplate"
      ( \x ->
          JobTemplate'
            <$> (x .:? "accelerationSettings")
            <*> (x .:? "lastUpdated")
            <*> (x .:? "priority")
            <*> (x .:? "statusUpdateInterval")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "category")
            <*> (x .:? "hopDestinations" .!= mempty)
            <*> (x .:? "queue")
            <*> (x .:? "type")
            <*> (x .:? "description")
            <*> (x .: "settings")
            <*> (x .: "name")
      )

instance Hashable JobTemplate

instance NFData JobTemplate
