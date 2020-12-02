{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TransformJobStatus

-- | Provides a summary of a transform job. Multiple @TransformJobSummary@ objects are returned as a list after in response to a 'ListTransformJobs' call.
--
--
--
-- /See:/ 'transformJobSummary' smart constructor.
data TransformJobSummary = TransformJobSummary'
  { _tjsFailureReason ::
      !(Maybe Text),
    _tjsLastModifiedTime :: !(Maybe POSIX),
    _tjsTransformEndTime :: !(Maybe POSIX),
    _tjsTransformJobName :: !Text,
    _tjsTransformJobARN :: !Text,
    _tjsCreationTime :: !POSIX,
    _tjsTransformJobStatus :: !TransformJobStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsFailureReason' - If the transform job failed, the reason it failed.
--
-- * 'tjsLastModifiedTime' - Indicates when the transform job was last modified.
--
-- * 'tjsTransformEndTime' - Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
--
-- * 'tjsTransformJobName' - The name of the transform job.
--
-- * 'tjsTransformJobARN' - The Amazon Resource Name (ARN) of the transform job.
--
-- * 'tjsCreationTime' - A timestamp that shows when the transform Job was created.
--
-- * 'tjsTransformJobStatus' - The status of the transform job.
transformJobSummary ::
  -- | 'tjsTransformJobName'
  Text ->
  -- | 'tjsTransformJobARN'
  Text ->
  -- | 'tjsCreationTime'
  UTCTime ->
  -- | 'tjsTransformJobStatus'
  TransformJobStatus ->
  TransformJobSummary
transformJobSummary
  pTransformJobName_
  pTransformJobARN_
  pCreationTime_
  pTransformJobStatus_ =
    TransformJobSummary'
      { _tjsFailureReason = Nothing,
        _tjsLastModifiedTime = Nothing,
        _tjsTransformEndTime = Nothing,
        _tjsTransformJobName = pTransformJobName_,
        _tjsTransformJobARN = pTransformJobARN_,
        _tjsCreationTime = _Time # pCreationTime_,
        _tjsTransformJobStatus = pTransformJobStatus_
      }

-- | If the transform job failed, the reason it failed.
tjsFailureReason :: Lens' TransformJobSummary (Maybe Text)
tjsFailureReason = lens _tjsFailureReason (\s a -> s {_tjsFailureReason = a})

-- | Indicates when the transform job was last modified.
tjsLastModifiedTime :: Lens' TransformJobSummary (Maybe UTCTime)
tjsLastModifiedTime = lens _tjsLastModifiedTime (\s a -> s {_tjsLastModifiedTime = a}) . mapping _Time

-- | Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
tjsTransformEndTime :: Lens' TransformJobSummary (Maybe UTCTime)
tjsTransformEndTime = lens _tjsTransformEndTime (\s a -> s {_tjsTransformEndTime = a}) . mapping _Time

-- | The name of the transform job.
tjsTransformJobName :: Lens' TransformJobSummary Text
tjsTransformJobName = lens _tjsTransformJobName (\s a -> s {_tjsTransformJobName = a})

-- | The Amazon Resource Name (ARN) of the transform job.
tjsTransformJobARN :: Lens' TransformJobSummary Text
tjsTransformJobARN = lens _tjsTransformJobARN (\s a -> s {_tjsTransformJobARN = a})

-- | A timestamp that shows when the transform Job was created.
tjsCreationTime :: Lens' TransformJobSummary UTCTime
tjsCreationTime = lens _tjsCreationTime (\s a -> s {_tjsCreationTime = a}) . _Time

-- | The status of the transform job.
tjsTransformJobStatus :: Lens' TransformJobSummary TransformJobStatus
tjsTransformJobStatus = lens _tjsTransformJobStatus (\s a -> s {_tjsTransformJobStatus = a})

instance FromJSON TransformJobSummary where
  parseJSON =
    withObject
      "TransformJobSummary"
      ( \x ->
          TransformJobSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "TransformEndTime")
            <*> (x .: "TransformJobName")
            <*> (x .: "TransformJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "TransformJobStatus")
      )

instance Hashable TransformJobSummary

instance NFData TransformJobSummary
