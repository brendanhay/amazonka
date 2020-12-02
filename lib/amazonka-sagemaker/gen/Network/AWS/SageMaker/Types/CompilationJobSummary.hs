{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CompilationJobStatus
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOS

-- | A summary of a model compilation job.
--
--
--
-- /See:/ 'compilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { _cjsCompilationStartTime ::
      !(Maybe POSIX),
    _cjsCompilationTargetPlatformAccelerator ::
      !(Maybe TargetPlatformAccelerator),
    _cjsCompilationTargetDevice ::
      !(Maybe TargetDevice),
    _cjsLastModifiedTime :: !(Maybe POSIX),
    _cjsCompilationTargetPlatformArch ::
      !(Maybe TargetPlatformArch),
    _cjsCompilationEndTime :: !(Maybe POSIX),
    _cjsCompilationTargetPlatformOS ::
      !(Maybe TargetPlatformOS),
    _cjsCompilationJobName :: !Text,
    _cjsCompilationJobARN :: !Text,
    _cjsCreationTime :: !POSIX,
    _cjsCompilationJobStatus ::
      !CompilationJobStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompilationJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjsCompilationStartTime' - The time when the model compilation job started.
--
-- * 'cjsCompilationTargetPlatformAccelerator' - The type of accelerator that the model will run on after the compilation job has completed.
--
-- * 'cjsCompilationTargetDevice' - The type of device that the model will run on after the compilation job has completed.
--
-- * 'cjsLastModifiedTime' - The time when the model compilation job was last modified.
--
-- * 'cjsCompilationTargetPlatformArch' - The type of architecture that the model will run on after the compilation job has completed.
--
-- * 'cjsCompilationEndTime' - The time when the model compilation job completed.
--
-- * 'cjsCompilationTargetPlatformOS' - The type of OS that the model will run on after the compilation job has completed.
--
-- * 'cjsCompilationJobName' - The name of the model compilation job that you want a summary for.
--
-- * 'cjsCompilationJobARN' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- * 'cjsCreationTime' - The time when the model compilation job was created.
--
-- * 'cjsCompilationJobStatus' - The status of the model compilation job.
compilationJobSummary ::
  -- | 'cjsCompilationJobName'
  Text ->
  -- | 'cjsCompilationJobARN'
  Text ->
  -- | 'cjsCreationTime'
  UTCTime ->
  -- | 'cjsCompilationJobStatus'
  CompilationJobStatus ->
  CompilationJobSummary
compilationJobSummary
  pCompilationJobName_
  pCompilationJobARN_
  pCreationTime_
  pCompilationJobStatus_ =
    CompilationJobSummary'
      { _cjsCompilationStartTime = Nothing,
        _cjsCompilationTargetPlatformAccelerator = Nothing,
        _cjsCompilationTargetDevice = Nothing,
        _cjsLastModifiedTime = Nothing,
        _cjsCompilationTargetPlatformArch = Nothing,
        _cjsCompilationEndTime = Nothing,
        _cjsCompilationTargetPlatformOS = Nothing,
        _cjsCompilationJobName = pCompilationJobName_,
        _cjsCompilationJobARN = pCompilationJobARN_,
        _cjsCreationTime = _Time # pCreationTime_,
        _cjsCompilationJobStatus = pCompilationJobStatus_
      }

-- | The time when the model compilation job started.
cjsCompilationStartTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsCompilationStartTime = lens _cjsCompilationStartTime (\s a -> s {_cjsCompilationStartTime = a}) . mapping _Time

-- | The type of accelerator that the model will run on after the compilation job has completed.
cjsCompilationTargetPlatformAccelerator :: Lens' CompilationJobSummary (Maybe TargetPlatformAccelerator)
cjsCompilationTargetPlatformAccelerator = lens _cjsCompilationTargetPlatformAccelerator (\s a -> s {_cjsCompilationTargetPlatformAccelerator = a})

-- | The type of device that the model will run on after the compilation job has completed.
cjsCompilationTargetDevice :: Lens' CompilationJobSummary (Maybe TargetDevice)
cjsCompilationTargetDevice = lens _cjsCompilationTargetDevice (\s a -> s {_cjsCompilationTargetDevice = a})

-- | The time when the model compilation job was last modified.
cjsLastModifiedTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsLastModifiedTime = lens _cjsLastModifiedTime (\s a -> s {_cjsLastModifiedTime = a}) . mapping _Time

-- | The type of architecture that the model will run on after the compilation job has completed.
cjsCompilationTargetPlatformArch :: Lens' CompilationJobSummary (Maybe TargetPlatformArch)
cjsCompilationTargetPlatformArch = lens _cjsCompilationTargetPlatformArch (\s a -> s {_cjsCompilationTargetPlatformArch = a})

-- | The time when the model compilation job completed.
cjsCompilationEndTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsCompilationEndTime = lens _cjsCompilationEndTime (\s a -> s {_cjsCompilationEndTime = a}) . mapping _Time

-- | The type of OS that the model will run on after the compilation job has completed.
cjsCompilationTargetPlatformOS :: Lens' CompilationJobSummary (Maybe TargetPlatformOS)
cjsCompilationTargetPlatformOS = lens _cjsCompilationTargetPlatformOS (\s a -> s {_cjsCompilationTargetPlatformOS = a})

-- | The name of the model compilation job that you want a summary for.
cjsCompilationJobName :: Lens' CompilationJobSummary Text
cjsCompilationJobName = lens _cjsCompilationJobName (\s a -> s {_cjsCompilationJobName = a})

-- | The Amazon Resource Name (ARN) of the model compilation job.
cjsCompilationJobARN :: Lens' CompilationJobSummary Text
cjsCompilationJobARN = lens _cjsCompilationJobARN (\s a -> s {_cjsCompilationJobARN = a})

-- | The time when the model compilation job was created.
cjsCreationTime :: Lens' CompilationJobSummary UTCTime
cjsCreationTime = lens _cjsCreationTime (\s a -> s {_cjsCreationTime = a}) . _Time

-- | The status of the model compilation job.
cjsCompilationJobStatus :: Lens' CompilationJobSummary CompilationJobStatus
cjsCompilationJobStatus = lens _cjsCompilationJobStatus (\s a -> s {_cjsCompilationJobStatus = a})

instance FromJSON CompilationJobSummary where
  parseJSON =
    withObject
      "CompilationJobSummary"
      ( \x ->
          CompilationJobSummary'
            <$> (x .:? "CompilationStartTime")
            <*> (x .:? "CompilationTargetPlatformAccelerator")
            <*> (x .:? "CompilationTargetDevice")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "CompilationTargetPlatformArch")
            <*> (x .:? "CompilationEndTime")
            <*> (x .:? "CompilationTargetPlatformOs")
            <*> (x .: "CompilationJobName")
            <*> (x .: "CompilationJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "CompilationJobStatus")
      )

instance Hashable CompilationJobSummary

instance NFData CompilationJobSummary
