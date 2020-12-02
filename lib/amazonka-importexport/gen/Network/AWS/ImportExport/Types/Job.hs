{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Job where

import Network.AWS.ImportExport.Types.JobType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jobJobType :: !JobType,
    _jobJobId :: !Text,
    _jobIsCanceled :: !Bool,
    _jobCreationDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobJobType' - Undocumented member.
--
-- * 'jobJobId' - Undocumented member.
--
-- * 'jobIsCanceled' - Undocumented member.
--
-- * 'jobCreationDate' - Undocumented member.
job ::
  -- | 'jobJobType'
  JobType ->
  -- | 'jobJobId'
  Text ->
  -- | 'jobIsCanceled'
  Bool ->
  -- | 'jobCreationDate'
  UTCTime ->
  Job
job pJobType_ pJobId_ pIsCanceled_ pCreationDate_ =
  Job'
    { _jobJobType = pJobType_,
      _jobJobId = pJobId_,
      _jobIsCanceled = pIsCanceled_,
      _jobCreationDate = _Time # pCreationDate_
    }

-- | Undocumented member.
jobJobType :: Lens' Job JobType
jobJobType = lens _jobJobType (\s a -> s {_jobJobType = a})

-- | Undocumented member.
jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\s a -> s {_jobJobId = a})

-- | Undocumented member.
jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\s a -> s {_jobIsCanceled = a})

-- | Undocumented member.
jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\s a -> s {_jobCreationDate = a}) . _Time

instance FromXML Job where
  parseXML x =
    Job'
      <$> (x .@ "JobType")
      <*> (x .@ "JobId")
      <*> (x .@ "IsCanceled")
      <*> (x .@ "CreationDate")

instance Hashable Job

instance NFData Job
