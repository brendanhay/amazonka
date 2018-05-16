{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.Product where

import Network.AWS.ImportExport.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A discrete item that contains the description and URL of an artifact (such as a PDF).
--
-- /See:/ 'artifact' smart constructor.
data Artifact = Artifact'
  { _aURL         :: !(Maybe Text)
  , _aDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Artifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aURL' - Undocumented member.
--
-- * 'aDescription' - Undocumented member.
artifact
    :: Artifact
artifact = Artifact' {_aURL = Nothing, _aDescription = Nothing}


-- | Undocumented member.
aURL :: Lens' Artifact (Maybe Text)
aURL = lens _aURL (\ s a -> s{_aURL = a})

-- | Undocumented member.
aDescription :: Lens' Artifact (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a})

instance FromXML Artifact where
        parseXML x
          = Artifact' <$>
              (x .@? "URL") <*> (x .@? "Description")

instance Hashable Artifact where

instance NFData Artifact where

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jobJobType      :: !JobType
  , _jobJobId        :: !Text
  , _jobIsCanceled   :: !Bool
  , _jobCreationDate :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
job
    :: JobType -- ^ 'jobJobType'
    -> Text -- ^ 'jobJobId'
    -> Bool -- ^ 'jobIsCanceled'
    -> UTCTime -- ^ 'jobCreationDate'
    -> Job
job pJobType_ pJobId_ pIsCanceled_ pCreationDate_ =
  Job'
    { _jobJobType = pJobType_
    , _jobJobId = pJobId_
    , _jobIsCanceled = pIsCanceled_
    , _jobCreationDate = _Time # pCreationDate_
    }


-- | Undocumented member.
jobJobType :: Lens' Job JobType
jobJobType = lens _jobJobType (\ s a -> s{_jobJobType = a})

-- | Undocumented member.
jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\ s a -> s{_jobJobId = a})

-- | Undocumented member.
jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\ s a -> s{_jobIsCanceled = a})

-- | Undocumented member.
jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\ s a -> s{_jobCreationDate = a}) . _Time

instance FromXML Job where
        parseXML x
          = Job' <$>
              (x .@ "JobType") <*> (x .@ "JobId") <*>
                (x .@ "IsCanceled")
                <*> (x .@ "CreationDate")

instance Hashable Job where

instance NFData Job where
