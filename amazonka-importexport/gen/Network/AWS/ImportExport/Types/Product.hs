{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types.Product where

import           Network.AWS.ImportExport.Types.Sum
import           Network.AWS.Prelude

-- | A discrete item that contains the description and URL of an artifact
-- (such as a PDF).
--
-- /See:/ 'artifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artURL'
--
-- * 'artDescription'
data Artifact = Artifact'
    { _artURL         :: !(Maybe Text)
    , _artDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Artifact' smart constructor.
artifact :: Artifact
artifact =
    Artifact'
    { _artURL = Nothing
    , _artDescription = Nothing
    }

-- | FIXME: Undocumented member.
artURL :: Lens' Artifact (Maybe Text)
artURL = lens _artURL (\ s a -> s{_artURL = a});

-- | FIXME: Undocumented member.
artDescription :: Lens' Artifact (Maybe Text)
artDescription = lens _artDescription (\ s a -> s{_artDescription = a});

instance FromXML Artifact where
        parseXML x
          = Artifact' <$>
              (x .@? "URL") <*> (x .@? "Description")

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'job' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jobJobType'
--
-- * 'jobJobId'
--
-- * 'jobIsCanceled'
--
-- * 'jobCreationDate'
data Job = Job'
    { _jobJobType      :: !JobType
    , _jobJobId        :: !Text
    , _jobIsCanceled   :: !Bool
    , _jobCreationDate :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Job' smart constructor.
job :: JobType -> Text -> Bool -> UTCTime -> Job
job pJobType pJobId pIsCanceled pCreationDate =
    Job'
    { _jobJobType = pJobType
    , _jobJobId = pJobId
    , _jobIsCanceled = pIsCanceled
    , _jobCreationDate = _Time # pCreationDate
    }

-- | FIXME: Undocumented member.
jobJobType :: Lens' Job JobType
jobJobType = lens _jobJobType (\ s a -> s{_jobJobType = a});

-- | FIXME: Undocumented member.
jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\ s a -> s{_jobJobId = a});

-- | FIXME: Undocumented member.
jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\ s a -> s{_jobIsCanceled = a});

-- | FIXME: Undocumented member.
jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\ s a -> s{_jobCreationDate = a}) . _Time;

instance FromXML Job where
        parseXML x
          = Job' <$>
              (x .@ "JobType") <*> (x .@ "JobId") <*>
                (x .@ "IsCanceled")
                <*> (x .@ "CreationDate")
