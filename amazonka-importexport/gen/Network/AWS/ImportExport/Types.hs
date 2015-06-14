{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ImportExport.Types
    (
    -- * Service
      ImportExport
    -- ** Errors
    , RESTError

    -- * Artifact
    , Artifact
    , artifact
    , artURL
    , artDescription

    -- * Job
    , Job
    , job
    , jobJobType
    , jobJobId
    , jobIsCanceled
    , jobCreationDate

    -- * JobType
    , JobType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V2

-- | Version @2010-06-01@ of the Amazon Import/Export SDK.
data ImportExport

instance AWSService ImportExport where
    type Sg ImportExport = V2
    type Er ImportExport = RESTError

    service = service'
      where
        service' :: Service ImportExport
        service' = Service
            { _svcAbbrev  = "ImportExport"
            , _svcPrefix  = "importexport"
            , _svcVersion = "2010-06-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry ImportExport
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'artifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artURL'
--
-- * 'artDescription'
data Artifact = Artifact'{_artURL :: Maybe Text, _artDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Artifact' smart constructor.
artifact :: Artifact
artifact = Artifact'{_artURL = Nothing, _artDescription = Nothing};

-- | FIXME: Undocumented member.
artURL :: Lens' Artifact (Maybe Text)
artURL = lens _artURL (\ s a -> s{_artURL = a});

-- | FIXME: Undocumented member.
artDescription :: Lens' Artifact (Maybe Text)
artDescription = lens _artDescription (\ s a -> s{_artDescription = a});

instance FromXML Artifact where
        parseXML x
          = Artifact' <$> x .@? "URL" <*> x .@? "Description"

-- | /See:/ 'job' smart constructor.
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
data Job = Job'{_jobJobType :: JobType, _jobJobId :: Text, _jobIsCanceled :: Bool, _jobCreationDate :: ISO8601} deriving (Eq, Read, Show)

-- | 'Job' smart constructor.
job :: JobType -> Text -> Bool -> UTCTime -> Job
job pJobType pJobId pIsCanceled pCreationDate = Job'{_jobJobType = pJobType, _jobJobId = pJobId, _jobIsCanceled = pIsCanceled, _jobCreationDate = _Time # pCreationDate};

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
              x .@ "JobType" <*> x .@ "JobId" <*> x .@ "IsCanceled"
                <*> x .@ "CreationDate"

data JobType = Export | Import deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText JobType where
    parser = takeLowerText >>= \case
        "Export" -> pure Export
        "Import" -> pure Import
        e -> fail ("Failure parsing JobType from " ++ show e)

instance ToText JobType where
    toText = \case
        Export -> "Export"
        Import -> "Import"

instance Hashable JobType
instance ToQuery JobType
instance ToHeader JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"
