{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ImportExport.Types
    (
    -- * Service
      ImportExport
    -- ** Error
    , RESTError

    -- * JobType
    , JobType (..)

    -- * Job
    , Job
    , job
    , jobCreationDate
    , jobIsCanceled
    , jobJobId
    , jobJobType
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V2
import qualified GHC.Exts

-- | Version @2010-06-01@ of the Amazon Import/Export service.
data ImportExport

instance AWSService ImportExport where
    type Sg ImportExport = V2
    type Er ImportExport = RESTError

    service = Service
        { _svcEndpoint     = global
        , _svcAbbrev       = "ImportExport"
        , _svcPrefix       = "importexport"
        , _svcVersion      = "2010-06-01"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        }

    handle = restError alwaysFail

data JobType
    = Export' -- ^ Export
    | Import' -- ^ Import
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable JobType

instance FromText JobType where
    parser = match "Export" Export'
         <|> match "Import" Import'

instance ToText JobType where
    toText = \case
        Export' -> "Export"
        Import' -> "Import"

instance FromXML JobType where
    parseXML = withElement "JobType" (const (return JobType))

instance ToQuery JobType

data Job = Job
    { _jobCreationDate :: RFC822
    , _jobIsCanceled   :: Bool
    , _jobJobId        :: Text
    , _jobJobType      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Job' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jobCreationDate' @::@ 'UTCTime'
--
-- * 'jobIsCanceled' @::@ 'Bool'
--
-- * 'jobJobId' @::@ 'Text'
--
-- * 'jobJobType' @::@ 'Text'
--
job :: Text -- ^ 'jobJobId'
    -> UTCTime -- ^ 'jobCreationDate'
    -> Bool -- ^ 'jobIsCanceled'
    -> Text -- ^ 'jobJobType'
    -> Job
job p1 p2 p3 p4 = Job
    { _jobJobId        = p1
    , _jobCreationDate = withIso _Time (const id) p2
    , _jobIsCanceled   = p3
    , _jobJobType      = p4
    }

jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\s a -> s { _jobCreationDate = a })
    . _Time

jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\s a -> s { _jobIsCanceled = a })

jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\s a -> s { _jobJobId = a })

jobJobType :: Lens' Job Text
jobJobType = lens _jobJobType (\s a -> s { _jobJobType = a })

instance FromXML Job where
    parseXML x = Job
        <$> x .@ "CreationDate"
        <*> x .@ "IsCanceled"
        <*> x .@ "JobId"
        <*> x .@ "JobType"

instance ToQuery Job
