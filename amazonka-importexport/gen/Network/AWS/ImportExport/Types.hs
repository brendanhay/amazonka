{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ImportExport.Types
    (
    -- * Service
      ImportExport
    -- ** Error
    , RESTError
    -- ** XML
    , ns

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

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2010-06-01@ of the Amazon Import/Export service.
data ImportExport

instance AWSService ImportExport where
    type Sg ImportExport = V2
    type Er ImportExport = RESTError

    service = service'
      where
        service' :: Service ImportExport
        service' = Service
              { _svcAbbrev       = "ImportExport"
              , _svcPrefix       = "importexport"
              , _svcVersion      = "2010-06-01"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry RESTError
        retry = Retry
            { _rPolicy = exponentialBackon 0.05 2 5
            , _rCheck  = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://importexport.amazonaws.com/doc/2010-06-01/"
{-# INLINE ns #-}

data JobType
    = Export' -- ^ Export
    | Import' -- ^ Import
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable JobType

instance FromText JobType where
    parser = takeText >>= \case
        "Export" -> pure Export'
        "Import" -> pure Import'
        e        -> fail $
            "Failure parsing JobType from " ++ show e

instance ToText JobType where
    toText = \case
        Export' -> "Export"
        Import' -> "Import"

instance ToByteString JobType
instance ToHeader     JobType
instance ToQuery      JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"

data Job = Job
    { _jobCreationDate :: ISO8601
    , _jobIsCanceled   :: Bool
    , _jobJobId        :: Text
    , _jobJobType      :: JobType
    } deriving (Eq, Show)

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
-- * 'jobJobType' @::@ 'JobType'
--
job :: Text -- ^ 'jobJobId'
    -> UTCTime -- ^ 'jobCreationDate'
    -> Bool -- ^ 'jobIsCanceled'
    -> JobType -- ^ 'jobJobType'
    -> Job
job p1 p2 p3 p4 = Job
    { _jobJobId        = p1
    , _jobCreationDate = withIso _Time (const id) p2
    , _jobIsCanceled   = p3
    , _jobJobType      = p4
    }

jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\s a -> s { _jobCreationDate = a }) . _Time

jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\s a -> s { _jobIsCanceled = a })

jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\s a -> s { _jobJobId = a })

jobJobType :: Lens' Job JobType
jobJobType = lens _jobJobType (\s a -> s { _jobJobType = a })

instance FromXML Job where
    parseXML x = Job
        <$> x .@  "CreationDate"
        <*> x .@  "IsCanceled"
        <*> x .@  "JobId"
        <*> x .@  "JobType"

instance ToQuery Job where
    toQuery Job{..} = mconcat
        [ "CreationDate" =? _jobCreationDate
        , "IsCanceled"   =? _jobIsCanceled
        , "JobId"        =? _jobJobId
        , "JobType"      =? _jobJobType
        ]
