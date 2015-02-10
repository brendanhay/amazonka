{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.ListJobs
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

-- | This operation lists jobs for a vault, including jobs that are in-progress
-- and jobs that have recently finished.
--
-- Amazon Glacier retains recently completed jobs for a period before deleting
-- them; however, it eventually removes completed jobs. The output of completed
-- jobs can be retrieved. Retaining completed jobs for a period of time after
-- they have completed enables you to get a job output in the event you miss the
-- job completion notification or your first attempt to download it fails. For
-- example, suppose you start an archive retrieval job to download an archive.
-- After the job completes, you start to download the archive but encounter a
-- network error. In this scenario, you can retry and download the archive while
-- the job exists.
--
-- To retrieve an archive or retrieve a vault inventory from Amazon Glacier,
-- you first initiate a job, and after the job completes, you download the data.
-- For an archive retrieval, the output is the archive data, and for an
-- inventory retrieval, it is the inventory list. The List Job operation returns
-- a list of these jobs sorted by job initiation time.
--
-- This List Jobs operation supports pagination. By default, this operation
-- returns up to 1,000 jobs in the response. You should always check the
-- response for a 'marker' at which to continue the list; if there are no more
-- items the 'marker' is 'null'. To return a list of jobs that begins at a specific
-- job, set the 'marker' request parameter to the value you obtained from a
-- previous List Jobs request. You can also limit the number of jobs returned in
-- the response by specifying the 'limit' parameter in the request.
--
-- Additionally, you can filter the jobs list returned by specifying an
-- optional 'statuscode' (InProgress, Succeeded, or Failed) and 'completed' (true,
-- false) parameter. The 'statuscode' allows you to specify that only jobs that
-- match a specified status are returned. The 'completed' parameter allows you to
-- specify that only jobs in a specific completion state are returned.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For the underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html List Jobs >
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListJobs.html>
module Network.AWS.Glacier.ListJobs
    (
    -- * Request
      ListJobs
    -- ** Request constructor
    , listJobs
    -- ** Request lenses
    , ljAccountId
    , ljCompleted
    , ljLimit
    , ljMarker
    , ljStatuscode
    , ljVaultName

    -- * Response
    , ListJobsResponse
    -- ** Response constructor
    , listJobsResponse
    -- ** Response lenses
    , ljrJobList
    , ljrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data ListJobs = ListJobs
    { _ljAccountId  :: Text
    , _ljCompleted  :: Maybe Text
    , _ljLimit      :: Maybe Text
    , _ljMarker     :: Maybe Text
    , _ljStatuscode :: Maybe Text
    , _ljVaultName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListJobs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljAccountId' @::@ 'Text'
--
-- * 'ljCompleted' @::@ 'Maybe' 'Text'
--
-- * 'ljLimit' @::@ 'Maybe' 'Text'
--
-- * 'ljMarker' @::@ 'Maybe' 'Text'
--
-- * 'ljStatuscode' @::@ 'Maybe' 'Text'
--
-- * 'ljVaultName' @::@ 'Text'
--
listJobs :: Text -- ^ 'ljAccountId'
         -> Text -- ^ 'ljVaultName'
         -> ListJobs
listJobs p1 p2 = ListJobs
    { _ljAccountId  = p1
    , _ljVaultName  = p2
    , _ljLimit      = Nothing
    , _ljMarker     = Nothing
    , _ljStatuscode = Nothing
    , _ljCompleted  = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
ljAccountId :: Lens' ListJobs Text
ljAccountId = lens _ljAccountId (\s a -> s { _ljAccountId = a })

-- | Specifies the state of the jobs to return. You can specify 'true' or 'false'.
ljCompleted :: Lens' ListJobs (Maybe Text)
ljCompleted = lens _ljCompleted (\s a -> s { _ljCompleted = a })

-- | Specifies that the response be limited to the specified number of items or
-- fewer. If not specified, the List Jobs operation returns up to 1,000 jobs.
ljLimit :: Lens' ListJobs (Maybe Text)
ljLimit = lens _ljLimit (\s a -> s { _ljLimit = a })

-- | An opaque string used for pagination. This value specifies the job at which
-- the listing of jobs should begin. Get the marker value from a previous List
-- Jobs response. You need only include the marker if you are continuing the
-- pagination of results started in a previous List Jobs request.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\s a -> s { _ljMarker = a })

-- | Specifies the type of job status to return. You can specify the following
-- values: "InProgress", "Succeeded", or "Failed".
ljStatuscode :: Lens' ListJobs (Maybe Text)
ljStatuscode = lens _ljStatuscode (\s a -> s { _ljStatuscode = a })

-- | The name of the vault.
ljVaultName :: Lens' ListJobs Text
ljVaultName = lens _ljVaultName (\s a -> s { _ljVaultName = a })

data ListJobsResponse = ListJobsResponse
    { _ljrJobList :: List "JobList" GlacierJobDescription
    , _ljrMarker  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListJobsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljrJobList' @::@ ['GlacierJobDescription']
--
-- * 'ljrMarker' @::@ 'Maybe' 'Text'
--
listJobsResponse :: ListJobsResponse
listJobsResponse = ListJobsResponse
    { _ljrJobList = mempty
    , _ljrMarker  = Nothing
    }

-- | A list of job objects. Each job object contains metadata describing the job.
ljrJobList :: Lens' ListJobsResponse [GlacierJobDescription]
ljrJobList = lens _ljrJobList (\s a -> s { _ljrJobList = a }) . _List

-- | An opaque string that represents where to continue pagination of the results.
-- You use this value in a new List Jobs request to obtain more jobs in the
-- list. If there are no more jobs, this value is 'null'.
ljrMarker :: Lens' ListJobsResponse (Maybe Text)
ljrMarker = lens _ljrMarker (\s a -> s { _ljrMarker = a })

instance ToPath ListJobs where
    toPath ListJobs{..} = mconcat
        [ "/"
        , toText _ljAccountId
        , "/vaults/"
        , toText _ljVaultName
        , "/jobs"
        ]

instance ToQuery ListJobs where
    toQuery ListJobs{..} = mconcat
        [ "limit"      =? _ljLimit
        , "marker"     =? _ljMarker
        , "statuscode" =? _ljStatuscode
        , "completed"  =? _ljCompleted
        ]

instance ToHeaders ListJobs

instance ToJSON ListJobs where
    toJSON = const (toJSON Empty)

instance AWSRequest ListJobs where
    type Sv ListJobs = Glacier
    type Rs ListJobs = ListJobsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListJobsResponse where
    parseJSON = withObject "ListJobsResponse" $ \o -> ListJobsResponse
        <$> o .:? "JobList" .!= mempty
        <*> o .:? "Marker"
