{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.ListJobs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the jobs associated with the requester. AWS
-- Import/Export lists the jobs in reverse chronological order based on the
-- date of creation. For example if Job Test1 was created 2009Dec30 and Test2
-- was created 2010Feb05, the ListJobs operation would return Test2 followed
-- by Test1.
module Network.AWS.ImportExport.V2010_06_01.ListJobs
    (
    -- * Request
      ListJobs
    -- ** Request constructor
    , mkListJobs
    -- ** Request lenses
    , ljMaxJobs
    , ljMarker

    -- * Response
    , ListJobsResponse
    -- ** Response lenses
    , ljrsJobs
    , ljrsIsTruncated
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Input structure for the ListJobs operation.
data ListJobs = ListJobs
    { _ljMaxJobs :: Maybe Integer
    , _ljMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListJobs' request.
mkListJobs :: ListJobs
mkListJobs = ListJobs
    { _ljMaxJobs = Nothing
    , _ljMarker = Nothing
    }

-- | Sets the maximum number of jobs returned in the response. If there are
-- additional jobs that were not returned because MaxJobs was exceeded, the
-- response contains &lt;IsTruncated&gt;true&lt;/IsTruncated&gt;. To return
-- the additional jobs, see Marker.
ljMaxJobs :: Lens' ListJobs (Maybe Integer)
ljMaxJobs = lens _ljMaxJobs (\s a -> s { _ljMaxJobs = a })

-- | Specifies the JOBID to start after when listing the jobs created with your
-- account. AWS Import/Export lists your jobs in reverse chronological order.
-- See MaxJobs.
ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\s a -> s { _ljMarker = a })

instance ToQuery ListJobs where
    toQuery = genericQuery def

-- | Output structure for the ListJobs operation.
data ListJobsResponse = ListJobsResponse
    { _ljrsJobs :: [Job]
    , _ljrsIsTruncated :: Maybe Bool
    } deriving (Show, Generic)

-- | A list container for Jobs returned by the ListJobs operation.
ljrsJobs :: Lens' ListJobsResponse [Job]
ljrsJobs = lens _ljrsJobs (\s a -> s { _ljrsJobs = a })

-- | Indicates whether the list of jobs was truncated. If true, then call
-- ListJobs again using the last JobId element as the marker.
ljrsIsTruncated :: Lens' ListJobsResponse (Maybe Bool)
ljrsIsTruncated = lens _ljrsIsTruncated (\s a -> s { _ljrsIsTruncated = a })

instance FromXML ListJobsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListJobs where
    type Sv ListJobs = ImportExport
    type Rs ListJobs = ListJobsResponse

    request = post "ListJobs"
    response _ = xmlResponse

instance AWSPager ListJobs where
    next rq rs
        | not (rs ^. ljrsIsTruncated) = Nothing
        | otherwise = Just (rq & ljMarker .~ rs ^. keyed jJobId ljrsJobs)
