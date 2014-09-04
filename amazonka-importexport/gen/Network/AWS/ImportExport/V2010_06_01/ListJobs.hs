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
    , listJobs
    -- ** Request lenses
    , ljiMarker
    , ljiMaxJobs

    -- * Response
    , ListJobsResponse
    -- ** Response lenses
    , ljoIsTruncated
    , ljoJobs
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListJobs' request.
listJobs :: ListJobs
listJobs = ListJobs
    { _ljiMarker = Nothing
    , _ljiMaxJobs = Nothing
    }
{-# INLINE listJobs #-}

data ListJobs = ListJobs
    { _ljiMarker :: Maybe Text
      -- ^ Specifies the JOBID to start after when listing the jobs created
      -- with your account. AWS Import/Export lists your jobs in reverse
      -- chronological order. See MaxJobs.
    , _ljiMaxJobs :: Maybe Integer
      -- ^ Sets the maximum number of jobs returned in the response. If
      -- there are additional jobs that were not returned because MaxJobs
      -- was exceeded, the response contains
      -- &lt;IsTruncated&gt;true&lt;/IsTruncated&gt;. To return the
      -- additional jobs, see Marker.
    } deriving (Show, Generic)

-- | Specifies the JOBID to start after when listing the jobs created with your
-- account. AWS Import/Export lists your jobs in reverse chronological order.
-- See MaxJobs.
ljiMarker :: Lens' ListJobs (Maybe Text)
ljiMarker f x =
    f (_ljiMarker x)
        <&> \y -> x { _ljiMarker = y }
{-# INLINE ljiMarker #-}

-- | Sets the maximum number of jobs returned in the response. If there are
-- additional jobs that were not returned because MaxJobs was exceeded, the
-- response contains &lt;IsTruncated&gt;true&lt;/IsTruncated&gt;. To return
-- the additional jobs, see Marker.
ljiMaxJobs :: Lens' ListJobs (Maybe Integer)
ljiMaxJobs f x =
    f (_ljiMaxJobs x)
        <&> \y -> x { _ljiMaxJobs = y }
{-# INLINE ljiMaxJobs #-}

instance ToQuery ListJobs where
    toQuery = genericQuery def

data ListJobsResponse = ListJobsResponse
    { _ljoIsTruncated :: Bool
      -- ^ Indicates whether the list of jobs was truncated. If true, then
      -- call ListJobs again using the last JobId element as the marker.
    , _ljoJobs :: [Job]
      -- ^ A list container for Jobs returned by the ListJobs operation.
    } deriving (Show, Generic)

-- | Indicates whether the list of jobs was truncated. If true, then call
-- ListJobs again using the last JobId element as the marker.
ljoIsTruncated :: Lens' ListJobsResponse (Bool)
ljoIsTruncated f x =
    f (_ljoIsTruncated x)
        <&> \y -> x { _ljoIsTruncated = y }
{-# INLINE ljoIsTruncated #-}

-- | A list container for Jobs returned by the ListJobs operation.
ljoJobs :: Lens' ListJobsResponse ([Job])
ljoJobs f x =
    f (_ljoJobs x)
        <&> \y -> x { _ljoJobs = y }
{-# INLINE ljoJobs #-}

instance FromXML ListJobsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListJobs where
    type Sv ListJobs = ImportExport
    type Rs ListJobs = ListJobsResponse

    request = post "ListJobs"
    response _ = xmlResponse

instance AWSPager ListJobs where
    next rq rs
        | not (_ljoIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _ljiMarker = keyed _jJobId _ljoJobs rs
            }
