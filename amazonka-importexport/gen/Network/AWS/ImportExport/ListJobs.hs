{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ImportExport.ListJobs
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
module Network.AWS.ImportExport.ListJobs
    (
    -- * Request
      ListJobsInput
    -- ** Request constructor
    , listJobsInput
    -- ** Request lenses
    , ljiMarker
    , ljiMaxJobs

    -- * Response
    , ListJobsOutput
    -- ** Response constructor
    , listJobsOutput
    -- ** Response lenses
    , ljoIsTruncated
    , ljoJobs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types

data ListJobsInput = ListJobsInput
    { _ljiMarker  :: Maybe Text
    , _ljiMaxJobs :: Maybe Int
    } (Eq, Ord, Show, Generic)

-- | 'ListJobsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljiMarker' @::@ 'Maybe' 'Text'
--
-- * 'ljiMaxJobs' @::@ 'Maybe' 'Int'
--
listJobsInput :: ListJobsInput
listJobsInput = ListJobsInput
    { _ljiMaxJobs = Nothing
    , _ljiMarker  = Nothing
    }

ljiMarker :: Lens' ListJobsInput (Maybe Text)
ljiMarker = lens _ljiMarker (\s a -> s { _ljiMarker = a })

ljiMaxJobs :: Lens' ListJobsInput (Maybe Int)
ljiMaxJobs = lens _ljiMaxJobs (\s a -> s { _ljiMaxJobs = a })
instance ToQuery ListJobsInput

instance ToPath ListJobsInput where
    toPath = const "/"

data ListJobsOutput = ListJobsOutput
    { _ljoIsTruncated :: Maybe Bool
    , _ljoJobs        :: [Job]
    } (Eq, Show, Generic)

-- | 'ListJobsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljoIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'ljoJobs' @::@ ['Job']
--
listJobsOutput :: ListJobsOutput
listJobsOutput = ListJobsOutput
    { _ljoJobs        = mempty
    , _ljoIsTruncated = Nothing
    }

ljoIsTruncated :: Lens' ListJobsOutput (Maybe Bool)
ljoIsTruncated = lens _ljoIsTruncated (\s a -> s { _ljoIsTruncated = a })

ljoJobs :: Lens' ListJobsOutput [Job]
ljoJobs = lens _ljoJobs (\s a -> s { _ljoJobs = a })

instance FromXML ListJobsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListJobsOutput"

instance AWSRequest ListJobsInput where
    type Sv ListJobsInput = ImportExport
    type Rs ListJobsInput = ListJobsOutput

    request  = post "ListJobs"
    response = xmlResponse $ \h x -> ListJobsOutput
        <$> x %| "IsTruncated"
        <*> x %| "Jobs"
