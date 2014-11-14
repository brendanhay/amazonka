{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of specified virtual tapes in the virtual tape shelf
-- (VTS). If a specific TapeARN is not specified, AWS Storage Gateway returns
-- a description of all virtual tapes found in the VTS associated with your
-- account.
module Network.AWS.StorageGateway.DescribeTapeArchives
    (
    -- * Request
      DescribeTapeArchives
    -- ** Request constructor
    , describeTapeArchives
    -- ** Request lenses
    , dtaLimit
    , dtaMarker
    , dtaTapeARNs

    -- * Response
    , DescribeTapeArchivesResponse
    -- ** Response constructor
    , describeTapeArchivesResponse
    -- ** Response lenses
    , dtarMarker
    , dtarTapeArchives
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data DescribeTapeArchives = DescribeTapeArchives
    { _dtaLimit    :: Maybe Natural
    , _dtaMarker   :: Maybe Text
    , _dtaTapeARNs :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeTapeArchives' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dtaMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtaTapeARNs' @::@ ['Text']
--
describeTapeArchives :: DescribeTapeArchives
describeTapeArchives = DescribeTapeArchives
    { _dtaTapeARNs = mempty
    , _dtaMarker   = Nothing
    , _dtaLimit    = Nothing
    }

-- | Specifies that the number of virtual tapes descried be limited to the
-- specified number.
dtaLimit :: Lens' DescribeTapeArchives (Maybe Natural)
dtaLimit = lens _dtaLimit (\s a -> s { _dtaLimit = a })

-- | An opaque string that indicates the position at which to begin describing
-- virtual tapes.
dtaMarker :: Lens' DescribeTapeArchives (Maybe Text)
dtaMarker = lens _dtaMarker (\s a -> s { _dtaMarker = a })

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe.
dtaTapeARNs :: Lens' DescribeTapeArchives [Text]
dtaTapeARNs = lens _dtaTapeARNs (\s a -> s { _dtaTapeARNs = a })

instance ToPath DescribeTapeArchives where
    toPath = const "/"

instance ToQuery DescribeTapeArchives where
    toQuery = const mempty

instance ToHeaders DescribeTapeArchives

instance ToBody DescribeTapeArchives where
    toBody = toBody . encode . _dtaTapeARNs

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtarMarker       :: Maybe Text
    , _dtarTapeArchives :: [TapeArchive]
    } deriving (Eq, Show, Generic)

-- | 'DescribeTapeArchivesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtarMarker' @::@ 'Maybe' 'Text'
--
-- * 'dtarTapeArchives' @::@ ['TapeArchive']
--
describeTapeArchivesResponse :: DescribeTapeArchivesResponse
describeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtarTapeArchives = mempty
    , _dtarMarker       = Nothing
    }

-- | An opaque string that indicates the position at which the virtual tapes
-- that were fetched for description ended. Use this marker in your next
-- request to fetch the next set of virtual tapes in the virtual tape shelf
-- (VTS). If there are no more virtual tapes to describe, this field does
-- not appear in the response.
dtarMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtarMarker = lens _dtarMarker (\s a -> s { _dtarMarker = a })

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The
-- description includes of the Amazon Resource Name(ARN) of the virtual
-- tapes. The information returned includes the Amazon Resource Names (ARNs)
-- of the tapes, size of the tapes, status of the tapes, progress of the
-- description and tape barcode.
dtarTapeArchives :: Lens' DescribeTapeArchivesResponse [TapeArchive]
dtarTapeArchives = lens _dtarTapeArchives (\s a -> s { _dtarTapeArchives = a })

instance AWSRequest DescribeTapeArchives where
    type Sv DescribeTapeArchives = StorageGateway
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeTapeArchivesResponse
        <$> o .: "Marker"
        <*> o .: "TapeArchives"
