{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives
    (
    -- * Request
      DescribeTapeArchives
    -- ** Request constructor
    , mkDescribeTapeArchives
    -- ** Request lenses
    , dta1TapeARNs
    , dta1Marker
    , dta1Limit

    -- * Response
    , DescribeTapeArchivesResponse
    -- ** Response lenses
    , dtarsrsTapeArchives
    , dtarsrsMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeTapeArchives = DescribeTapeArchives
    { _dta1TapeARNs :: [Text]
    , _dta1Marker :: Maybe Text
    , _dta1Limit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeArchives' request.
mkDescribeTapeArchives :: DescribeTapeArchives
mkDescribeTapeArchives = DescribeTapeArchives
    { _dta1TapeARNs = mempty
    , _dta1Marker = Nothing
    , _dta1Limit = Nothing
    }

dta1TapeARNs :: Lens' DescribeTapeArchives [Text]
dta1TapeARNs = lens _dta1TapeARNs (\s a -> s { _dta1TapeARNs = a })

dta1Marker :: Lens' DescribeTapeArchives (Maybe Text)
dta1Marker = lens _dta1Marker (\s a -> s { _dta1Marker = a })

dta1Limit :: Lens' DescribeTapeArchives (Maybe Integer)
dta1Limit = lens _dta1Limit (\s a -> s { _dta1Limit = a })

instance ToPath DescribeTapeArchives

instance ToQuery DescribeTapeArchives

instance ToHeaders DescribeTapeArchives

instance ToJSON DescribeTapeArchives

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtarsrsTapeArchives :: [TapeArchive]
    , _dtarsrsMarker :: Maybe Text
    } deriving (Show, Generic)

dtarsrsTapeArchives :: Lens' DescribeTapeArchivesResponse [TapeArchive]
dtarsrsTapeArchives =
    lens _dtarsrsTapeArchives (\s a -> s { _dtarsrsTapeArchives = a })

dtarsrsMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtarsrsMarker = lens _dtarsrsMarker (\s a -> s { _dtarsrsMarker = a })

instance FromJSON DescribeTapeArchivesResponse

instance AWSRequest DescribeTapeArchives where
    type Sv DescribeTapeArchives = StorageGateway
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeArchives where
    next rq rs = (\x -> rq & dta1Marker ?~ x) <$> (rs ^. dtarsrsMarker)

