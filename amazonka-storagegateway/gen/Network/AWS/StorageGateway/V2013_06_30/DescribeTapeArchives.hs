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
    , mkDescribeTapeArchivesInput
    -- ** Request lenses
    , dtajTapeARNs
    , dtajMarker
    , dtajLimit

    -- * Response
    , DescribeTapeArchivesResponse
    -- ** Response lenses
    , dtapTapeArchives
    , dtapMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTapeArchives' request.
mkDescribeTapeArchivesInput :: DescribeTapeArchives
mkDescribeTapeArchivesInput = DescribeTapeArchives
    { _dtajTapeARNs = mempty
    , _dtajMarker = Nothing
    , _dtajLimit = Nothing
    }
{-# INLINE mkDescribeTapeArchivesInput #-}

data DescribeTapeArchives = DescribeTapeArchives
    { _dtajTapeARNs :: [Text]
    , _dtajMarker :: Maybe Text
    , _dtajLimit :: Maybe Integer
    } deriving (Show, Generic)

dtajTapeARNs :: Lens' DescribeTapeArchives ([Text])
dtajTapeARNs = lens _dtajTapeARNs (\s a -> s { _dtajTapeARNs = a })
{-# INLINE dtajTapeARNs #-}

dtajMarker :: Lens' DescribeTapeArchives (Maybe Text)
dtajMarker = lens _dtajMarker (\s a -> s { _dtajMarker = a })
{-# INLINE dtajMarker #-}

dtajLimit :: Lens' DescribeTapeArchives (Maybe Integer)
dtajLimit = lens _dtajLimit (\s a -> s { _dtajLimit = a })
{-# INLINE dtajLimit #-}

instance ToPath DescribeTapeArchives

instance ToQuery DescribeTapeArchives

instance ToHeaders DescribeTapeArchives

instance ToJSON DescribeTapeArchives

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtapTapeArchives :: [TapeArchive]
    , _dtapMarker :: Maybe Text
    } deriving (Show, Generic)

dtapTapeArchives :: Lens' DescribeTapeArchivesResponse ([TapeArchive])
dtapTapeArchives = lens _dtapTapeArchives (\s a -> s { _dtapTapeArchives = a })
{-# INLINE dtapTapeArchives #-}

dtapMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtapMarker = lens _dtapMarker (\s a -> s { _dtapMarker = a })
{-# INLINE dtapMarker #-}

instance FromJSON DescribeTapeArchivesResponse

instance AWSRequest DescribeTapeArchives where
    type Sv DescribeTapeArchives = StorageGateway
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeArchives where
    next rq rs = (\x -> rq { _dtajMarker = Just x })
        <$> (_dtapMarker rs)
