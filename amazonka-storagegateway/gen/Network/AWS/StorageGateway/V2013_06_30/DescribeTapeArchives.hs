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
    , describeTapeArchives
    -- ** Request lenses
    , dtajMarker
    , dtajLimit
    , dtajTapeARNs

    -- * Response
    , DescribeTapeArchivesResponse
    -- ** Response lenses
    , dtapMarker
    , dtapTapeArchives
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeTapeArchives' request.
describeTapeArchives :: DescribeTapeArchives
describeTapeArchives = DescribeTapeArchives
    { _dtajMarker = Nothing
    , _dtajLimit = Nothing
    , _dtajTapeARNs = mempty
    }

data DescribeTapeArchives = DescribeTapeArchives
    { _dtajMarker :: Maybe Text
    , _dtajLimit :: Maybe Integer
    , _dtajTapeARNs :: [Text]
    } deriving (Show, Generic)

dtajMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapeArchives
    -> f DescribeTapeArchives
dtajMarker f x =
    (\y -> x { _dtajMarker = y })
       <$> f (_dtajMarker x)
{-# INLINE dtajMarker #-}

dtajLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeTapeArchives
    -> f DescribeTapeArchives
dtajLimit f x =
    (\y -> x { _dtajLimit = y })
       <$> f (_dtajLimit x)
{-# INLINE dtajLimit #-}

dtajTapeARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeTapeArchives
    -> f DescribeTapeArchives
dtajTapeARNs f x =
    (\y -> x { _dtajTapeARNs = y })
       <$> f (_dtajTapeARNs x)
{-# INLINE dtajTapeARNs #-}

instance ToPath DescribeTapeArchives

instance ToQuery DescribeTapeArchives

instance ToHeaders DescribeTapeArchives

instance ToJSON DescribeTapeArchives

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtapMarker :: Maybe Text
    , _dtapTapeArchives :: [TapeArchive]
    } deriving (Show, Generic)

dtapMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeTapeArchivesResponse
    -> f DescribeTapeArchivesResponse
dtapMarker f x =
    (\y -> x { _dtapMarker = y })
       <$> f (_dtapMarker x)
{-# INLINE dtapMarker #-}

dtapTapeArchives
    :: Functor f
    => ([TapeArchive]
    -> f ([TapeArchive]))
    -> DescribeTapeArchivesResponse
    -> f DescribeTapeArchivesResponse
dtapTapeArchives f x =
    (\y -> x { _dtapTapeArchives = y })
       <$> f (_dtapTapeArchives x)
{-# INLINE dtapTapeArchives #-}

instance FromJSON DescribeTapeArchivesResponse

instance AWSRequest DescribeTapeArchives where
    type Sv DescribeTapeArchives = StorageGateway
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeArchives where
    next rq rs = (\x -> rq { _dtajMarker = Just x })
        <$> (_dtapMarker rs)
