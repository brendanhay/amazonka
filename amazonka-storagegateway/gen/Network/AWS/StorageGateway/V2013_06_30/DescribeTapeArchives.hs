{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

module Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeTapeArchives' request.
describeTapeArchives :: DescribeTapeArchives
describeTapeArchives = DescribeTapeArchives
    { _dtaiMarker = Nothing
    , _dtaiLimit = Nothing
    , _dtaiTapeARNs = mempty
    }

data DescribeTapeArchives = DescribeTapeArchives
    { _dtaiMarker :: Maybe Text
    , _dtaiLimit :: Maybe Integer
    , _dtaiTapeARNs :: [Text]
    } deriving (Show, Generic)

makeLenses ''DescribeTapeArchives

instance ToPath DescribeTapeArchives

instance ToQuery DescribeTapeArchives

instance ToHeaders DescribeTapeArchives

instance ToJSON DescribeTapeArchives

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { _dtaoMarker :: Maybe Text
    , _dtaoTapeArchives :: [TapeArchive]
    } deriving (Show, Generic)

makeLenses ''DescribeTapeArchivesResponse

instance FromJSON DescribeTapeArchivesResponse

instance AWSRequest DescribeTapeArchives where
    type Sv DescribeTapeArchives = StorageGateway
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeTapeArchives where
    next rq rs = (\x -> rq { _dtaiMarker = Just x })
        <$> (_dtaoMarker rs)
