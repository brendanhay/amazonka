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

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a streaming distribution.
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution2014_05_31
    -- ** Request constructor
    , getStreamingDistribution2014_05_31
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistribution2014_05_31Response
    -- ** Response constructor
    , getStreamingDistribution2014_05_31Response
    -- ** Response lenses
    , gsdrETag
    , gsdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetStreamingDistribution2014_05_31 = GetStreamingDistribution2014_05_31
    { _gsdId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetStreamingDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdId' @::@ 'Text'
--
getStreamingDistribution2014_05_31 :: Text -- ^ 'gsdId'
                                   -> GetStreamingDistribution2014_05_31
getStreamingDistribution2014_05_31 p1 = GetStreamingDistribution2014_05_31
    { _gsdId = p1
    }

-- | The streaming distribution's id.
gsdId :: Lens' GetStreamingDistribution2014_05_31 Text
gsdId = lens _gsdId (\s a -> s { _gsdId = a })

instance ToPath GetStreamingDistribution2014_05_31 where
    toPath GetStreamingDistribution2014_05_31{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _gsdId
        ]

instance ToQuery GetStreamingDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetStreamingDistribution2014_05_31

data GetStreamingDistribution2014_05_31Response = GetStreamingDistribution2014_05_31Response
    { _gsdrETag                  :: Maybe Text
    , _gsdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'GetStreamingDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdrETag' @::@ 'Maybe' 'Text'
--
-- * 'gsdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
getStreamingDistribution2014_05_31Response :: GetStreamingDistribution2014_05_31Response
getStreamingDistribution2014_05_31Response = GetStreamingDistribution2014_05_31Response
    { _gsdrStreamingDistribution = Nothing
    , _gsdrETag                  = Nothing
    }

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistribution2014_05_31Response (Maybe Text)
gsdrETag = lens _gsdrETag (\s a -> s { _gsdrETag = a })

-- | The streaming distribution's information.
gsdrStreamingDistribution :: Lens' GetStreamingDistribution2014_05_31Response (Maybe StreamingDistribution)
gsdrStreamingDistribution =
    lens _gsdrStreamingDistribution
        (\s a -> s { _gsdrStreamingDistribution = a })

instance AWSRequest GetStreamingDistribution2014_05_31 where
    type Sv GetStreamingDistribution2014_05_31 = CloudFront
    type Rs GetStreamingDistribution2014_05_31 = GetStreamingDistribution2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetStreamingDistribution2014_05_31Response
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistribution"
