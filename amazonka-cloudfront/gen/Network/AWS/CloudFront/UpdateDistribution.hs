{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a distribution.
module Network.AWS.CloudFront.UpdateDistribution
    (
    -- * Request
      UpdateDistribution
    -- ** Request constructor
    , updateDistribution
    -- ** Request lenses
    , udDistributionConfig
    , udId
    , udIfMatch

    -- * Response
    , UpdateDistributionResponse
    -- ** Response constructor
    , updateDistributionResponse
    -- ** Response lenses
    , udrDistribution
    , udrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data UpdateDistribution = UpdateDistribution
    { _udDistributionConfig :: DistributionConfig
    , _udId                 :: Text
    , _udIfMatch            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udDistributionConfig' @::@ 'DistributionConfig'
--
-- * 'udId' @::@ 'Text'
--
-- * 'udIfMatch' @::@ 'Maybe' 'Text'
--
updateDistribution :: DistributionConfig -- ^ 'udDistributionConfig'
                   -> Text -- ^ 'udId'
                   -> UpdateDistribution
updateDistribution p1 p2 = UpdateDistribution
    { _udDistributionConfig = p1
    , _udId                 = p2
    , _udIfMatch            = Nothing
    }

-- | The distribution's configuration information.
udDistributionConfig :: Lens' UpdateDistribution DistributionConfig
udDistributionConfig =
    lens _udDistributionConfig (\s a -> s { _udDistributionConfig = a })

-- | The distribution's id.
udId :: Lens' UpdateDistribution Text
udId = lens _udId (\s a -> s { _udId = a })

-- | The value of the ETag header you received when retrieving the
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
udIfMatch :: Lens' UpdateDistribution (Maybe Text)
udIfMatch = lens _udIfMatch (\s a -> s { _udIfMatch = a })

data UpdateDistributionResponse = UpdateDistributionResponse
    { _udrDistribution :: Maybe Distribution
    , _udrETag         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'udrETag' @::@ 'Maybe' 'Text'
--
updateDistributionResponse :: UpdateDistributionResponse
updateDistributionResponse = UpdateDistributionResponse
    { _udrDistribution = Nothing
    , _udrETag         = Nothing
    }

-- | The distribution's information.
udrDistribution :: Lens' UpdateDistributionResponse (Maybe Distribution)
udrDistribution = lens _udrDistribution (\s a -> s { _udrDistribution = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrETag :: Lens' UpdateDistributionResponse (Maybe Text)
udrETag = lens _udrETag (\s a -> s { _udrETag = a })

instance AWSRequest UpdateDistribution where
    type Sv UpdateDistribution = CloudFront
    type Rs UpdateDistribution = UpdateDistributionResponse

    request  = put
    response = xmlHeaderResponse $ \h x -> UpdateDistributionResponse
        <$> x %| "Distribution"
        <*> h ~:? "ETag"

instance ToPath UpdateDistribution where
    toPath UpdateDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _udId
        , "/config"
        ]

instance ToHeaders UpdateDistribution where
    toHeaders UpdateDistribution{..} = mconcat
        [ "If-Match" =: _udIfMatch
        ]

instance ToQuery UpdateDistribution where
    toQuery = const mempty

instance ToXML UpdateDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateDistribution"
