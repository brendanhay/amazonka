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
      UpdateDistribution2014_05_31
    -- ** Request constructor
    , updateDistribution2014_05_31
    -- ** Request lenses
    , udDistributionConfig
    , udId
    , udIfMatch

    -- * Response
    , UpdateDistribution2014_05_31Response
    -- ** Response constructor
    , updateDistribution2014_05_31Response
    -- ** Response lenses
    , udrDistribution
    , udrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data UpdateDistribution2014_05_31 = UpdateDistribution2014_05_31
    { _udDistributionConfig :: DistributionConfig
    , _udId                 :: Text
    , _udIfMatch            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udDistributionConfig' @::@ 'DistributionConfig'
--
-- * 'udId' @::@ 'Text'
--
-- * 'udIfMatch' @::@ 'Maybe' 'Text'
--
updateDistribution2014_05_31 :: DistributionConfig -- ^ 'udDistributionConfig'
                             -> Text -- ^ 'udId'
                             -> UpdateDistribution2014_05_31
updateDistribution2014_05_31 p1 p2 = UpdateDistribution2014_05_31
    { _udDistributionConfig = p1
    , _udId                 = p2
    , _udIfMatch            = Nothing
    }

-- | The distribution's configuration information.
udDistributionConfig :: Lens' UpdateDistribution2014_05_31 DistributionConfig
udDistributionConfig =
    lens _udDistributionConfig (\s a -> s { _udDistributionConfig = a })

-- | The distribution's id.
udId :: Lens' UpdateDistribution2014_05_31 Text
udId = lens _udId (\s a -> s { _udId = a })

-- | The value of the ETag header you received when retrieving the
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
udIfMatch :: Lens' UpdateDistribution2014_05_31 (Maybe Text)
udIfMatch = lens _udIfMatch (\s a -> s { _udIfMatch = a })

instance ToPath UpdateDistribution2014_05_31 where
    toPath UpdateDistribution2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _udId
        , "/config"
        ]

instance ToQuery UpdateDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders UpdateDistribution2014_05_31 where
    toHeaders UpdateDistribution2014_05_31{..} = mconcat
        [ "If-Match" =: _udIfMatch
        ]

instance ToBody UpdateDistribution2014_05_31 where
    toBody = toBody . encodeXML . _udDistributionConfig

data UpdateDistribution2014_05_31Response = UpdateDistribution2014_05_31Response
    { _udrDistribution :: Maybe Distribution
    , _udrETag         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'udrETag' @::@ 'Maybe' 'Text'
--
updateDistribution2014_05_31Response :: UpdateDistribution2014_05_31Response
updateDistribution2014_05_31Response = UpdateDistribution2014_05_31Response
    { _udrDistribution = Nothing
    , _udrETag         = Nothing
    }

-- | The distribution's information.
udrDistribution :: Lens' UpdateDistribution2014_05_31Response (Maybe Distribution)
udrDistribution = lens _udrDistribution (\s a -> s { _udrDistribution = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrETag :: Lens' UpdateDistribution2014_05_31Response (Maybe Text)
udrETag = lens _udrETag (\s a -> s { _udrETag = a })

instance FromXML UpdateDistribution2014_05_31Response where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateDistribution2014_05_31Response"
instance AWSRequest UpdateDistribution2014_05_31 where
    type Sv UpdateDistribution2014_05_31 = CloudFront
    type Rs UpdateDistribution2014_05_31 = UpdateDistribution2014_05_31Response

    request  = put
    response = xmlResponse $ \h x -> UpdateDistribution2014_05_31Response
        <$> x %| "Distribution"
        <*> h ~:? "ETag"
