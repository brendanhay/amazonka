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

-- Module      : Network.AWS.CloudFront.UpdateDistribution2014_05_31
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a distribution.
module Network.AWS.CloudFront.UpdateDistribution2014_05_31
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
    , UpdateDistributionResult
    -- ** Response constructor
    , updateDistributionResult
    -- ** Response lenses
    , udrDistribution
    , udrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

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

instance ToPath UpdateDistribution where
    toPath UpdateDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _udId
        , "/config"
        ]

instance ToQuery UpdateDistribution where
    toQuery = const mempty

instance ToHeaders UpdateDistribution where
    toHeaders UpdateDistribution{..} = mconcat
        [ "If-Match" =: _udIfMatch
        ]

instance ToBody UpdateDistribution where
    toBody = toBody . encodeXML . _udDistributionConfig

data UpdateDistributionResult = UpdateDistributionResult
    { _udrDistribution :: Maybe Distribution
    , _udrETag         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateDistributionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'udrETag' @::@ 'Maybe' 'Text'
--
updateDistributionResult :: UpdateDistributionResult
updateDistributionResult = UpdateDistributionResult
    { _udrDistribution = Nothing
    , _udrETag         = Nothing
    }

-- | The distribution's information.
udrDistribution :: Lens' UpdateDistributionResult (Maybe Distribution)
udrDistribution = lens _udrDistribution (\s a -> s { _udrDistribution = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrETag :: Lens' UpdateDistributionResult (Maybe Text)
udrETag = lens _udrETag (\s a -> s { _udrETag = a })

instance AWSRequest UpdateDistribution where
    type Sv UpdateDistribution = CloudFront
    type Rs UpdateDistribution = UpdateDistributionResult

    request  = put
    response = const . xmlResponse $ \h x -> UpdateDistributionResult
        <$> x %| "Distribution"
        <*> h ~:? "ETag"
