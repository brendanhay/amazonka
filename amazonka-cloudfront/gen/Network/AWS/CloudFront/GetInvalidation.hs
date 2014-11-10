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

-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an invalidation.
module Network.AWS.CloudFront.GetInvalidation
    (
    -- * Request
      GetInvalidation
    -- ** Request constructor
    , getInvalidation2014_05_31
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidationResult
    -- ** Response constructor
    , getInvalidation2014_05_31Response
    -- ** Response lenses
    , girInvalidation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data GetInvalidation = GetInvalidation
    { _giDistributionId :: Text
    , _giId             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetInvalidation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giDistributionId' @::@ 'Text'
--
-- * 'giId' @::@ 'Text'
--
getInvalidation2014_05_31 :: Text -- ^ 'giDistributionId'
                          -> Text -- ^ 'giId'
                          -> GetInvalidation
getInvalidation2014_05_31 p1 p2 = GetInvalidation
    { _giDistributionId = p1
    , _giId             = p2
    }

-- | The distribution's id.
giDistributionId :: Lens' GetInvalidation Text
giDistributionId = lens _giDistributionId (\s a -> s { _giDistributionId = a })

-- | The invalidation's id.
giId :: Lens' GetInvalidation Text
giId = lens _giId (\s a -> s { _giId = a })

instance ToPath GetInvalidation where
    toPath GetInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _giDistributionId
        , "/invalidation/"
        , toText _giId
        ]

instance ToQuery GetInvalidation where
    toQuery = const mempty

instance ToHeaders GetInvalidation

newtype GetInvalidationResult = GetInvalidationResult
    { _girInvalidation :: Maybe Invalidation
    } deriving (Eq, Show, Generic)

-- | 'GetInvalidationResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girInvalidation' @::@ 'Maybe' 'Invalidation'
--
getInvalidation2014_05_31Response :: GetInvalidationResult
getInvalidation2014_05_31Response = GetInvalidationResult
    { _girInvalidation = Nothing
    }

-- | The invalidation's information.
girInvalidation :: Lens' GetInvalidationResult (Maybe Invalidation)
girInvalidation = lens _girInvalidation (\s a -> s { _girInvalidation = a })

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResult

    request  = get
    response = xmlResponse $ \h x -> GetInvalidationResult
        <$> x %| "Invalidation"
