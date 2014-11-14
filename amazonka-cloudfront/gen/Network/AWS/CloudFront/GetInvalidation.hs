{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
      GetInvalidation2014_05_31
    -- ** Request constructor
    , getInvalidation2014_05_31
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidation2014_05_31Response
    -- ** Response constructor
    , getInvalidation2014_05_31Response
    -- ** Response lenses
    , girInvalidation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data GetInvalidation2014_05_31 = GetInvalidation2014_05_31
    { _giDistributionId :: Text
    , _giId             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetInvalidation2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giDistributionId' @::@ 'Text'
--
-- * 'giId' @::@ 'Text'
--
getInvalidation2014_05_31 :: Text -- ^ 'giDistributionId'
                          -> Text -- ^ 'giId'
                          -> GetInvalidation2014_05_31
getInvalidation2014_05_31 p1 p2 = GetInvalidation2014_05_31
    { _giDistributionId = p1
    , _giId             = p2
    }

-- | The distribution's id.
giDistributionId :: Lens' GetInvalidation2014_05_31 Text
giDistributionId = lens _giDistributionId (\s a -> s { _giDistributionId = a })

-- | The invalidation's id.
giId :: Lens' GetInvalidation2014_05_31 Text
giId = lens _giId (\s a -> s { _giId = a })

instance ToPath GetInvalidation2014_05_31 where
    toPath GetInvalidation2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _giDistributionId
        , "/invalidation/"
        , toText _giId
        ]

instance ToQuery GetInvalidation2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetInvalidation2014_05_31

newtype GetInvalidation2014_05_31Response = GetInvalidation2014_05_31Response
    { _girInvalidation :: Maybe Invalidation
    } deriving (Eq, Show, Generic)

-- | 'GetInvalidation2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girInvalidation' @::@ 'Maybe' 'Invalidation'
--
getInvalidation2014_05_31Response :: GetInvalidation2014_05_31Response
getInvalidation2014_05_31Response = GetInvalidation2014_05_31Response
    { _girInvalidation = Nothing
    }

-- | The invalidation's information.
girInvalidation :: Lens' GetInvalidation2014_05_31Response (Maybe Invalidation)
girInvalidation = lens _girInvalidation (\s a -> s { _girInvalidation = a })

instance AWSRequest GetInvalidation2014_05_31 where
    type Sv GetInvalidation2014_05_31 = CloudFront
    type Rs GetInvalidation2014_05_31 = GetInvalidation2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetInvalidation2014_05_31Response
        <$> x %| "Invalidation"
