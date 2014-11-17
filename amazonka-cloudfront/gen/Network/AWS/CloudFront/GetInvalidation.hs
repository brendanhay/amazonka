{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetInvalidation.html>
module Network.AWS.CloudFront.GetInvalidation
    (
    -- * Request
      GetInvalidation
    -- ** Request constructor
    , getInvalidation
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidationResponse
    -- ** Response constructor
    , getInvalidationResponse
    -- ** Response lenses
    , girInvalidation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

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
getInvalidation :: Text -- ^ 'giDistributionId'
                -> Text -- ^ 'giId'
                -> GetInvalidation
getInvalidation p1 p2 = GetInvalidation
    { _giDistributionId = p1
    , _giId             = p2
    }

-- | The distribution's id.
giDistributionId :: Lens' GetInvalidation Text
giDistributionId = lens _giDistributionId (\s a -> s { _giDistributionId = a })

-- | The invalidation's id.
giId :: Lens' GetInvalidation Text
giId = lens _giId (\s a -> s { _giId = a })

newtype GetInvalidationResponse = GetInvalidationResponse
    { _girInvalidation :: Maybe Invalidation
    } deriving (Eq, Show, Generic)

-- | 'GetInvalidationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girInvalidation' @::@ 'Maybe' 'Invalidation'
--
getInvalidationResponse :: GetInvalidationResponse
getInvalidationResponse = GetInvalidationResponse
    { _girInvalidation = Nothing
    }

-- | The invalidation's information.
girInvalidation :: Lens' GetInvalidationResponse (Maybe Invalidation)
girInvalidation = lens _girInvalidation (\s a -> s { _girInvalidation = a })

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

instance ToXML GetInvalidation where
    toXML = const (node "GetInvalidation" [])

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResponse

    request  = get
    response = xmlResponse

instance FromXML GetInvalidationResponse where
    parseXML c = GetInvalidationResponse
        <$> c .: "Invalidation"
