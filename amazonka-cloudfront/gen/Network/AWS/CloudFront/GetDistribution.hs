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

-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a distribution.
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Request
      GetDistribution2014_05_31
    -- ** Request constructor
    , getDistribution2014_05_31
    -- ** Request lenses
    , gdId

    -- * Response
    , GetDistribution2014_05_31Response
    -- ** Response constructor
    , getDistribution2014_05_31Response
    -- ** Response lenses
    , gdrDistribution
    , gdrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetDistribution2014_05_31 = GetDistribution2014_05_31
    { _gdId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdId' @::@ 'Text'
--
getDistribution2014_05_31 :: Text -- ^ 'gdId'
                          -> GetDistribution2014_05_31
getDistribution2014_05_31 p1 = GetDistribution2014_05_31
    { _gdId = p1
    }

-- | The distribution's id.
gdId :: Lens' GetDistribution2014_05_31 Text
gdId = lens _gdId (\s a -> s { _gdId = a })

instance ToPath GetDistribution2014_05_31 where
    toPath GetDistribution2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _gdId
        ]

instance ToQuery GetDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetDistribution2014_05_31

data GetDistribution2014_05_31Response = GetDistribution2014_05_31Response
    { _gdrDistribution :: Maybe Distribution
    , _gdrETag         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'gdrETag' @::@ 'Maybe' 'Text'
--
getDistribution2014_05_31Response :: GetDistribution2014_05_31Response
getDistribution2014_05_31Response = GetDistribution2014_05_31Response
    { _gdrDistribution = Nothing
    , _gdrETag         = Nothing
    }

-- | The distribution's information.
gdrDistribution :: Lens' GetDistribution2014_05_31Response (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\s a -> s { _gdrDistribution = a })

-- | The current version of the distribution's information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistribution2014_05_31Response (Maybe Text)
gdrETag = lens _gdrETag (\s a -> s { _gdrETag = a })

instance AWSRequest GetDistribution2014_05_31 where
    type Sv GetDistribution2014_05_31 = CloudFront
    type Rs GetDistribution2014_05_31 = GetDistribution2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetDistribution2014_05_31Response
        <$> x %| "Distribution"
        <*> h ~:? "ETag"
