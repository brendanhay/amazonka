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

-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a streaming distribution.
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Request
      DeleteStreamingDistribution
    -- ** Request constructor
    , deleteStreamingDistribution2014_05_31
    -- ** Request lenses
    , dsdId
    , dsdIfMatch

    -- * Response
    , DeleteStreamingDistribution2014_05_31Response
    -- ** Response constructor
    , deleteStreamingDistribution2014_05_31Response
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data DeleteStreamingDistribution = DeleteStreamingDistribution
    { _dsdId      :: Text
    , _dsdIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdId' @::@ 'Text'
--
-- * 'dsdIfMatch' @::@ 'Maybe' 'Text'
--
deleteStreamingDistribution2014_05_31 :: Text -- ^ 'dsdId'
                                      -> DeleteStreamingDistribution
deleteStreamingDistribution2014_05_31 p1 = DeleteStreamingDistribution
    { _dsdId      = p1
    , _dsdIfMatch = Nothing
    }

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\s a -> s { _dsdId = a })

-- | The value of the ETag header you received when you disabled the streaming
-- distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\s a -> s { _dsdIfMatch = a })

instance ToPath DeleteStreamingDistribution where
    toPath DeleteStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _dsdId
        ]

instance ToQuery DeleteStreamingDistribution where
    toQuery = const mempty

instance ToHeaders DeleteStreamingDistribution where
    toHeaders DeleteStreamingDistribution{..} = mconcat
        [ "If-Match" =: _dsdIfMatch
        ]

data DeleteStreamingDistribution2014_05_31Response = DeleteStreamingDistribution2014_05_31Response
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStreamingDistribution2014_05_31Response' constructor.
deleteStreamingDistribution2014_05_31Response :: DeleteStreamingDistribution2014_05_31Response
deleteStreamingDistribution2014_05_31Response = DeleteStreamingDistribution2014_05_31Response

instance FromXML DeleteStreamingDistribution2014_05_31Response where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteStreamingDistribution2014_05_31Response"
instance AWSRequest DeleteStreamingDistribution where
    type Sv DeleteStreamingDistribution = CloudFront
    type Rs DeleteStreamingDistribution = DeleteStreamingDistribution2014_05_31Response

    request  = delete
    response = nullaryResponse DeleteStreamingDistribution2014_05_31Response
