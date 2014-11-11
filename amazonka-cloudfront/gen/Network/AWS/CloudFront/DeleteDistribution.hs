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

-- Module      : Network.AWS.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a distribution.
module Network.AWS.CloudFront.DeleteDistribution
    (
    -- * Request
      DeleteDistribution
    -- ** Request constructor
    , deleteDistribution
    -- ** Request lenses
    , ddId
    , ddIfMatch

    -- * Response
    , DeleteDistribution2014_05_31Response
    -- ** Response constructor
    , deleteDistribution2014_05_31Response
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data DeleteDistribution = DeleteDistribution
    { _ddId      :: Text
    , _ddIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddId' @::@ 'Text'
--
-- * 'ddIfMatch' @::@ 'Maybe' 'Text'
--
deleteDistribution :: Text -- ^ 'ddId'
                   -> DeleteDistribution
deleteDistribution p1 = DeleteDistribution
    { _ddId      = p1
    , _ddIfMatch = Nothing
    }

-- | The distribution id.
ddId :: Lens' DeleteDistribution Text
ddId = lens _ddId (\s a -> s { _ddId = a })

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\s a -> s { _ddIfMatch = a })

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _ddId
        ]

instance ToQuery DeleteDistribution where
    toQuery = const mempty

instance ToHeaders DeleteDistribution where
    toHeaders DeleteDistribution{..} = mconcat
        [ "If-Match" =: _ddIfMatch
        ]

data DeleteDistribution2014_05_31Response = DeleteDistribution2014_05_31Response
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDistribution2014_05_31Response' constructor.
deleteDistribution2014_05_31Response :: DeleteDistribution2014_05_31Response
deleteDistribution2014_05_31Response = DeleteDistribution2014_05_31Response

instance FromXML DeleteDistribution2014_05_31Response where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDistribution2014_05_31Response"
instance AWSRequest DeleteDistribution where
    type Sv DeleteDistribution = CloudFront
    type Rs DeleteDistribution = DeleteDistribution2014_05_31Response

    request  = delete
    response = nullaryResponse DeleteDistribution2014_05_31Response
