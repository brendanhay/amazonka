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
      DeleteDistribution2014_05_31
    -- ** Request constructor
    , deleteDistribution2014_05_31
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
import qualified GHC.Exts

data DeleteDistribution2014_05_31 = DeleteDistribution2014_05_31
    { _ddId      :: Text
    , _ddIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddId' @::@ 'Text'
--
-- * 'ddIfMatch' @::@ 'Maybe' 'Text'
--
deleteDistribution2014_05_31 :: Text -- ^ 'ddId'
                             -> DeleteDistribution2014_05_31
deleteDistribution2014_05_31 p1 = DeleteDistribution2014_05_31
    { _ddId      = p1
    , _ddIfMatch = Nothing
    }

-- | The distribution id.
ddId :: Lens' DeleteDistribution2014_05_31 Text
ddId = lens _ddId (\s a -> s { _ddId = a })

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution2014_05_31 (Maybe Text)
ddIfMatch = lens _ddIfMatch (\s a -> s { _ddIfMatch = a })

instance ToPath DeleteDistribution2014_05_31 where
    toPath DeleteDistribution2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _ddId
        ]

instance ToQuery DeleteDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders DeleteDistribution2014_05_31 where
    toHeaders DeleteDistribution2014_05_31{..} = mconcat
        [ "If-Match" =: _ddIfMatch
        ]

data DeleteDistribution2014_05_31Response = DeleteDistribution2014_05_31Response
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDistribution2014_05_31Response' constructor.
deleteDistribution2014_05_31Response :: DeleteDistribution2014_05_31Response
deleteDistribution2014_05_31Response = DeleteDistribution2014_05_31Response

instance AWSRequest DeleteDistribution2014_05_31 where
    type Sv DeleteDistribution2014_05_31 = CloudFront
    type Rs DeleteDistribution2014_05_31 = DeleteDistribution2014_05_31Response

    request  = delete
    response = nullaryResponse DeleteDistribution2014_05_31Response
