{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.DeleteDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a distribution.
module Network.AWS.CloudFront.V2014_05_31.DeleteDistribution
    (
    -- * Request
      DeleteDistribution
    -- ** Request constructor
    , mkDeleteDistribution
    -- ** Request lenses
    , ddId
    , ddIfMatch

    -- * Response
    , DeleteDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to delete a distribution.
data DeleteDistribution = DeleteDistribution
    { _ddId :: Text
    , _ddIfMatch :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDistribution' request.
mkDeleteDistribution :: Text -- ^ 'ddId'
                     -> DeleteDistribution
mkDeleteDistribution p1 = DeleteDistribution
    { _ddId = p1
    , _ddIfMatch = Nothing
    }
{-# INLINE mkDeleteDistribution #-}

-- | The distribution id.
ddId :: Lens' DeleteDistribution Text
ddId = lens _ddId (\s a -> s { _ddId = a })
{-# INLINE ddId #-}

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\s a -> s { _ddIfMatch = a })
{-# INLINE ddIfMatch #-}

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _ddId
        ]

instance ToQuery DeleteDistribution

instance ToHeaders DeleteDistribution where
    toHeaders DeleteDistribution{..} = concat
        [ "If-Match" =: _ddIfMatch
        ]

instance ToXML DeleteDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteDistributionRequest"

data DeleteDistributionResponse = DeleteDistributionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteDistribution where
    type Sv DeleteDistribution = CloudFront
    type Rs DeleteDistribution = DeleteDistributionResponse

    request = delete
    response _ = nullaryResponse DeleteDistributionResponse
