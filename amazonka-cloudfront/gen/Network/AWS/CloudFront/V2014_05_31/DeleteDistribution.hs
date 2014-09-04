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
    , mkDeleteDistributionRequest
    -- ** Request lenses
    , ddrId
    , ddrIfMatch

    -- * Response
    , DeleteDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDistribution' request.
mkDeleteDistributionRequest :: Text -- ^ 'ddrId'
                            -> DeleteDistribution
mkDeleteDistributionRequest p1 = DeleteDistribution
    { _ddrId = p1
    , _ddrIfMatch = Nothing
    }
{-# INLINE mkDeleteDistributionRequest #-}

data DeleteDistribution = DeleteDistribution
    { _ddrId :: Text
      -- ^ The distribution id.
    , _ddrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when you disabled the
      -- distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The distribution id.
ddrId :: Lens' DeleteDistribution (Text)
ddrId = lens _ddrId (\s a -> s { _ddrId = a })
{-# INLINE ddrId #-}

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddrIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddrIfMatch = lens _ddrIfMatch (\s a -> s { _ddrIfMatch = a })
{-# INLINE ddrIfMatch #-}

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _ddrId
        ]

instance ToQuery DeleteDistribution

instance ToHeaders DeleteDistribution

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
