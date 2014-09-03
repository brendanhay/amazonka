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
    , deleteDistribution
    -- ** Request lenses
    , ddrId
    , ddrIfMatch

    -- * Response
    , DeleteDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDistribution' request.
deleteDistribution :: Text -- ^ 'ddrId'
                   -> DeleteDistribution
deleteDistribution p1 = DeleteDistribution
    { _ddrId = p1
    , _ddrIfMatch = Nothing
    }

data DeleteDistribution = DeleteDistribution
    { _ddrId :: Text
      -- ^ The distribution id.
    , _ddrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when you disabled the
      -- distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The distribution id.
ddrId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteDistribution
    -> f DeleteDistribution
ddrId f x =
    (\y -> x { _ddrId = y })
       <$> f (_ddrId x)
{-# INLINE ddrId #-}

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddrIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteDistribution
    -> f DeleteDistribution
ddrIfMatch f x =
    (\y -> x { _ddrIfMatch = y })
       <$> f (_ddrIfMatch x)
{-# INLINE ddrIfMatch #-}

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _ddrId
        ]

instance ToQuery DeleteDistribution

instance ToHeaders DeleteDistribution where
    toHeaders DeleteDistribution{..} = concat
        [ "If-Match" =: _ddrIfMatch
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
