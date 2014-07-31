{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
module Network.AWS.CloudFront.V2014_05_31.DeleteDistribution where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDistribution' request.
deleteDistribution :: Text -- ^ '_ddrId'
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
    } deriving (Generic)

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

instance AWSRequest DeleteDistribution where
    type Sv DeleteDistribution = CloudFront
    type Rs DeleteDistribution = DeleteDistributionResponse

    request = delete
    response _ _ = return (Right DeleteDistributionResponse)

data DeleteDistributionResponse = DeleteDistributionResponse
    deriving (Eq, Show, Generic)
