{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteStreamingDistribution' request.
deleteStreamingDistribution :: Text -- ^ '_dsdrId'
                            -> DeleteStreamingDistribution
deleteStreamingDistribution p1 = DeleteStreamingDistribution
    { _dsdrId = p1
    , _dsdrIfMatch = Nothing
    }

data DeleteStreamingDistribution = DeleteStreamingDistribution
    { _dsdrId :: Text
      -- ^ The distribution id.
    , _dsdrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when you disabled the
      -- streaming distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

makeLenses ''DeleteStreamingDistribution

instance ToPath DeleteStreamingDistribution where
    toPath DeleteStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _dsdrId
        ]

instance ToQuery DeleteStreamingDistribution

instance ToHeaders DeleteStreamingDistribution where
    toHeaders DeleteStreamingDistribution{..} = concat
        [ "If-Match" =: _dsdrIfMatch
        ]

instance ToXML DeleteStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteStreamingDistributionRequest"

data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteStreamingDistributionResponse

instance AWSRequest DeleteStreamingDistribution where
    type Sv DeleteStreamingDistribution = CloudFront
    type Rs DeleteStreamingDistribution = DeleteStreamingDistributionResponse

    request = delete
    response _ = nullaryResponse DeleteStreamingDistributionResponse
