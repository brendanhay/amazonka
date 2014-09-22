{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , deleteStreamingDistribution
    -- ** Request lenses
    , dsdId
    , dsdIfMatch

    -- * Response
    , DeleteStreamingDistributionResponse
    -- ** Response constructor
    , deleteStreamingDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to delete a streaming distribution.
data DeleteStreamingDistribution = DeleteStreamingDistribution
    { _dsdId :: Text
    , _dsdIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStreamingDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @IfMatch ::@ @Maybe Text@
--
deleteStreamingDistribution :: Text -- ^ 'dsdId'
                            -> DeleteStreamingDistribution
deleteStreamingDistribution p1 = DeleteStreamingDistribution
    { _dsdId = p1
    , _dsdIfMatch = Nothing
    }

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\s a -> s { _dsdId = a })

-- | The value of the ETag header you received when you disabled the streaming
-- distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\s a -> s { _dsdIfMatch = a })

instance ToPath DeleteStreamingDistribution

instance ToQuery DeleteStreamingDistribution

instance ToHeaders DeleteStreamingDistribution where
    toHeaders DeleteStreamingDistribution{..} = concat
        [ "If-Match" =: _dsdIfMatch
        ]

instance ToXML DeleteStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteStreamingDistribution"

data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStreamingDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse

instance AWSRequest DeleteStreamingDistribution where
    type Sv DeleteStreamingDistribution = CloudFront
    type Rs DeleteStreamingDistribution = DeleteStreamingDistributionResponse

    request = get
    response _ = nullaryResponse DeleteStreamingDistributionResponse
