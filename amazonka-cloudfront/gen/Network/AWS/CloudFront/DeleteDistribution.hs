{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudFront
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
    -- ** Response constructor
    , mkDeleteDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to delete a distribution.
data DeleteDistribution = DeleteDistribution
    { _ddId :: !Text
    , _ddIfMatch :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @IfMatch ::@ @Maybe Text@
--
mkDeleteDistribution :: Text -- ^ 'ddId'
                     -> DeleteDistribution
mkDeleteDistribution p1 = DeleteDistribution
    { _ddId = p1
    , _ddIfMatch = Nothing
    }

-- | The distribution id.
ddId :: Lens' DeleteDistribution Text
ddId = lens _ddId (\s a -> s { _ddId = a })

-- | The value of the ETag header you received when you disabled the
-- distribution. For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\s a -> s { _ddIfMatch = a })

instance ToPath DeleteDistribution

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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteDistributionResponse :: DeleteDistributionResponse
mkDeleteDistributionResponse = DeleteDistributionResponse

instance AWSRequest DeleteDistribution where
    type Sv DeleteDistribution = CloudFront
    type Rs DeleteDistribution = DeleteDistributionResponse

    request = get
    response _ = nullaryResponse DeleteDistributionResponse
