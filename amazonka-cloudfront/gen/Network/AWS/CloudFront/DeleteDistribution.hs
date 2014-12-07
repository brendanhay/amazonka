{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Delete a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteDistribution.html>
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
    , DeleteDistributionResponse
    -- ** Response constructor
    , deleteDistributionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data DeleteDistribution = DeleteDistribution
    { _ddId      :: Text
    , _ddIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The value of the ETag header you received when you disabled the distribution.
-- For example: E2QWRUHAPOMQZL.
ddIfMatch :: Lens' DeleteDistribution (Maybe Text)
ddIfMatch = lens _ddIfMatch (\s a -> s { _ddIfMatch = a })

data DeleteDistributionResponse = DeleteDistributionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDistributionResponse' constructor.
deleteDistributionResponse :: DeleteDistributionResponse
deleteDistributionResponse = DeleteDistributionResponse

instance ToPath DeleteDistribution where
    toPath DeleteDistribution{..} = mconcat
        [ "/2014-10-21/distribution/"
        , toText _ddId
        ]

instance ToQuery DeleteDistribution where
    toQuery = const mempty

instance ToHeaders DeleteDistribution where
    toHeaders DeleteDistribution{..} = mconcat
        [ "If-Match" =: _ddIfMatch
        ]

instance ToXMLRoot DeleteDistribution where
    toXMLRoot = const (namespaced ns "DeleteDistribution" [])

instance ToXML DeleteDistribution

instance AWSRequest DeleteDistribution where
    type Sv DeleteDistribution = CloudFront
    type Rs DeleteDistribution = DeleteDistributionResponse

    request  = delete
    response = nullResponse DeleteDistributionResponse
