{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action returns the current status of a change batch request. The
-- status is one of the following values: - PENDING indicates that the changes
-- in this request have not replicated to all Route 53 DNS servers. This is
-- the initial status of all change batch requests. - INSYNC indicates that
-- the changes have replicated to all Amazon Route 53 DNS servers.
module Network.AWS.Route53
    (
    -- * Request
      GetChange
    -- ** Request constructor
    , mkGetChange
    -- ** Request lenses
    , gcId

    -- * Response
    , GetChangeResponse
    -- ** Response constructor
    , mkGetChangeResponse
    -- ** Response lenses
    , gcrChangeInfo
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The input for a GetChange request.
newtype GetChange = GetChange
    { _gcId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetChange' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkGetChange :: Text -- ^ 'gcId'
            -> GetChange
mkGetChange p1 = GetChange
    { _gcId = p1
    }

-- | The ID of the change batch request. The value that you specify here is the
-- value that ChangeResourceRecordSets returned in the Id element when you
-- submitted the request.
gcId :: Lens' GetChange Text
gcId = lens _gcId (\s a -> s { _gcId = a })

instance ToPath GetChange

instance ToQuery GetChange

instance ToHeaders GetChange

instance ToXML GetChange where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetChangeRequest"

-- | A complex type that contains the ChangeInfo element.
newtype GetChangeResponse = GetChangeResponse
    { _gcrChangeInfo :: ChangeInfo
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetChangeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ChangeInfo ::@ @ChangeInfo@
--
mkGetChangeResponse :: ChangeInfo -- ^ 'gcrChangeInfo'
                    -> GetChangeResponse
mkGetChangeResponse p1 = GetChangeResponse
    { _gcrChangeInfo = p1
    }

-- | A complex type that contains information about the specified change batch,
-- including the change batch ID, the status of the change, and the date and
-- time of the request.
gcrChangeInfo :: Lens' GetChangeResponse ChangeInfo
gcrChangeInfo = lens _gcrChangeInfo (\s a -> s { _gcrChangeInfo = a })

instance FromXML GetChangeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetChange where
    type Sv GetChange = Route53
    type Rs GetChange = GetChangeResponse

    request = get
    response _ = xmlResponse
