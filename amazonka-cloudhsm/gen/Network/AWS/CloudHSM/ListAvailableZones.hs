{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the Availability Zones that have available AWS CloudHSM capacity.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListAvailableZones.html>
module Network.AWS.CloudHSM.ListAvailableZones
    (
    -- * Request
      ListAvailableZones
    -- ** Request constructor
    , listAvailableZones

    -- * Response
    , ListAvailableZonesResponse
    -- ** Response constructor
    , listAvailableZonesResponse
    -- ** Response lenses
    , lazrAZList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'listAvailableZones' smart constructor.
data ListAvailableZones = ListAvailableZones' deriving (Eq, Read, Show)

-- | 'ListAvailableZones' smart constructor.
listAvailableZones :: ListAvailableZones
listAvailableZones = ListAvailableZones';

instance AWSRequest ListAvailableZones where
        type Sv ListAvailableZones = CloudHSM
        type Rs ListAvailableZones =
             ListAvailableZonesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListAvailableZonesResponse' <$>
                   x .?> "AZList" .!@ mempty)

instance ToHeaders ListAvailableZones where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListAvailableZones" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAvailableZones where
        toJSON = const (Object mempty)

instance ToPath ListAvailableZones where
        toPath = const "/"

instance ToQuery ListAvailableZones where
        toQuery = const mempty

-- | /See:/ 'listAvailableZonesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lazrAZList'
newtype ListAvailableZonesResponse = ListAvailableZonesResponse'{_lazrAZList :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'ListAvailableZonesResponse' smart constructor.
listAvailableZonesResponse :: ListAvailableZonesResponse
listAvailableZonesResponse = ListAvailableZonesResponse'{_lazrAZList = Nothing};

-- | The list of Availability Zones that have available AWS CloudHSM
-- capacity.
lazrAZList :: Lens' ListAvailableZonesResponse (Maybe [Text])
lazrAZList = lens _lazrAZList (\ s a -> s{_lazrAZList = a});
