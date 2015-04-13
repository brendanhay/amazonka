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

-- Module      : Network.AWS.CloudHSM.ListAvailableZones
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data ListAvailableZones = ListAvailableZones
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ListAvailableZones' constructor.
listAvailableZones :: ListAvailableZones
listAvailableZones = ListAvailableZones

newtype ListAvailableZonesResponse = ListAvailableZonesResponse
    { _lazrAZList :: List "AZList" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListAvailableZonesResponse where
    type Item ListAvailableZonesResponse = Text

    fromList = ListAvailableZonesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _lazrAZList

-- | 'ListAvailableZonesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lazrAZList' @::@ ['Text']
--
listAvailableZonesResponse :: ListAvailableZonesResponse
listAvailableZonesResponse = ListAvailableZonesResponse
    { _lazrAZList = mempty
    }

-- | The list of Availability Zones that have available AWS CloudHSM capacity.
lazrAZList :: Lens' ListAvailableZonesResponse [Text]
lazrAZList = lens _lazrAZList (\s a -> s { _lazrAZList = a }) . _List

instance ToPath ListAvailableZones where
    toPath = const "/"

instance ToQuery ListAvailableZones where
    toQuery = const mempty

instance ToHeaders ListAvailableZones

instance ToJSON ListAvailableZones where
    toJSON = const (toJSON Empty)

instance AWSRequest ListAvailableZones where
    type Sv ListAvailableZones = CloudHSM
    type Rs ListAvailableZones = ListAvailableZonesResponse

    request  = post "ListAvailableZones"
    response = jsonResponse

instance FromJSON ListAvailableZonesResponse where
    parseJSON = withObject "ListAvailableZonesResponse" $ \o -> ListAvailableZonesResponse
        <$> o .:? "AZList" .!= mempty
