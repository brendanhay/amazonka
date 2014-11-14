{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a virtual interface.
module Network.AWS.DirectConnect.DeleteVirtualInterface
    (
    -- * Request
      DeleteVirtualInterface
    -- ** Request constructor
    , deleteVirtualInterface
    -- ** Request lenses
    , dvi1VirtualInterfaceId

    -- * Response
    , DeleteVirtualInterfaceResponse
    -- ** Response constructor
    , deleteVirtualInterfaceResponse
    -- ** Response lenses
    , dvirVirtualInterfaceState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DirectConnect.Types

newtype DeleteVirtualInterface = DeleteVirtualInterface
    { _dvi1VirtualInterfaceId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvi1VirtualInterfaceId' @::@ 'Text'
--
deleteVirtualInterface :: Text -- ^ 'dvi1VirtualInterfaceId'
                       -> DeleteVirtualInterface
deleteVirtualInterface p1 = DeleteVirtualInterface
    { _dvi1VirtualInterfaceId = p1
    }

dvi1VirtualInterfaceId :: Lens' DeleteVirtualInterface Text
dvi1VirtualInterfaceId =
    lens _dvi1VirtualInterfaceId (\s a -> s { _dvi1VirtualInterfaceId = a })

instance ToPath DeleteVirtualInterface where
    toPath = const "/"

instance ToQuery DeleteVirtualInterface where
    toQuery = const mempty

instance ToHeaders DeleteVirtualInterface

instance ToBody DeleteVirtualInterface where
    toBody = toBody . encode . _dvi1VirtualInterfaceId

newtype DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse
    { _dvirVirtualInterfaceState :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirVirtualInterfaceState' @::@ 'Maybe' 'Text'
--
deleteVirtualInterfaceResponse :: DeleteVirtualInterfaceResponse
deleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse
    { _dvirVirtualInterfaceState = Nothing
    }

dvirVirtualInterfaceState :: Lens' DeleteVirtualInterfaceResponse (Maybe Text)
dvirVirtualInterfaceState =
    lens _dvirVirtualInterfaceState
        (\s a -> s { _dvirVirtualInterfaceState = a })

instance AWSRequest DeleteVirtualInterface where
    type Sv DeleteVirtualInterface = DirectConnect
    type Rs DeleteVirtualInterface = DeleteVirtualInterfaceResponse

    request  = post
    response = jsonResponse $ \h o -> DeleteVirtualInterfaceResponse
        <$> o .: "virtualInterfaceState"
