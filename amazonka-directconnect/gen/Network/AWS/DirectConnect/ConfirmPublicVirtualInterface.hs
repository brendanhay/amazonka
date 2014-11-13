{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept ownership of a public virtual interface created by another customer.
-- After the virtual interface owner calls this function, the specified
-- virtual interface will be created and made available for handling traffic.
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
    (
    -- * Request
      ConfirmPublicVirtualInterface
    -- ** Request constructor
    , confirmPublicVirtualInterface
    -- ** Request lenses
    , cpvi1VirtualInterfaceId

    -- * Response
    , ConfirmPublicVirtualInterfaceResponse
    -- ** Response constructor
    , confirmPublicVirtualInterfaceResponse
    -- ** Response lenses
    , cpvir3VirtualInterfaceState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DirectConnect.Types

newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface
    { _cpvi1VirtualInterfaceId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ConfirmPublicVirtualInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvi1VirtualInterfaceId' @::@ 'Text'
--
confirmPublicVirtualInterface :: Text -- ^ 'cpvi1VirtualInterfaceId'
                              -> ConfirmPublicVirtualInterface
confirmPublicVirtualInterface p1 = ConfirmPublicVirtualInterface
    { _cpvi1VirtualInterfaceId = p1
    }

cpvi1VirtualInterfaceId :: Lens' ConfirmPublicVirtualInterface Text
cpvi1VirtualInterfaceId =
    lens _cpvi1VirtualInterfaceId (\s a -> s { _cpvi1VirtualInterfaceId = a })

instance ToPath ConfirmPublicVirtualInterface where
    toPath = const "/"

instance ToQuery ConfirmPublicVirtualInterface where
    toQuery = const mempty

instance ToHeaders ConfirmPublicVirtualInterface

instance ToBody ConfirmPublicVirtualInterface where
    toBody = toBody . encode . _cpvi1VirtualInterfaceId

newtype ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse
    { _cpvir3VirtualInterfaceState :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ConfirmPublicVirtualInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvir3VirtualInterfaceState' @::@ 'Maybe' 'Text'
--
confirmPublicVirtualInterfaceResponse :: ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse
    { _cpvir3VirtualInterfaceState = Nothing
    }

cpvir3VirtualInterfaceState :: Lens' ConfirmPublicVirtualInterfaceResponse (Maybe Text)
cpvir3VirtualInterfaceState =
    lens _cpvir3VirtualInterfaceState
        (\s a -> s { _cpvir3VirtualInterfaceState = a })

-- FromJSON

instance AWSRequest ConfirmPublicVirtualInterface where
    type Sv ConfirmPublicVirtualInterface = DirectConnect
    type Rs ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterfaceResponse

    request  = post'
    response = jsonResponse $ \h o -> ConfirmPublicVirtualInterfaceResponse
        <$> o .: "virtualInterfaceState"
