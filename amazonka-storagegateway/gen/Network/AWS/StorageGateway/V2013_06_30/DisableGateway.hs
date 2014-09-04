{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DisableGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DisableGateway
    (
    -- * Request
      DisableGateway
    -- ** Request constructor
    , mkDisableGatewayInput
    -- ** Request lenses
    , dgjGatewayARN

    -- * Response
    , DisableGatewayResponse
    -- ** Response lenses
    , dgpGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableGateway' request.
mkDisableGatewayInput :: Text -- ^ 'dgjGatewayARN'
                      -> DisableGateway
mkDisableGatewayInput p1 = DisableGateway
    { _dgjGatewayARN = p1
    }
{-# INLINE mkDisableGatewayInput #-}

newtype DisableGateway = DisableGateway
    { _dgjGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgjGatewayARN :: Lens' DisableGateway (Text)
dgjGatewayARN = lens _dgjGatewayARN (\s a -> s { _dgjGatewayARN = a })
{-# INLINE dgjGatewayARN #-}

instance ToPath DisableGateway

instance ToQuery DisableGateway

instance ToHeaders DisableGateway

instance ToJSON DisableGateway

newtype DisableGatewayResponse = DisableGatewayResponse
    { _dgpGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgpGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
dgpGatewayARN = lens _dgpGatewayARN (\s a -> s { _dgpGatewayARN = a })
{-# INLINE dgpGatewayARN #-}

instance FromJSON DisableGatewayResponse

instance AWSRequest DisableGateway where
    type Sv DisableGateway = StorageGateway
    type Rs DisableGateway = DisableGatewayResponse

    request = get
    response _ = jsonResponse
