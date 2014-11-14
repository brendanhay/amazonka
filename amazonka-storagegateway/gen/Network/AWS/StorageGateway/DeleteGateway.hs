{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.StorageGateway.DeleteGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes a gateway. To specify which gateway to delete, use
-- the Amazon Resource Name (ARN) of the gateway in your request. The
-- operation deletes the gateway; however, it does not delete the gateway
-- virtual machine (VM) from your host computer. After you delete a gateway,
-- you cannot reactivate it. Completed snapshots of the gateway volumes are
-- not deleted upon deleting the gateway, however, pending snapshots will not
-- complete. After you delete a gateway, your next step is to remove it from
-- your environment. You no longer pay software charges after the gateway is
-- deleted; however, your existing Amazon EBS snapshots persist and you will
-- continue to be billed for these snapshots. You can choose to remove all
-- remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.
-- If you prefer not to cancel your Amazon EC2 subscription, you can delete
-- your snapshots using the Amazon EC2 console. For more information, see the
-- AWS Storage Gateway Detail Page.
module Network.AWS.StorageGateway.DeleteGateway
    (
    -- * Request
      DeleteGateway
    -- ** Request constructor
    , deleteGateway
    -- ** Request lenses
    , dgGatewayARN

    -- * Response
    , DeleteGatewayResponse
    -- ** Response constructor
    , deleteGatewayResponse
    -- ** Response lenses
    , dgrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

newtype DeleteGateway = DeleteGateway
    { _dgGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgGatewayARN' @::@ 'Text'
--
deleteGateway :: Text -- ^ 'dgGatewayARN'
              -> DeleteGateway
deleteGateway p1 = DeleteGateway
    { _dgGatewayARN = p1
    }

dgGatewayARN :: Lens' DeleteGateway Text
dgGatewayARN = lens _dgGatewayARN (\s a -> s { _dgGatewayARN = a })

instance ToPath DeleteGateway where
    toPath = const "/"

instance ToQuery DeleteGateway where
    toQuery = const mempty

instance ToHeaders DeleteGateway

instance ToBody DeleteGateway where
    toBody = toBody . encode . _dgGatewayARN

newtype DeleteGatewayResponse = DeleteGatewayResponse
    { _dgrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgrGatewayARN' @::@ 'Maybe' 'Text'
--
deleteGatewayResponse :: DeleteGatewayResponse
deleteGatewayResponse = DeleteGatewayResponse
    { _dgrGatewayARN = Nothing
    }

dgrGatewayARN :: Lens' DeleteGatewayResponse (Maybe Text)
dgrGatewayARN = lens _dgrGatewayARN (\s a -> s { _dgrGatewayARN = a })

instance AWSRequest DeleteGateway where
    type Sv DeleteGateway = StorageGateway
    type Rs DeleteGateway = DeleteGatewayResponse

    request  = post
    response = jsonResponse $ \h o -> DeleteGatewayResponse
        <$> o .: "GatewayARN"
