{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DeleteGateway
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

-- | This operation deletes a gateway. To specify which gateway to delete,
-- use the Amazon Resource Name (ARN) of the gateway in your request. The
-- operation deletes the gateway; however, it does not delete the gateway
-- virtual machine (VM) from your host computer.
--
-- After you delete a gateway, you cannot reactivate it. Completed
-- snapshots of the gateway volumes are not deleted upon deleting the
-- gateway, however, pending snapshots will not complete. After you delete
-- a gateway, your next step is to remove it from your environment.
--
-- You no longer pay software charges after the gateway is deleted;
-- however, your existing Amazon EBS snapshots persist and you will
-- continue to be billed for these snapshots. You can choose to remove all
-- remaining Amazon EBS snapshots by canceling your Amazon EC2
-- subscription.  If you prefer not to cancel your Amazon EC2 subscription,
-- you can delete your snapshots using the Amazon EC2 console. For more
-- information, see the
-- <http://aws.amazon.com/storagegateway AWS Storage Gateway Detail Page>.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteGateway.html>
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
    , dgrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the id of the gateway to delete.
--
-- /See:/ 'deleteGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgGatewayARN'
newtype DeleteGateway = DeleteGateway'{_dgGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteGateway' smart constructor.
deleteGateway :: Text -> DeleteGateway
deleteGateway pGatewayARN = DeleteGateway'{_dgGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
dgGatewayARN :: Lens' DeleteGateway Text
dgGatewayARN = lens _dgGatewayARN (\ s a -> s{_dgGatewayARN = a});

instance AWSRequest DeleteGateway where
        type Sv DeleteGateway = StorageGateway
        type Rs DeleteGateway = DeleteGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders DeleteGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteGateway where
        toJSON DeleteGateway'{..}
          = object ["GatewayARN" .= _dgGatewayARN]

instance ToPath DeleteGateway where
        toPath = const "/"

instance ToQuery DeleteGateway where
        toQuery = const mempty

-- | A JSON object containing the id of the deleted gateway.
--
-- /See:/ 'deleteGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgrGatewayARN'
--
-- * 'dgrStatusCode'
data DeleteGatewayResponse = DeleteGatewayResponse'{_dgrGatewayARN :: Maybe Text, _dgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DeleteGatewayResponse' smart constructor.
deleteGatewayResponse :: Int -> DeleteGatewayResponse
deleteGatewayResponse pStatusCode = DeleteGatewayResponse'{_dgrGatewayARN = Nothing, _dgrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
dgrGatewayARN :: Lens' DeleteGatewayResponse (Maybe Text)
dgrGatewayARN = lens _dgrGatewayARN (\ s a -> s{_dgrGatewayARN = a});

-- | FIXME: Undocumented member.
dgrStatusCode :: Lens' DeleteGatewayResponse Int
dgrStatusCode = lens _dgrStatusCode (\ s a -> s{_dgrStatusCode = a});
