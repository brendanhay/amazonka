{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
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

-- | This operation lists iSCSI initiators that are connected to a volume.
-- You can use this operation to determine whether a volume is being used
-- or not.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListVolumeInitiators.html>
module Network.AWS.StorageGateway.ListVolumeInitiators
    (
    -- * Request
      ListVolumeInitiators
    -- ** Request constructor
    , listVolumeInitiators
    -- ** Request lenses
    , lviVolumeARN

    -- * Response
    , ListVolumeInitiatorsResponse
    -- ** Response constructor
    , listVolumeInitiatorsResponse
    -- ** Response lenses
    , lvirInitiators
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listVolumeInitiators' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lviVolumeARN'
newtype ListVolumeInitiators = ListVolumeInitiators'{_lviVolumeARN :: Text} deriving (Eq, Read, Show)

-- | 'ListVolumeInitiators' smart constructor.
listVolumeInitiators :: Text -> ListVolumeInitiators
listVolumeInitiators pVolumeARN = ListVolumeInitiators'{_lviVolumeARN = pVolumeARN};

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes for the gateway.
lviVolumeARN :: Lens' ListVolumeInitiators Text
lviVolumeARN = lens _lviVolumeARN (\ s a -> s{_lviVolumeARN = a});

instance AWSRequest ListVolumeInitiators where
        type Sv ListVolumeInitiators = StorageGateway
        type Rs ListVolumeInitiators =
             ListVolumeInitiatorsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumeInitiatorsResponse' <$>
                   x .?> "Initiators" .!@ mempty)

instance ToHeaders ListVolumeInitiators where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListVolumeInitiators" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListVolumeInitiators where
        toJSON ListVolumeInitiators'{..}
          = object ["VolumeARN" .= _lviVolumeARN]

instance ToPath ListVolumeInitiators where
        toPath = const "/"

instance ToQuery ListVolumeInitiators where
        toQuery = const mempty

-- | /See:/ 'listVolumeInitiatorsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvirInitiators'
newtype ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'{_lvirInitiators :: [Text]} deriving (Eq, Read, Show)

-- | 'ListVolumeInitiatorsResponse' smart constructor.
listVolumeInitiatorsResponse :: ListVolumeInitiatorsResponse
listVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'{_lvirInitiators = mempty};

-- | The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
lvirInitiators :: Lens' ListVolumeInitiatorsResponse [Text]
lvirInitiators = lens _lvirInitiators (\ s a -> s{_lvirInitiators = a});
