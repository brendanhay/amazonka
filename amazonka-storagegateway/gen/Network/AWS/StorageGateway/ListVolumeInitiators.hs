{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , lvirStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'listVolumeInitiators' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lviVolumeARN'
newtype ListVolumeInitiators = ListVolumeInitiators'
    { _lviVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumeInitiators' smart constructor.
listVolumeInitiators :: Text -> ListVolumeInitiators
listVolumeInitiators pVolumeARN =
    ListVolumeInitiators'
    { _lviVolumeARN = pVolumeARN
    }

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
                   (x .?> "Initiators" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'listVolumeInitiatorsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvirInitiators'
--
-- * 'lvirStatus'
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
    { _lvirInitiators :: !(Maybe [Text])
    , _lvirStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumeInitiatorsResponse' smart constructor.
listVolumeInitiatorsResponse :: Int -> ListVolumeInitiatorsResponse
listVolumeInitiatorsResponse pStatus =
    ListVolumeInitiatorsResponse'
    { _lvirInitiators = Nothing
    , _lvirStatus = pStatus
    }

-- | The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
lvirInitiators :: Lens' ListVolumeInitiatorsResponse [Text]
lvirInitiators = lens _lvirInitiators (\ s a -> s{_lvirInitiators = a}) . _Default;

-- | FIXME: Undocumented member.
lvirStatus :: Lens' ListVolumeInitiatorsResponse Int
lvirStatus = lens _lvirStatus (\ s a -> s{_lvirStatus = a});
