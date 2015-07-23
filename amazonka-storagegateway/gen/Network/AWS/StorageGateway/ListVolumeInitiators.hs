{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists iSCSI initiators that are connected to a volume.
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
    , lvirqVolumeARN

    -- * Response
    , ListVolumeInitiatorsResponse
    -- ** Response constructor
    , listVolumeInitiatorsResponse
    -- ** Response lenses
    , lvirsInitiators
    , lvirsStatus
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
-- * 'lvirqVolumeARN'
newtype ListVolumeInitiators = ListVolumeInitiators'
    { _lvirqVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumeInitiators' smart constructor.
listVolumeInitiators :: Text -> ListVolumeInitiators
listVolumeInitiators pVolumeARN_ =
    ListVolumeInitiators'
    { _lvirqVolumeARN = pVolumeARN_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes for the gateway.
lvirqVolumeARN :: Lens' ListVolumeInitiators Text
lvirqVolumeARN = lens _lvirqVolumeARN (\ s a -> s{_lvirqVolumeARN = a});

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
          = object ["VolumeARN" .= _lvirqVolumeARN]

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
-- * 'lvirsInitiators'
--
-- * 'lvirsStatus'
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
    { _lvirsInitiators :: !(Maybe [Text])
    , _lvirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumeInitiatorsResponse' smart constructor.
listVolumeInitiatorsResponse :: Int -> ListVolumeInitiatorsResponse
listVolumeInitiatorsResponse pStatus_ =
    ListVolumeInitiatorsResponse'
    { _lvirsInitiators = Nothing
    , _lvirsStatus = pStatus_
    }

-- | The host names and port numbers of all iSCSI initiators that are
-- connected to the gateway.
lvirsInitiators :: Lens' ListVolumeInitiatorsResponse [Text]
lvirsInitiators = lens _lvirsInitiators (\ s a -> s{_lvirsInitiators = a}) . _Default;

-- | FIXME: Undocumented member.
lvirsStatus :: Lens' ListVolumeInitiatorsResponse Int
lvirsStatus = lens _lvirsStatus (\ s a -> s{_lvirsStatus = a});
