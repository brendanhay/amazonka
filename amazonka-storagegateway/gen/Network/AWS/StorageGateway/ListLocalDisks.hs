{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a list of the gateway\'s local disks. To specify
-- which gateway to describe, you use the Amazon Resource Name (ARN) of the
-- gateway in the body of the request.
--
-- The request returns a list of all disks, specifying which are configured
-- as working storage, cache storage, or stored volume or not configured at
-- all. The response includes a @DiskStatus@ field. This field can have a
-- value of present (the disk is available to use), missing (the disk is no
-- longer connected to the gateway), or mismatch (the disk node is occupied
-- by a disk that has incorrect metadata or the disk content is corrupted).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListLocalDisks.html>
module Network.AWS.StorageGateway.ListLocalDisks
    (
    -- * Request
      ListLocalDisks
    -- ** Request constructor
    , listLocalDisks
    -- ** Request lenses
    , lldrqGatewayARN

    -- * Response
    , ListLocalDisksResponse
    -- ** Response constructor
    , listLocalDisksResponse
    -- ** Response lenses
    , lldrsGatewayARN
    , lldrsDisks
    , lldrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'listLocalDisks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lldrqGatewayARN'
newtype ListLocalDisks = ListLocalDisks'
    { _lldrqGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListLocalDisks' smart constructor.
listLocalDisks :: Text -> ListLocalDisks
listLocalDisks pGatewayARN =
    ListLocalDisks'
    { _lldrqGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
lldrqGatewayARN :: Lens' ListLocalDisks Text
lldrqGatewayARN = lens _lldrqGatewayARN (\ s a -> s{_lldrqGatewayARN = a});

instance AWSRequest ListLocalDisks where
        type Sv ListLocalDisks = StorageGateway
        type Rs ListLocalDisks = ListLocalDisksResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListLocalDisksResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "Disks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListLocalDisks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListLocalDisks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLocalDisks where
        toJSON ListLocalDisks'{..}
          = object ["GatewayARN" .= _lldrqGatewayARN]

instance ToPath ListLocalDisks where
        toPath = const "/"

instance ToQuery ListLocalDisks where
        toQuery = const mempty

-- | /See:/ 'listLocalDisksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lldrsGatewayARN'
--
-- * 'lldrsDisks'
--
-- * 'lldrsStatus'
data ListLocalDisksResponse = ListLocalDisksResponse'
    { _lldrsGatewayARN :: !(Maybe Text)
    , _lldrsDisks      :: !(Maybe [Disk])
    , _lldrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListLocalDisksResponse' smart constructor.
listLocalDisksResponse :: Int -> ListLocalDisksResponse
listLocalDisksResponse pStatus =
    ListLocalDisksResponse'
    { _lldrsGatewayARN = Nothing
    , _lldrsDisks = Nothing
    , _lldrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
lldrsGatewayARN :: Lens' ListLocalDisksResponse (Maybe Text)
lldrsGatewayARN = lens _lldrsGatewayARN (\ s a -> s{_lldrsGatewayARN = a});

-- | FIXME: Undocumented member.
lldrsDisks :: Lens' ListLocalDisksResponse [Disk]
lldrsDisks = lens _lldrsDisks (\ s a -> s{_lldrsDisks = a}) . _Default;

-- | FIXME: Undocumented member.
lldrsStatus :: Lens' ListLocalDisksResponse Int
lldrsStatus = lens _lldrsStatus (\ s a -> s{_lldrsStatus = a});
