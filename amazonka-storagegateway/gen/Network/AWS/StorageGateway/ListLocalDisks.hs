{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a list of the gateway\'s local disks. To specify
-- which gateway to describe, you use the Amazon Resource Name (ARN) of the
-- gateway in the body of the request.
--
-- The request returns a list of all disks, specifying which are configured
-- as working storage, cache storage, or stored volume or not configured at
-- all. The response includes a 'DiskStatus' field. This field can have a
-- value of present (the disk is available to use), missing (the disk is no
-- longer connected to the gateway), or mismatch (the disk node is occupied
-- by a disk that has incorrect metadata or the disk content is corrupted).
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListLocalDisks.html AWS API Reference> for ListLocalDisks.
module Network.AWS.StorageGateway.ListLocalDisks
    (
    -- * Creating a Request
      listLocalDisks
    , ListLocalDisks
    -- * Request Lenses
    , lldGatewayARN

    -- * Destructuring the Response
    , listLocalDisksResponse
    , ListLocalDisksResponse
    -- * Response Lenses
    , lldrsGatewayARN
    , lldrsDisks
    , lldrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'listLocalDisks' smart constructor.
newtype ListLocalDisks = ListLocalDisks'
    { _lldGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListLocalDisks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lldGatewayARN'
listLocalDisks
    :: Text -- ^ 'lldGatewayARN'
    -> ListLocalDisks
listLocalDisks pGatewayARN_ =
    ListLocalDisks'
    { _lldGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
lldGatewayARN :: Lens' ListLocalDisks Text
lldGatewayARN = lens _lldGatewayARN (\ s a -> s{_lldGatewayARN = a});

instance AWSRequest ListLocalDisks where
        type Rs ListLocalDisks = ListLocalDisksResponse
        request = postJSON storageGateway
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
          = object
              (catMaybes [Just ("GatewayARN" .= _lldGatewayARN)])

instance ToPath ListLocalDisks where
        toPath = const "/"

instance ToQuery ListLocalDisks where
        toQuery = const mempty

-- | /See:/ 'listLocalDisksResponse' smart constructor.
data ListLocalDisksResponse = ListLocalDisksResponse'
    { _lldrsGatewayARN :: !(Maybe Text)
    , _lldrsDisks      :: !(Maybe [Disk])
    , _lldrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListLocalDisksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lldrsGatewayARN'
--
-- * 'lldrsDisks'
--
-- * 'lldrsStatus'
listLocalDisksResponse
    :: Int -- ^ 'lldrsStatus'
    -> ListLocalDisksResponse
listLocalDisksResponse pStatus_ =
    ListLocalDisksResponse'
    { _lldrsGatewayARN = Nothing
    , _lldrsDisks = Nothing
    , _lldrsStatus = pStatus_
    }

-- | Undocumented member.
lldrsGatewayARN :: Lens' ListLocalDisksResponse (Maybe Text)
lldrsGatewayARN = lens _lldrsGatewayARN (\ s a -> s{_lldrsGatewayARN = a});

-- | Undocumented member.
lldrsDisks :: Lens' ListLocalDisksResponse [Disk]
lldrsDisks = lens _lldrsDisks (\ s a -> s{_lldrsDisks = a}) . _Default . _Coerce;

-- | The response status code.
lldrsStatus :: Lens' ListLocalDisksResponse Int
lldrsStatus = lens _lldrsStatus (\ s a -> s{_lldrsStatus = a});
