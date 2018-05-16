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
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.
--
--
module Network.AWS.StorageGateway.ListVolumeInitiators
    (
    -- * Creating a Request
      listVolumeInitiators
    , ListVolumeInitiators
    -- * Request Lenses
    , lviVolumeARN

    -- * Destructuring the Response
    , listVolumeInitiatorsResponse
    , ListVolumeInitiatorsResponse
    -- * Response Lenses
    , lvirsInitiators
    , lvirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | ListVolumeInitiatorsInput
--
--
--
-- /See:/ 'listVolumeInitiators' smart constructor.
newtype ListVolumeInitiators = ListVolumeInitiators'
  { _lviVolumeARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumeInitiators' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lviVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
listVolumeInitiators
    :: Text -- ^ 'lviVolumeARN'
    -> ListVolumeInitiators
listVolumeInitiators pVolumeARN_ =
  ListVolumeInitiators' {_lviVolumeARN = pVolumeARN_}


-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
lviVolumeARN :: Lens' ListVolumeInitiators Text
lviVolumeARN = lens _lviVolumeARN (\ s a -> s{_lviVolumeARN = a})

instance AWSRequest ListVolumeInitiators where
        type Rs ListVolumeInitiators =
             ListVolumeInitiatorsResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumeInitiatorsResponse' <$>
                   (x .?> "Initiators" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListVolumeInitiators where

instance NFData ListVolumeInitiators where

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
          = object
              (catMaybes [Just ("VolumeARN" .= _lviVolumeARN)])

instance ToPath ListVolumeInitiators where
        toPath = const "/"

instance ToQuery ListVolumeInitiators where
        toQuery = const mempty

-- | ListVolumeInitiatorsOutput
--
--
--
-- /See:/ 'listVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { _lvirsInitiators     :: !(Maybe [Text])
  , _lvirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumeInitiatorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvirsInitiators' - The host names and port numbers of all iSCSI initiators that are connected to the gateway.
--
-- * 'lvirsResponseStatus' - -- | The response status code.
listVolumeInitiatorsResponse
    :: Int -- ^ 'lvirsResponseStatus'
    -> ListVolumeInitiatorsResponse
listVolumeInitiatorsResponse pResponseStatus_ =
  ListVolumeInitiatorsResponse'
    {_lvirsInitiators = Nothing, _lvirsResponseStatus = pResponseStatus_}


-- | The host names and port numbers of all iSCSI initiators that are connected to the gateway.
lvirsInitiators :: Lens' ListVolumeInitiatorsResponse [Text]
lvirsInitiators = lens _lvirsInitiators (\ s a -> s{_lvirsInitiators = a}) . _Default . _Coerce

-- | -- | The response status code.
lvirsResponseStatus :: Lens' ListVolumeInitiatorsResponse Int
lvirsResponseStatus = lens _lvirsResponseStatus (\ s a -> s{_lvirsResponseStatus = a})

instance NFData ListVolumeInitiatorsResponse where
