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
-- Module      : Network.AWS.CloudHSM.DeleteHSM
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.
--
--
module Network.AWS.CloudHSM.DeleteHSM
    (
    -- * Creating a Request
      deleteHSM
    , DeleteHSM
    -- * Request Lenses
    , dhHSMARN

    -- * Destructuring the Response
    , deleteHSMResponse
    , DeleteHSMResponse
    -- * Response Lenses
    , dhsmrsResponseStatus
    , dhsmrsStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'DeleteHsm' operation.
--
--
--
-- /See:/ 'deleteHSM' smart constructor.
newtype DeleteHSM = DeleteHSM'
  { _dhHSMARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhHSMARN' - The ARN of the HSM to delete.
deleteHSM
    :: Text -- ^ 'dhHSMARN'
    -> DeleteHSM
deleteHSM pHSMARN_ = DeleteHSM' {_dhHSMARN = pHSMARN_}


-- | The ARN of the HSM to delete.
dhHSMARN :: Lens' DeleteHSM Text
dhHSMARN = lens _dhHSMARN (\ s a -> s{_dhHSMARN = a});

instance AWSRequest DeleteHSM where
        type Rs DeleteHSM = DeleteHSMResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 DeleteHSMResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Status"))

instance Hashable DeleteHSM where

instance NFData DeleteHSM where

instance ToHeaders DeleteHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteHsm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHSM where
        toJSON DeleteHSM'{..}
          = object (catMaybes [Just ("HsmArn" .= _dhHSMARN)])

instance ToPath DeleteHSM where
        toPath = const "/"

instance ToQuery DeleteHSM where
        toQuery = const mempty

-- | Contains the output of the 'DeleteHsm' operation.
--
--
--
-- /See:/ 'deleteHSMResponse' smart constructor.
data DeleteHSMResponse = DeleteHSMResponse'
  { _dhsmrsResponseStatus :: {-# NOUNPACK #-}!Int
  , _dhsmrsStatus         :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhsmrsResponseStatus' - -- | The response status code.
--
-- * 'dhsmrsStatus' - The status of the operation.
deleteHSMResponse
    :: Int -- ^ 'dhsmrsResponseStatus'
    -> Text -- ^ 'dhsmrsStatus'
    -> DeleteHSMResponse
deleteHSMResponse pResponseStatus_ pStatus_ =
  DeleteHSMResponse'
  {_dhsmrsResponseStatus = pResponseStatus_, _dhsmrsStatus = pStatus_}


-- | -- | The response status code.
dhsmrsResponseStatus :: Lens' DeleteHSMResponse Int
dhsmrsResponseStatus = lens _dhsmrsResponseStatus (\ s a -> s{_dhsmrsResponseStatus = a});

-- | The status of the operation.
dhsmrsStatus :: Lens' DeleteHSMResponse Text
dhsmrsStatus = lens _dhsmrsStatus (\ s a -> s{_dhsmrsStatus = a});

instance NFData DeleteHSMResponse where
