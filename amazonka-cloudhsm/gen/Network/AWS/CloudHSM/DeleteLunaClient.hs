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
-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a client.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteLunaClient.html AWS API Reference> for DeleteLunaClient.
module Network.AWS.CloudHSM.DeleteLunaClient
    (
    -- * Creating a Request
      deleteLunaClient
    , DeleteLunaClient
    -- * Request Lenses
    , dClientARN

    -- * Destructuring the Response
    , deleteLunaClientResponse
    , DeleteLunaClientResponse
    -- * Response Lenses
    , dlcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLunaClient' smart constructor.
newtype DeleteLunaClient = DeleteLunaClient'
    { _dClientARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLunaClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dClientARN'
deleteLunaClient
    :: Text -- ^ 'dClientARN'
    -> DeleteLunaClient
deleteLunaClient pClientARN_ =
    DeleteLunaClient'
    { _dClientARN = pClientARN_
    }

-- | The ARN of the client to delete.
dClientARN :: Lens' DeleteLunaClient Text
dClientARN = lens _dClientARN (\ s a -> s{_dClientARN = a});

instance AWSRequest DeleteLunaClient where
        type Rs DeleteLunaClient = DeleteLunaClientResponse
        request = postJSON cloudHSM
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLunaClientResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteLunaClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteLunaClient" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLunaClient where
        toJSON DeleteLunaClient'{..}
          = object
              (catMaybes [Just ("ClientArn" .= _dClientARN)])

instance ToPath DeleteLunaClient where
        toPath = const "/"

instance ToQuery DeleteLunaClient where
        toQuery = const mempty

-- | /See:/ 'deleteLunaClientResponse' smart constructor.
newtype DeleteLunaClientResponse = DeleteLunaClientResponse'
    { _dlcrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLunaClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcrsStatus'
deleteLunaClientResponse
    :: Int -- ^ 'dlcrsStatus'
    -> DeleteLunaClientResponse
deleteLunaClientResponse pStatus_ =
    DeleteLunaClientResponse'
    { _dlcrsStatus = pStatus_
    }

-- | The response status code.
dlcrsStatus :: Lens' DeleteLunaClientResponse Int
dlcrsStatus = lens _dlcrsStatus (\ s a -> s{_dlcrsStatus = a});
