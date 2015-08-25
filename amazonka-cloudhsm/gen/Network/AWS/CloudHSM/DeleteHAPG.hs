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
-- Module      : Network.AWS.CloudHSM.DeleteHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a high-availability partition group.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHAPG.html AWS API Reference> for DeleteHAPG.
module Network.AWS.CloudHSM.DeleteHAPG
    (
    -- * Creating a Request
      deleteHAPG
    , DeleteHAPG
    -- * Request Lenses
    , dhHAPGARN

    -- * Destructuring the Response
    , deleteHAPGResponse
    , DeleteHAPGResponse
    -- * Response Lenses
    , dhrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DeleteHapg action.
--
-- /See:/ 'deleteHAPG' smart constructor.
newtype DeleteHAPG = DeleteHAPG'
    { _dhHAPGARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHAPG' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhHAPGARN'
deleteHAPG
    :: Text -- ^ 'dhHAPGARN'
    -> DeleteHAPG
deleteHAPG pHAPGARN_ =
    DeleteHAPG'
    { _dhHAPGARN = pHAPGARN_
    }

-- | The ARN of the high-availability partition group to delete.
dhHAPGARN :: Lens' DeleteHAPG Text
dhHAPGARN = lens _dhHAPGARN (\ s a -> s{_dhHAPGARN = a});

instance AWSRequest DeleteHAPG where
        type Rs DeleteHAPG = DeleteHAPGResponse
        request = postJSON cloudHSM
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteHAPGResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHAPG where
        toJSON DeleteHAPG'{..}
          = object (catMaybes [Just ("HapgArn" .= _dhHAPGARN)])

instance ToPath DeleteHAPG where
        toPath = const "/"

instance ToQuery DeleteHAPG where
        toQuery = const mempty

-- | Contains the output of the DeleteHapg action.
--
-- /See:/ 'deleteHAPGResponse' smart constructor.
newtype DeleteHAPGResponse = DeleteHAPGResponse'
    { _dhrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHAPGResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrsStatus'
deleteHAPGResponse
    :: Int -- ^ 'dhrsStatus'
    -> DeleteHAPGResponse
deleteHAPGResponse pStatus_ =
    DeleteHAPGResponse'
    { _dhrsStatus = pStatus_
    }

-- | The response status code.
dhrsStatus :: Lens' DeleteHAPGResponse Int
dhrsStatus = lens _dhrsStatus (\ s a -> s{_dhrsStatus = a});
