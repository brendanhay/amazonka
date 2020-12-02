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
-- Module      : Network.AWS.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
--
-- __Note__ Deleting this resource does not stop an in-progress run.
--
module Network.AWS.DeviceFarm.DeleteRun
    (
    -- * Creating a Request
      deleteRun
    , DeleteRun
    -- * Request Lenses
    , drArn

    -- * Destructuring the Response
    , deleteRunResponse
    , DeleteRunResponse
    -- * Response Lenses
    , drrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the delete run operation.
--
--
--
-- /See:/ 'deleteRun' smart constructor.
newtype DeleteRun = DeleteRun'
  { _drArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drArn' - The Amazon Resource Name (ARN) for the run you wish to delete.
deleteRun
    :: Text -- ^ 'drArn'
    -> DeleteRun
deleteRun pArn_ = DeleteRun' {_drArn = pArn_}


-- | The Amazon Resource Name (ARN) for the run you wish to delete.
drArn :: Lens' DeleteRun Text
drArn = lens _drArn (\ s a -> s{_drArn = a})

instance AWSRequest DeleteRun where
        type Rs DeleteRun = DeleteRunResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteRunResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteRun where

instance NFData DeleteRun where

instance ToHeaders DeleteRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRun where
        toJSON DeleteRun'{..}
          = object (catMaybes [Just ("arn" .= _drArn)])

instance ToPath DeleteRun where
        toPath = const "/"

instance ToQuery DeleteRun where
        toQuery = const mempty

-- | Represents the result of a delete run request.
--
--
--
-- /See:/ 'deleteRunResponse' smart constructor.
newtype DeleteRunResponse = DeleteRunResponse'
  { _drrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRunResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DeleteRunResponse
deleteRunResponse pResponseStatus_ =
  DeleteRunResponse' {_drrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRunResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DeleteRunResponse where
