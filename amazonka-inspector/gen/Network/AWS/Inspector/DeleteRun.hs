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
-- Module      : Network.AWS.Inspector.DeleteRun
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run specified by the run ARN.
module Network.AWS.Inspector.DeleteRun
    (
    -- * Creating a Request
      deleteRun
    , DeleteRun
    -- * Request Lenses
    , dRunARN

    -- * Destructuring the Response
    , deleteRunResponse
    , DeleteRunResponse
    -- * Response Lenses
    , delrsMessage
    , delrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRun' smart constructor.
newtype DeleteRun = DeleteRun'
    { _dRunARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRunARN'
deleteRun
    :: Text -- ^ 'dRunARN'
    -> DeleteRun
deleteRun pRunARN_ =
    DeleteRun'
    { _dRunARN = pRunARN_
    }

-- | The ARN specifying the assessment run that you want to delete.
dRunARN :: Lens' DeleteRun Text
dRunARN = lens _dRunARN (\ s a -> s{_dRunARN = a});

instance AWSRequest DeleteRun where
        type Rs DeleteRun = DeleteRunResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRunResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders DeleteRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRun where
        toJSON DeleteRun'{..}
          = object (catMaybes [Just ("runArn" .= _dRunARN)])

instance ToPath DeleteRun where
        toPath = const "/"

instance ToQuery DeleteRun where
        toQuery = const mempty

-- | /See:/ 'deleteRunResponse' smart constructor.
data DeleteRunResponse = DeleteRunResponse'
    { _delrsMessage        :: !(Maybe Text)
    , _delrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsMessage'
--
-- * 'delrsResponseStatus'
deleteRunResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteRunResponse
deleteRunResponse pResponseStatus_ =
    DeleteRunResponse'
    { _delrsMessage = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
delrsMessage :: Lens' DeleteRunResponse (Maybe Text)
delrsMessage = lens _delrsMessage (\ s a -> s{_delrsMessage = a});

-- | The response status code.
delrsResponseStatus :: Lens' DeleteRunResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});
