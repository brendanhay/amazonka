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
-- Module      : Network.AWS.Glue.DeleteJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified job definition. If the job definition is not found, no exception is thrown.
--
--
module Network.AWS.Glue.DeleteJob
    (
    -- * Creating a Request
      deleteJob
    , DeleteJob
    -- * Request Lenses
    , djJobName

    -- * Destructuring the Response
    , deleteJobResponse
    , DeleteJobResponse
    -- * Response Lenses
    , djrsJobName
    , djrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteJob' smart constructor.
newtype DeleteJob = DeleteJob'
  { _djJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djJobName' - The name of the job definition to delete.
deleteJob
    :: Text -- ^ 'djJobName'
    -> DeleteJob
deleteJob pJobName_ = DeleteJob' {_djJobName = pJobName_}


-- | The name of the job definition to delete.
djJobName :: Lens' DeleteJob Text
djJobName = lens _djJobName (\ s a -> s{_djJobName = a})

instance AWSRequest DeleteJob where
        type Rs DeleteJob = DeleteJobResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 DeleteJobResponse' <$>
                   (x .?> "JobName") <*> (pure (fromEnum s)))

instance Hashable DeleteJob where

instance NFData DeleteJob where

instance ToHeaders DeleteJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteJob where
        toJSON DeleteJob'{..}
          = object (catMaybes [Just ("JobName" .= _djJobName)])

instance ToPath DeleteJob where
        toPath = const "/"

instance ToQuery DeleteJob where
        toQuery = const mempty

-- | /See:/ 'deleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { _djrsJobName        :: !(Maybe Text)
  , _djrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsJobName' - The name of the job definition that was deleted.
--
-- * 'djrsResponseStatus' - -- | The response status code.
deleteJobResponse
    :: Int -- ^ 'djrsResponseStatus'
    -> DeleteJobResponse
deleteJobResponse pResponseStatus_ =
  DeleteJobResponse'
    {_djrsJobName = Nothing, _djrsResponseStatus = pResponseStatus_}


-- | The name of the job definition that was deleted.
djrsJobName :: Lens' DeleteJobResponse (Maybe Text)
djrsJobName = lens _djrsJobName (\ s a -> s{_djrsJobName = a})

-- | -- | The response status code.
djrsResponseStatus :: Lens' DeleteJobResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\ s a -> s{_djrsResponseStatus = a})

instance NFData DeleteJobResponse where
