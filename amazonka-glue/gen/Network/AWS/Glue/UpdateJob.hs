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
-- Module      : Network.AWS.Glue.UpdateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition.
--
--
module Network.AWS.Glue.UpdateJob
    (
    -- * Creating a Request
      updateJob
    , UpdateJob
    -- * Request Lenses
    , ujJobName
    , ujJobUpdate

    -- * Destructuring the Response
    , updateJobResponse
    , UpdateJobResponse
    -- * Response Lenses
    , ujrsJobName
    , ujrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJob' smart constructor.
data UpdateJob = UpdateJob'
  { _ujJobName   :: !Text
  , _ujJobUpdate :: !JobUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujJobName' - Name of the job definition to update.
--
-- * 'ujJobUpdate' - Specifies the values with which to update the job definition.
updateJob
    :: Text -- ^ 'ujJobName'
    -> JobUpdate -- ^ 'ujJobUpdate'
    -> UpdateJob
updateJob pJobName_ pJobUpdate_ =
  UpdateJob' {_ujJobName = pJobName_, _ujJobUpdate = pJobUpdate_}


-- | Name of the job definition to update.
ujJobName :: Lens' UpdateJob Text
ujJobName = lens _ujJobName (\ s a -> s{_ujJobName = a})

-- | Specifies the values with which to update the job definition.
ujJobUpdate :: Lens' UpdateJob JobUpdate
ujJobUpdate = lens _ujJobUpdate (\ s a -> s{_ujJobUpdate = a})

instance AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 UpdateJobResponse' <$>
                   (x .?> "JobName") <*> (pure (fromEnum s)))

instance Hashable UpdateJob where

instance NFData UpdateJob where

instance ToHeaders UpdateJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateJob where
        toJSON UpdateJob'{..}
          = object
              (catMaybes
                 [Just ("JobName" .= _ujJobName),
                  Just ("JobUpdate" .= _ujJobUpdate)])

instance ToPath UpdateJob where
        toPath = const "/"

instance ToQuery UpdateJob where
        toQuery = const mempty

-- | /See:/ 'updateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { _ujrsJobName        :: !(Maybe Text)
  , _ujrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujrsJobName' - Returns the name of the updated job definition.
--
-- * 'ujrsResponseStatus' - -- | The response status code.
updateJobResponse
    :: Int -- ^ 'ujrsResponseStatus'
    -> UpdateJobResponse
updateJobResponse pResponseStatus_ =
  UpdateJobResponse'
    {_ujrsJobName = Nothing, _ujrsResponseStatus = pResponseStatus_}


-- | Returns the name of the updated job definition.
ujrsJobName :: Lens' UpdateJobResponse (Maybe Text)
ujrsJobName = lens _ujrsJobName (\ s a -> s{_ujrsJobName = a})

-- | -- | The response status code.
ujrsResponseStatus :: Lens' UpdateJobResponse Int
ujrsResponseStatus = lens _ujrsResponseStatus (\ s a -> s{_ujrsResponseStatus = a})

instance NFData UpdateJobResponse where
