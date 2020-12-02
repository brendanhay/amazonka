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
-- Module      : Network.AWS.IoT.GetJobDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a job document.
--
--
module Network.AWS.IoT.GetJobDocument
    (
    -- * Creating a Request
      getJobDocument
    , GetJobDocument
    -- * Request Lenses
    , gjdJobId

    -- * Destructuring the Response
    , getJobDocumentResponse
    , GetJobDocumentResponse
    -- * Response Lenses
    , gjdrsDocument
    , gjdrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJobDocument' smart constructor.
newtype GetJobDocument = GetJobDocument'
  { _gjdJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdJobId' - The unique identifier you assigned to this job when it was created.
getJobDocument
    :: Text -- ^ 'gjdJobId'
    -> GetJobDocument
getJobDocument pJobId_ = GetJobDocument' {_gjdJobId = pJobId_}


-- | The unique identifier you assigned to this job when it was created.
gjdJobId :: Lens' GetJobDocument Text
gjdJobId = lens _gjdJobId (\ s a -> s{_gjdJobId = a})

instance AWSRequest GetJobDocument where
        type Rs GetJobDocument = GetJobDocumentResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetJobDocumentResponse' <$>
                   (x .?> "document") <*> (pure (fromEnum s)))

instance Hashable GetJobDocument where

instance NFData GetJobDocument where

instance ToHeaders GetJobDocument where
        toHeaders = const mempty

instance ToPath GetJobDocument where
        toPath GetJobDocument'{..}
          = mconcat ["/jobs/", toBS _gjdJobId, "/job-document"]

instance ToQuery GetJobDocument where
        toQuery = const mempty

-- | /See:/ 'getJobDocumentResponse' smart constructor.
data GetJobDocumentResponse = GetJobDocumentResponse'
  { _gjdrsDocument       :: !(Maybe Text)
  , _gjdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdrsDocument' - The job document content.
--
-- * 'gjdrsResponseStatus' - -- | The response status code.
getJobDocumentResponse
    :: Int -- ^ 'gjdrsResponseStatus'
    -> GetJobDocumentResponse
getJobDocumentResponse pResponseStatus_ =
  GetJobDocumentResponse'
    {_gjdrsDocument = Nothing, _gjdrsResponseStatus = pResponseStatus_}


-- | The job document content.
gjdrsDocument :: Lens' GetJobDocumentResponse (Maybe Text)
gjdrsDocument = lens _gjdrsDocument (\ s a -> s{_gjdrsDocument = a})

-- | -- | The response status code.
gjdrsResponseStatus :: Lens' GetJobDocumentResponse Int
gjdrsResponseStatus = lens _gjdrsResponseStatus (\ s a -> s{_gjdrsResponseStatus = a})

instance NFData GetJobDocumentResponse where
