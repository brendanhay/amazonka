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
-- Module      : Network.AWS.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular @UnlockCode@ value can be accessed for up to 90 days after the associated job has been created.
--
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric characters and 4 hyphens. This code is used to decrypt the manifest file when it is passed along with the manifest to the Snowball through the Snowball client when the client is started for the first time.
--
-- As a best practice, we recommend that you don't save a copy of the @UnlockCode@ in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snowball associated with that job.
--
module Network.AWS.Snowball.GetJobUnlockCode
    (
    -- * Creating a Request
      getJobUnlockCode
    , GetJobUnlockCode
    -- * Request Lenses
    , gjucJobId

    -- * Destructuring the Response
    , getJobUnlockCodeResponse
    , GetJobUnlockCodeResponse
    -- * Response Lenses
    , gjucrsUnlockCode
    , gjucrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'getJobUnlockCode' smart constructor.
newtype GetJobUnlockCode = GetJobUnlockCode'
  { _gjucJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobUnlockCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjucJobId' - The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
getJobUnlockCode
    :: Text -- ^ 'gjucJobId'
    -> GetJobUnlockCode
getJobUnlockCode pJobId_ = GetJobUnlockCode' {_gjucJobId = pJobId_}


-- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
gjucJobId :: Lens' GetJobUnlockCode Text
gjucJobId = lens _gjucJobId (\ s a -> s{_gjucJobId = a})

instance AWSRequest GetJobUnlockCode where
        type Rs GetJobUnlockCode = GetJobUnlockCodeResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 GetJobUnlockCodeResponse' <$>
                   (x .?> "UnlockCode") <*> (pure (fromEnum s)))

instance Hashable GetJobUnlockCode where

instance NFData GetJobUnlockCode where

instance ToHeaders GetJobUnlockCode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.GetJobUnlockCode"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJobUnlockCode where
        toJSON GetJobUnlockCode'{..}
          = object (catMaybes [Just ("JobId" .= _gjucJobId)])

instance ToPath GetJobUnlockCode where
        toPath = const "/"

instance ToQuery GetJobUnlockCode where
        toQuery = const mempty

-- | /See:/ 'getJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { _gjucrsUnlockCode     :: !(Maybe Text)
  , _gjucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobUnlockCodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjucrsUnlockCode' - The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
--
-- * 'gjucrsResponseStatus' - -- | The response status code.
getJobUnlockCodeResponse
    :: Int -- ^ 'gjucrsResponseStatus'
    -> GetJobUnlockCodeResponse
getJobUnlockCodeResponse pResponseStatus_ =
  GetJobUnlockCodeResponse'
    {_gjucrsUnlockCode = Nothing, _gjucrsResponseStatus = pResponseStatus_}


-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
gjucrsUnlockCode :: Lens' GetJobUnlockCodeResponse (Maybe Text)
gjucrsUnlockCode = lens _gjucrsUnlockCode (\ s a -> s{_gjucrsUnlockCode = a})

-- | -- | The response status code.
gjucrsResponseStatus :: Lens' GetJobUnlockCodeResponse Int
gjucrsResponseStatus = lens _gjucrsResponseStatus (\ s a -> s{_gjucrsResponseStatus = a})

instance NFData GetJobUnlockCodeResponse where
