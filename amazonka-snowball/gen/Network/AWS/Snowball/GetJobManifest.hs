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
-- Module      : Network.AWS.Snowball.GetJobManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to an Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value. You can access the manifest file for up to 60 minutes after this request has been made. To access the manifest file after 60 minutes have passed, you'll have to make another call to the @GetJobManifest@ action.
--
--
-- The manifest is an encrypted file that you can download after your job enters the @WithCustomer@ status. The manifest is decrypted by using the @UnlockCode@ code value, when you pass both values to the Snowball through the Snowball client when the client is started for the first time.
--
-- As a best practice, we recommend that you don't save a copy of an @UnlockCode@ value in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snowball associated with that job.
--
-- The credentials of a given job, including its manifest file and unlock code, expire 90 days after the job is created.
--
module Network.AWS.Snowball.GetJobManifest
    (
    -- * Creating a Request
      getJobManifest
    , GetJobManifest
    -- * Request Lenses
    , gjmJobId

    -- * Destructuring the Response
    , getJobManifestResponse
    , GetJobManifestResponse
    -- * Response Lenses
    , gjmrsManifestURI
    , gjmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'getJobManifest' smart constructor.
newtype GetJobManifest = GetJobManifest'
  { _gjmJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjmJobId' - The ID for a job that you want to get the manifest file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
getJobManifest
    :: Text -- ^ 'gjmJobId'
    -> GetJobManifest
getJobManifest pJobId_ = GetJobManifest' {_gjmJobId = pJobId_}


-- | The ID for a job that you want to get the manifest file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
gjmJobId :: Lens' GetJobManifest Text
gjmJobId = lens _gjmJobId (\ s a -> s{_gjmJobId = a})

instance AWSRequest GetJobManifest where
        type Rs GetJobManifest = GetJobManifestResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 GetJobManifestResponse' <$>
                   (x .?> "ManifestURI") <*> (pure (fromEnum s)))

instance Hashable GetJobManifest where

instance NFData GetJobManifest where

instance ToHeaders GetJobManifest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.GetJobManifest"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJobManifest where
        toJSON GetJobManifest'{..}
          = object (catMaybes [Just ("JobId" .= _gjmJobId)])

instance ToPath GetJobManifest where
        toPath = const "/"

instance ToQuery GetJobManifest where
        toQuery = const mempty

-- | /See:/ 'getJobManifestResponse' smart constructor.
data GetJobManifestResponse = GetJobManifestResponse'
  { _gjmrsManifestURI    :: !(Maybe Text)
  , _gjmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobManifestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjmrsManifestURI' - The Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value.
--
-- * 'gjmrsResponseStatus' - -- | The response status code.
getJobManifestResponse
    :: Int -- ^ 'gjmrsResponseStatus'
    -> GetJobManifestResponse
getJobManifestResponse pResponseStatus_ =
  GetJobManifestResponse'
    {_gjmrsManifestURI = Nothing, _gjmrsResponseStatus = pResponseStatus_}


-- | The Amazon S3 presigned URL for the manifest file associated with the specified @JobId@ value.
gjmrsManifestURI :: Lens' GetJobManifestResponse (Maybe Text)
gjmrsManifestURI = lens _gjmrsManifestURI (\ s a -> s{_gjmrsManifestURI = a})

-- | -- | The response status code.
gjmrsResponseStatus :: Lens' GetJobManifestResponse Int
gjmrsResponseStatus = lens _gjmrsResponseStatus (\ s a -> s{_gjmrsResponseStatus = a})

instance NFData GetJobManifestResponse where
