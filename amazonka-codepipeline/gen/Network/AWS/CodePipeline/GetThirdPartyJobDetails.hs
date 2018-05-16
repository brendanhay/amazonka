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
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Only used for partner actions.
--
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.
--
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
    (
    -- * Creating a Request
      getThirdPartyJobDetails
    , GetThirdPartyJobDetails
    -- * Request Lenses
    , gtpjdJobId
    , gtpjdClientToken

    -- * Destructuring the Response
    , getThirdPartyJobDetailsResponse
    , GetThirdPartyJobDetailsResponse
    -- * Response Lenses
    , gtpjdrsJobDetails
    , gtpjdrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetThirdPartyJobDetails action.
--
--
--
-- /See:/ 'getThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { _gtpjdJobId       :: !Text
  , _gtpjdClientToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetThirdPartyJobDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtpjdJobId' - The unique system-generated ID used for identifying the job.
--
-- * 'gtpjdClientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
getThirdPartyJobDetails
    :: Text -- ^ 'gtpjdJobId'
    -> Text -- ^ 'gtpjdClientToken'
    -> GetThirdPartyJobDetails
getThirdPartyJobDetails pJobId_ pClientToken_ =
  GetThirdPartyJobDetails'
    {_gtpjdJobId = pJobId_, _gtpjdClientToken = pClientToken_}


-- | The unique system-generated ID used for identifying the job.
gtpjdJobId :: Lens' GetThirdPartyJobDetails Text
gtpjdJobId = lens _gtpjdJobId (\ s a -> s{_gtpjdJobId = a})

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
gtpjdClientToken :: Lens' GetThirdPartyJobDetails Text
gtpjdClientToken = lens _gtpjdClientToken (\ s a -> s{_gtpjdClientToken = a})

instance AWSRequest GetThirdPartyJobDetails where
        type Rs GetThirdPartyJobDetails =
             GetThirdPartyJobDetailsResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 GetThirdPartyJobDetailsResponse' <$>
                   (x .?> "jobDetails") <*> (pure (fromEnum s)))

instance Hashable GetThirdPartyJobDetails where

instance NFData GetThirdPartyJobDetails where

instance ToHeaders GetThirdPartyJobDetails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.GetThirdPartyJobDetails" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetThirdPartyJobDetails where
        toJSON GetThirdPartyJobDetails'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _gtpjdJobId),
                  Just ("clientToken" .= _gtpjdClientToken)])

instance ToPath GetThirdPartyJobDetails where
        toPath = const "/"

instance ToQuery GetThirdPartyJobDetails where
        toQuery = const mempty

-- | Represents the output of a GetThirdPartyJobDetails action.
--
--
--
-- /See:/ 'getThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { _gtpjdrsJobDetails     :: !(Maybe ThirdPartyJobDetails)
  , _gtpjdrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetThirdPartyJobDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtpjdrsJobDetails' - The details of the job, including any protected values defined for the job.
--
-- * 'gtpjdrsResponseStatus' - -- | The response status code.
getThirdPartyJobDetailsResponse
    :: Int -- ^ 'gtpjdrsResponseStatus'
    -> GetThirdPartyJobDetailsResponse
getThirdPartyJobDetailsResponse pResponseStatus_ =
  GetThirdPartyJobDetailsResponse'
    {_gtpjdrsJobDetails = Nothing, _gtpjdrsResponseStatus = pResponseStatus_}


-- | The details of the job, including any protected values defined for the job.
gtpjdrsJobDetails :: Lens' GetThirdPartyJobDetailsResponse (Maybe ThirdPartyJobDetails)
gtpjdrsJobDetails = lens _gtpjdrsJobDetails (\ s a -> s{_gtpjdrsJobDetails = a})

-- | -- | The response status code.
gtpjdrsResponseStatus :: Lens' GetThirdPartyJobDetailsResponse Int
gtpjdrsResponseStatus = lens _gtpjdrsResponseStatus (\ s a -> s{_gtpjdrsResponseStatus = a})

instance NFData GetThirdPartyJobDetailsResponse where
