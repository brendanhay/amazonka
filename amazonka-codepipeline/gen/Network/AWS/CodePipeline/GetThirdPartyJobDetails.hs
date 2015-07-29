{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Only used for
-- partner actions.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the Amazon S3 bucket used to store artifacts for the pipeline, if
-- the action requires access to that Amazon S3 bucket for input or output
-- artifacts. Additionally, this API returns any secret values defined for
-- the action.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_GetThirdPartyJobDetails.html>
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
    (
    -- * Request
      GetThirdPartyJobDetails
    -- ** Request constructor
    , getThirdPartyJobDetails
    -- ** Request lenses
    , gtpjdJobId
    , gtpjdClientToken

    -- * Response
    , GetThirdPartyJobDetailsResponse
    -- ** Response constructor
    , getThirdPartyJobDetailsResponse
    -- ** Response lenses
    , gtpjdrsJobDetails
    , gtpjdrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get third party job details action.
--
-- /See:/ 'getThirdPartyJobDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtpjdJobId'
--
-- * 'gtpjdClientToken'
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
    { _gtpjdJobId       :: !Text
    , _gtpjdClientToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetThirdPartyJobDetails' smart constructor.
getThirdPartyJobDetails :: Text -> Text -> GetThirdPartyJobDetails
getThirdPartyJobDetails pJobId_ pClientToken_ =
    GetThirdPartyJobDetails'
    { _gtpjdJobId = pJobId_
    , _gtpjdClientToken = pClientToken_
    }

-- | The unique system-generated ID used for identifying the job.
gtpjdJobId :: Lens' GetThirdPartyJobDetails Text
gtpjdJobId = lens _gtpjdJobId (\ s a -> s{_gtpjdJobId = a});

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
gtpjdClientToken :: Lens' GetThirdPartyJobDetails Text
gtpjdClientToken = lens _gtpjdClientToken (\ s a -> s{_gtpjdClientToken = a});

instance AWSRequest GetThirdPartyJobDetails where
        type Sv GetThirdPartyJobDetails = CodePipeline
        type Rs GetThirdPartyJobDetails =
             GetThirdPartyJobDetailsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetThirdPartyJobDetailsResponse' <$>
                   (x .?> "jobDetails") <*> (pure (fromEnum s)))

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
              ["jobId" .= _gtpjdJobId,
               "clientToken" .= _gtpjdClientToken]

instance ToPath GetThirdPartyJobDetails where
        toPath = const mempty

instance ToQuery GetThirdPartyJobDetails where
        toQuery = const mempty

-- | Represents the output of a get third party job details action.
--
-- /See:/ 'getThirdPartyJobDetailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtpjdrsJobDetails'
--
-- * 'gtpjdrsStatus'
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
    { _gtpjdrsJobDetails :: !(Maybe ThirdPartyJobDetails)
    , _gtpjdrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetThirdPartyJobDetailsResponse' smart constructor.
getThirdPartyJobDetailsResponse :: Int -> GetThirdPartyJobDetailsResponse
getThirdPartyJobDetailsResponse pStatus_ =
    GetThirdPartyJobDetailsResponse'
    { _gtpjdrsJobDetails = Nothing
    , _gtpjdrsStatus = pStatus_
    }

-- | The details of the job, including any protected values defined for the
-- job.
gtpjdrsJobDetails :: Lens' GetThirdPartyJobDetailsResponse (Maybe ThirdPartyJobDetails)
gtpjdrsJobDetails = lens _gtpjdrsJobDetails (\ s a -> s{_gtpjdrsJobDetails = a});

-- | FIXME: Undocumented member.
gtpjdrsStatus :: Lens' GetThirdPartyJobDetailsResponse Int
gtpjdrsStatus = lens _gtpjdrsStatus (\ s a -> s{_gtpjdrsStatus = a});
