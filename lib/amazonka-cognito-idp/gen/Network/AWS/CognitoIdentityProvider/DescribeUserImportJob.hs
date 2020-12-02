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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user import job.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
    (
    -- * Creating a Request
      describeUserImportJob
    , DescribeUserImportJob
    -- * Request Lenses
    , duijUserPoolId
    , duijJobId

    -- * Destructuring the Response
    , describeUserImportJobResponse
    , DescribeUserImportJobResponse
    -- * Response Lenses
    , duijrsUserImportJob
    , duijrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to describe the user import job.
--
--
--
-- /See:/ 'describeUserImportJob' smart constructor.
data DescribeUserImportJob = DescribeUserImportJob'
  { _duijUserPoolId :: !Text
  , _duijJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duijUserPoolId' - The user pool ID for the user pool that the users are being imported into.
--
-- * 'duijJobId' - The job ID for the user import job.
describeUserImportJob
    :: Text -- ^ 'duijUserPoolId'
    -> Text -- ^ 'duijJobId'
    -> DescribeUserImportJob
describeUserImportJob pUserPoolId_ pJobId_ =
  DescribeUserImportJob' {_duijUserPoolId = pUserPoolId_, _duijJobId = pJobId_}


-- | The user pool ID for the user pool that the users are being imported into.
duijUserPoolId :: Lens' DescribeUserImportJob Text
duijUserPoolId = lens _duijUserPoolId (\ s a -> s{_duijUserPoolId = a})

-- | The job ID for the user import job.
duijJobId :: Lens' DescribeUserImportJob Text
duijJobId = lens _duijJobId (\ s a -> s{_duijJobId = a})

instance AWSRequest DescribeUserImportJob where
        type Rs DescribeUserImportJob =
             DescribeUserImportJobResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserImportJobResponse' <$>
                   (x .?> "UserImportJob") <*> (pure (fromEnum s)))

instance Hashable DescribeUserImportJob where

instance NFData DescribeUserImportJob where

instance ToHeaders DescribeUserImportJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeUserImportJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserImportJob where
        toJSON DescribeUserImportJob'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _duijUserPoolId),
                  Just ("JobId" .= _duijJobId)])

instance ToPath DescribeUserImportJob where
        toPath = const "/"

instance ToQuery DescribeUserImportJob where
        toQuery = const mempty

-- | Represents the response from the server to the request to describe the user import job.
--
--
--
-- /See:/ 'describeUserImportJobResponse' smart constructor.
data DescribeUserImportJobResponse = DescribeUserImportJobResponse'
  { _duijrsUserImportJob  :: !(Maybe UserImportJobType)
  , _duijrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duijrsUserImportJob' - The job object that represents the user import job.
--
-- * 'duijrsResponseStatus' - -- | The response status code.
describeUserImportJobResponse
    :: Int -- ^ 'duijrsResponseStatus'
    -> DescribeUserImportJobResponse
describeUserImportJobResponse pResponseStatus_ =
  DescribeUserImportJobResponse'
    {_duijrsUserImportJob = Nothing, _duijrsResponseStatus = pResponseStatus_}


-- | The job object that represents the user import job.
duijrsUserImportJob :: Lens' DescribeUserImportJobResponse (Maybe UserImportJobType)
duijrsUserImportJob = lens _duijrsUserImportJob (\ s a -> s{_duijrsUserImportJob = a})

-- | -- | The response status code.
duijrsResponseStatus :: Lens' DescribeUserImportJobResponse Int
duijrsResponseStatus = lens _duijrsResponseStatus (\ s a -> s{_duijrsResponseStatus = a})

instance NFData DescribeUserImportJobResponse where
