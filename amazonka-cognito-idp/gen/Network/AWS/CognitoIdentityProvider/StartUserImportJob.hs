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
-- Module      : Network.AWS.CognitoIdentityProvider.StartUserImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the user import.
--
--
module Network.AWS.CognitoIdentityProvider.StartUserImportJob
    (
    -- * Creating a Request
      startUserImportJob
    , StartUserImportJob
    -- * Request Lenses
    , suijUserPoolId
    , suijJobId

    -- * Destructuring the Response
    , startUserImportJobResponse
    , StartUserImportJobResponse
    -- * Response Lenses
    , suijrsUserImportJob
    , suijrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to start the user import job.
--
--
--
-- /See:/ 'startUserImportJob' smart constructor.
data StartUserImportJob = StartUserImportJob'
  { _suijUserPoolId :: !Text
  , _suijJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartUserImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'suijUserPoolId' - The user pool ID for the user pool that the users are being imported into.
--
-- * 'suijJobId' - The job ID for the user import job.
startUserImportJob
    :: Text -- ^ 'suijUserPoolId'
    -> Text -- ^ 'suijJobId'
    -> StartUserImportJob
startUserImportJob pUserPoolId_ pJobId_ =
  StartUserImportJob' {_suijUserPoolId = pUserPoolId_, _suijJobId = pJobId_}


-- | The user pool ID for the user pool that the users are being imported into.
suijUserPoolId :: Lens' StartUserImportJob Text
suijUserPoolId = lens _suijUserPoolId (\ s a -> s{_suijUserPoolId = a})

-- | The job ID for the user import job.
suijJobId :: Lens' StartUserImportJob Text
suijJobId = lens _suijJobId (\ s a -> s{_suijJobId = a})

instance AWSRequest StartUserImportJob where
        type Rs StartUserImportJob =
             StartUserImportJobResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 StartUserImportJobResponse' <$>
                   (x .?> "UserImportJob") <*> (pure (fromEnum s)))

instance Hashable StartUserImportJob where

instance NFData StartUserImportJob where

instance ToHeaders StartUserImportJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.StartUserImportJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartUserImportJob where
        toJSON StartUserImportJob'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _suijUserPoolId),
                  Just ("JobId" .= _suijJobId)])

instance ToPath StartUserImportJob where
        toPath = const "/"

instance ToQuery StartUserImportJob where
        toQuery = const mempty

-- | Represents the response from the server to the request to start the user import job.
--
--
--
-- /See:/ 'startUserImportJobResponse' smart constructor.
data StartUserImportJobResponse = StartUserImportJobResponse'
  { _suijrsUserImportJob  :: !(Maybe UserImportJobType)
  , _suijrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartUserImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'suijrsUserImportJob' - The job object that represents the user import job.
--
-- * 'suijrsResponseStatus' - -- | The response status code.
startUserImportJobResponse
    :: Int -- ^ 'suijrsResponseStatus'
    -> StartUserImportJobResponse
startUserImportJobResponse pResponseStatus_ =
  StartUserImportJobResponse'
    {_suijrsUserImportJob = Nothing, _suijrsResponseStatus = pResponseStatus_}


-- | The job object that represents the user import job.
suijrsUserImportJob :: Lens' StartUserImportJobResponse (Maybe UserImportJobType)
suijrsUserImportJob = lens _suijrsUserImportJob (\ s a -> s{_suijrsUserImportJob = a})

-- | -- | The response status code.
suijrsResponseStatus :: Lens' StartUserImportJobResponse Int
suijrsResponseStatus = lens _suijrsResponseStatus (\ s a -> s{_suijrsResponseStatus = a})

instance NFData StartUserImportJobResponse where
