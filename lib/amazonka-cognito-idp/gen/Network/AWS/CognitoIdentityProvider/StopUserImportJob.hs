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
-- Module      : Network.AWS.CognitoIdentityProvider.StopUserImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the user import job.
--
--
module Network.AWS.CognitoIdentityProvider.StopUserImportJob
    (
    -- * Creating a Request
      stopUserImportJob
    , StopUserImportJob
    -- * Request Lenses
    , sUserPoolId
    , sJobId

    -- * Destructuring the Response
    , stopUserImportJobResponse
    , StopUserImportJobResponse
    -- * Response Lenses
    , srsUserImportJob
    , srsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to stop the user import job.
--
--
--
-- /See:/ 'stopUserImportJob' smart constructor.
data StopUserImportJob = StopUserImportJob'
  { _sUserPoolId :: !Text
  , _sJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopUserImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sUserPoolId' - The user pool ID for the user pool that the users are being imported into.
--
-- * 'sJobId' - The job ID for the user import job.
stopUserImportJob
    :: Text -- ^ 'sUserPoolId'
    -> Text -- ^ 'sJobId'
    -> StopUserImportJob
stopUserImportJob pUserPoolId_ pJobId_ =
  StopUserImportJob' {_sUserPoolId = pUserPoolId_, _sJobId = pJobId_}


-- | The user pool ID for the user pool that the users are being imported into.
sUserPoolId :: Lens' StopUserImportJob Text
sUserPoolId = lens _sUserPoolId (\ s a -> s{_sUserPoolId = a})

-- | The job ID for the user import job.
sJobId :: Lens' StopUserImportJob Text
sJobId = lens _sJobId (\ s a -> s{_sJobId = a})

instance AWSRequest StopUserImportJob where
        type Rs StopUserImportJob = StopUserImportJobResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 StopUserImportJobResponse' <$>
                   (x .?> "UserImportJob") <*> (pure (fromEnum s)))

instance Hashable StopUserImportJob where

instance NFData StopUserImportJob where

instance ToHeaders StopUserImportJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.StopUserImportJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopUserImportJob where
        toJSON StopUserImportJob'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _sUserPoolId),
                  Just ("JobId" .= _sJobId)])

instance ToPath StopUserImportJob where
        toPath = const "/"

instance ToQuery StopUserImportJob where
        toQuery = const mempty

-- | Represents the response from the server to the request to stop the user import job.
--
--
--
-- /See:/ 'stopUserImportJobResponse' smart constructor.
data StopUserImportJobResponse = StopUserImportJobResponse'
  { _srsUserImportJob  :: !(Maybe UserImportJobType)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopUserImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsUserImportJob' - The job object that represents the user import job.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopUserImportJobResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopUserImportJobResponse
stopUserImportJobResponse pResponseStatus_ =
  StopUserImportJobResponse'
    {_srsUserImportJob = Nothing, _srsResponseStatus = pResponseStatus_}


-- | The job object that represents the user import job.
srsUserImportJob :: Lens' StopUserImportJobResponse (Maybe UserImportJobType)
srsUserImportJob = lens _srsUserImportJob (\ s a -> s{_srsUserImportJob = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopUserImportJobResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopUserImportJobResponse where
