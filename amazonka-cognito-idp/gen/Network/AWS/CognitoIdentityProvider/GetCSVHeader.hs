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
-- Module      : Network.AWS.CognitoIdentityProvider.GetCSVHeader
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the header information for the .csv file to be used as input for the user import job.
--
--
module Network.AWS.CognitoIdentityProvider.GetCSVHeader
    (
    -- * Creating a Request
      getCSVHeader
    , GetCSVHeader
    -- * Request Lenses
    , gchUserPoolId

    -- * Destructuring the Response
    , getCSVHeaderResponse
    , GetCSVHeaderResponse
    -- * Response Lenses
    , gchrsUserPoolId
    , gchrsCSVHeader
    , gchrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get the header information for the .csv file for the user import job.
--
--
--
-- /See:/ 'getCSVHeader' smart constructor.
newtype GetCSVHeader = GetCSVHeader'
  { _gchUserPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCSVHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gchUserPoolId' - The user pool ID for the user pool that the users are to be imported into.
getCSVHeader
    :: Text -- ^ 'gchUserPoolId'
    -> GetCSVHeader
getCSVHeader pUserPoolId_ = GetCSVHeader' {_gchUserPoolId = pUserPoolId_}


-- | The user pool ID for the user pool that the users are to be imported into.
gchUserPoolId :: Lens' GetCSVHeader Text
gchUserPoolId = lens _gchUserPoolId (\ s a -> s{_gchUserPoolId = a})

instance AWSRequest GetCSVHeader where
        type Rs GetCSVHeader = GetCSVHeaderResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetCSVHeaderResponse' <$>
                   (x .?> "UserPoolId") <*>
                     (x .?> "CSVHeader" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetCSVHeader where

instance NFData GetCSVHeader where

instance ToHeaders GetCSVHeader where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetCSVHeader" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCSVHeader where
        toJSON GetCSVHeader'{..}
          = object
              (catMaybes [Just ("UserPoolId" .= _gchUserPoolId)])

instance ToPath GetCSVHeader where
        toPath = const "/"

instance ToQuery GetCSVHeader where
        toQuery = const mempty

-- | Represents the response from the server to the request to get the header information for the .csv file for the user import job.
--
--
--
-- /See:/ 'getCSVHeaderResponse' smart constructor.
data GetCSVHeaderResponse = GetCSVHeaderResponse'
  { _gchrsUserPoolId     :: !(Maybe Text)
  , _gchrsCSVHeader      :: !(Maybe [Text])
  , _gchrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCSVHeaderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gchrsUserPoolId' - The user pool ID for the user pool that the users are to be imported into.
--
-- * 'gchrsCSVHeader' - The header information for the .csv file for the user import job.
--
-- * 'gchrsResponseStatus' - -- | The response status code.
getCSVHeaderResponse
    :: Int -- ^ 'gchrsResponseStatus'
    -> GetCSVHeaderResponse
getCSVHeaderResponse pResponseStatus_ =
  GetCSVHeaderResponse'
    { _gchrsUserPoolId = Nothing
    , _gchrsCSVHeader = Nothing
    , _gchrsResponseStatus = pResponseStatus_
    }


-- | The user pool ID for the user pool that the users are to be imported into.
gchrsUserPoolId :: Lens' GetCSVHeaderResponse (Maybe Text)
gchrsUserPoolId = lens _gchrsUserPoolId (\ s a -> s{_gchrsUserPoolId = a})

-- | The header information for the .csv file for the user import job.
gchrsCSVHeader :: Lens' GetCSVHeaderResponse [Text]
gchrsCSVHeader = lens _gchrsCSVHeader (\ s a -> s{_gchrsCSVHeader = a}) . _Default . _Coerce

-- | -- | The response status code.
gchrsResponseStatus :: Lens' GetCSVHeaderResponse Int
gchrsResponseStatus = lens _gchrsResponseStatus (\ s a -> s{_gchrsResponseStatus = a})

instance NFData GetCSVHeaderResponse where
