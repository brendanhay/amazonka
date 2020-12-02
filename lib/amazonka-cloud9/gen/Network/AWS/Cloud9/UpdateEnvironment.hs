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
-- Module      : Network.AWS.Cloud9.UpdateEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing AWS Cloud9 development environment.
--
--
module Network.AWS.Cloud9.UpdateEnvironment
    (
    -- * Creating a Request
      updateEnvironment
    , UpdateEnvironment
    -- * Request Lenses
    , ueName
    , ueDescription
    , ueEnvironmentId

    -- * Destructuring the Response
    , updateEnvironmentResponse
    , UpdateEnvironmentResponse
    -- * Response Lenses
    , uersResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { _ueName          :: !(Maybe Text)
  , _ueDescription   :: !(Maybe Text)
  , _ueEnvironmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueName' - A replacement name for the environment.
--
-- * 'ueDescription' - Any new or replacement description for the environment.
--
-- * 'ueEnvironmentId' - The ID of the environment to change settings.
updateEnvironment
    :: Text -- ^ 'ueEnvironmentId'
    -> UpdateEnvironment
updateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { _ueName = Nothing
    , _ueDescription = Nothing
    , _ueEnvironmentId = pEnvironmentId_
    }


-- | A replacement name for the environment.
ueName :: Lens' UpdateEnvironment (Maybe Text)
ueName = lens _ueName (\ s a -> s{_ueName = a})

-- | Any new or replacement description for the environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\ s a -> s{_ueDescription = a})

-- | The ID of the environment to change settings.
ueEnvironmentId :: Lens' UpdateEnvironment Text
ueEnvironmentId = lens _ueEnvironmentId (\ s a -> s{_ueEnvironmentId = a})

instance AWSRequest UpdateEnvironment where
        type Rs UpdateEnvironment = UpdateEnvironmentResponse
        request = postJSON cloud9
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateEnvironmentResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateEnvironment where

instance NFData UpdateEnvironment where

instance ToHeaders UpdateEnvironment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.UpdateEnvironment"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEnvironment where
        toJSON UpdateEnvironment'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _ueName,
                  ("description" .=) <$> _ueDescription,
                  Just ("environmentId" .= _ueEnvironmentId)])

instance ToPath UpdateEnvironment where
        toPath = const "/"

instance ToQuery UpdateEnvironment where
        toQuery = const mempty

-- | /See:/ 'updateEnvironmentResponse' smart constructor.
newtype UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { _uersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEnvironmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uersResponseStatus' - -- | The response status code.
updateEnvironmentResponse
    :: Int -- ^ 'uersResponseStatus'
    -> UpdateEnvironmentResponse
updateEnvironmentResponse pResponseStatus_ =
  UpdateEnvironmentResponse' {_uersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uersResponseStatus :: Lens' UpdateEnvironmentResponse Int
uersResponseStatus = lens _uersResponseStatus (\ s a -> s{_uersResponseStatus = a})

instance NFData UpdateEnvironmentResponse where
