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
-- Module      : Network.AWS.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified DevEndpoint.
--
--
module Network.AWS.Glue.UpdateDevEndpoint
    (
    -- * Creating a Request
      updateDevEndpoint
    , UpdateDevEndpoint
    -- * Request Lenses
    , udeCustomLibraries
    , udePublicKey
    , udeUpdateEtlLibraries
    , udeEndpointName

    -- * Destructuring the Response
    , updateDevEndpointResponse
    , UpdateDevEndpointResponse
    -- * Response Lenses
    , udersResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { _udeCustomLibraries    :: !(Maybe DevEndpointCustomLibraries)
  , _udePublicKey          :: !(Maybe Text)
  , _udeUpdateEtlLibraries :: !(Maybe Bool)
  , _udeEndpointName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udeCustomLibraries' - Custom Python or Java libraries to be loaded in the DevEndpoint.
--
-- * 'udePublicKey' - The public key for the DevEndpoint to use.
--
-- * 'udeUpdateEtlLibraries' - True if the list of custom libraries to be loaded in the development endpoint needs to be updated, or False otherwise.
--
-- * 'udeEndpointName' - The name of the DevEndpoint to be updated.
updateDevEndpoint
    :: Text -- ^ 'udeEndpointName'
    -> UpdateDevEndpoint
updateDevEndpoint pEndpointName_ =
  UpdateDevEndpoint'
    { _udeCustomLibraries = Nothing
    , _udePublicKey = Nothing
    , _udeUpdateEtlLibraries = Nothing
    , _udeEndpointName = pEndpointName_
    }


-- | Custom Python or Java libraries to be loaded in the DevEndpoint.
udeCustomLibraries :: Lens' UpdateDevEndpoint (Maybe DevEndpointCustomLibraries)
udeCustomLibraries = lens _udeCustomLibraries (\ s a -> s{_udeCustomLibraries = a})

-- | The public key for the DevEndpoint to use.
udePublicKey :: Lens' UpdateDevEndpoint (Maybe Text)
udePublicKey = lens _udePublicKey (\ s a -> s{_udePublicKey = a})

-- | True if the list of custom libraries to be loaded in the development endpoint needs to be updated, or False otherwise.
udeUpdateEtlLibraries :: Lens' UpdateDevEndpoint (Maybe Bool)
udeUpdateEtlLibraries = lens _udeUpdateEtlLibraries (\ s a -> s{_udeUpdateEtlLibraries = a})

-- | The name of the DevEndpoint to be updated.
udeEndpointName :: Lens' UpdateDevEndpoint Text
udeEndpointName = lens _udeEndpointName (\ s a -> s{_udeEndpointName = a})

instance AWSRequest UpdateDevEndpoint where
        type Rs UpdateDevEndpoint = UpdateDevEndpointResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDevEndpointResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDevEndpoint where

instance NFData UpdateDevEndpoint where

instance ToHeaders UpdateDevEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateDevEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDevEndpoint where
        toJSON UpdateDevEndpoint'{..}
          = object
              (catMaybes
                 [("CustomLibraries" .=) <$> _udeCustomLibraries,
                  ("PublicKey" .=) <$> _udePublicKey,
                  ("UpdateEtlLibraries" .=) <$> _udeUpdateEtlLibraries,
                  Just ("EndpointName" .= _udeEndpointName)])

instance ToPath UpdateDevEndpoint where
        toPath = const "/"

instance ToQuery UpdateDevEndpoint where
        toQuery = const mempty

-- | /See:/ 'updateDevEndpointResponse' smart constructor.
newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { _udersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udersResponseStatus' - -- | The response status code.
updateDevEndpointResponse
    :: Int -- ^ 'udersResponseStatus'
    -> UpdateDevEndpointResponse
updateDevEndpointResponse pResponseStatus_ =
  UpdateDevEndpointResponse' {_udersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udersResponseStatus :: Lens' UpdateDevEndpointResponse Int
udersResponseStatus = lens _udersResponseStatus (\ s a -> s{_udersResponseStatus = a})

instance NFData UpdateDevEndpointResponse where
