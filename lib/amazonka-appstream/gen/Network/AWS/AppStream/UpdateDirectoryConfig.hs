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
-- Module      : Network.AWS.AppStream.UpdateDirectoryConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified directory configuration.
--
--
module Network.AWS.AppStream.UpdateDirectoryConfig
    (
    -- * Creating a Request
      updateDirectoryConfig
    , UpdateDirectoryConfig
    -- * Request Lenses
    , udcServiceAccountCredentials
    , udcOrganizationalUnitDistinguishedNames
    , udcDirectoryName

    -- * Destructuring the Response
    , updateDirectoryConfigResponse
    , UpdateDirectoryConfigResponse
    -- * Response Lenses
    , udcrsDirectoryConfig
    , udcrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDirectoryConfig' smart constructor.
data UpdateDirectoryConfig = UpdateDirectoryConfig'
  { _udcServiceAccountCredentials :: !(Maybe ServiceAccountCredentials)
  , _udcOrganizationalUnitDistinguishedNames :: !(Maybe [Text])
  , _udcDirectoryName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcServiceAccountCredentials' - The credentials for the service account used by the streaming instance to connect to the directory.
--
-- * 'udcOrganizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
--
-- * 'udcDirectoryName' - The name of the directory configuration.
updateDirectoryConfig
    :: Text -- ^ 'udcDirectoryName'
    -> UpdateDirectoryConfig
updateDirectoryConfig pDirectoryName_ =
  UpdateDirectoryConfig'
    { _udcServiceAccountCredentials = Nothing
    , _udcOrganizationalUnitDistinguishedNames = Nothing
    , _udcDirectoryName = pDirectoryName_
    }


-- | The credentials for the service account used by the streaming instance to connect to the directory.
udcServiceAccountCredentials :: Lens' UpdateDirectoryConfig (Maybe ServiceAccountCredentials)
udcServiceAccountCredentials = lens _udcServiceAccountCredentials (\ s a -> s{_udcServiceAccountCredentials = a})

-- | The distinguished names of the organizational units for computer accounts.
udcOrganizationalUnitDistinguishedNames :: Lens' UpdateDirectoryConfig [Text]
udcOrganizationalUnitDistinguishedNames = lens _udcOrganizationalUnitDistinguishedNames (\ s a -> s{_udcOrganizationalUnitDistinguishedNames = a}) . _Default . _Coerce

-- | The name of the directory configuration.
udcDirectoryName :: Lens' UpdateDirectoryConfig Text
udcDirectoryName = lens _udcDirectoryName (\ s a -> s{_udcDirectoryName = a})

instance AWSRequest UpdateDirectoryConfig where
        type Rs UpdateDirectoryConfig =
             UpdateDirectoryConfigResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDirectoryConfigResponse' <$>
                   (x .?> "DirectoryConfig") <*> (pure (fromEnum s)))

instance Hashable UpdateDirectoryConfig where

instance NFData UpdateDirectoryConfig where

instance ToHeaders UpdateDirectoryConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.UpdateDirectoryConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDirectoryConfig where
        toJSON UpdateDirectoryConfig'{..}
          = object
              (catMaybes
                 [("ServiceAccountCredentials" .=) <$>
                    _udcServiceAccountCredentials,
                  ("OrganizationalUnitDistinguishedNames" .=) <$>
                    _udcOrganizationalUnitDistinguishedNames,
                  Just ("DirectoryName" .= _udcDirectoryName)])

instance ToPath UpdateDirectoryConfig where
        toPath = const "/"

instance ToQuery UpdateDirectoryConfig where
        toQuery = const mempty

-- | /See:/ 'updateDirectoryConfigResponse' smart constructor.
data UpdateDirectoryConfigResponse = UpdateDirectoryConfigResponse'
  { _udcrsDirectoryConfig :: !(Maybe DirectoryConfig)
  , _udcrsResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcrsDirectoryConfig' - Information about the directory configuration.
--
-- * 'udcrsResponseStatus' - -- | The response status code.
updateDirectoryConfigResponse
    :: Int -- ^ 'udcrsResponseStatus'
    -> UpdateDirectoryConfigResponse
updateDirectoryConfigResponse pResponseStatus_ =
  UpdateDirectoryConfigResponse'
    {_udcrsDirectoryConfig = Nothing, _udcrsResponseStatus = pResponseStatus_}


-- | Information about the directory configuration.
udcrsDirectoryConfig :: Lens' UpdateDirectoryConfigResponse (Maybe DirectoryConfig)
udcrsDirectoryConfig = lens _udcrsDirectoryConfig (\ s a -> s{_udcrsDirectoryConfig = a})

-- | -- | The response status code.
udcrsResponseStatus :: Lens' UpdateDirectoryConfigResponse Int
udcrsResponseStatus = lens _udcrsResponseStatus (\ s a -> s{_udcrsResponseStatus = a})

instance NFData UpdateDirectoryConfigResponse where
