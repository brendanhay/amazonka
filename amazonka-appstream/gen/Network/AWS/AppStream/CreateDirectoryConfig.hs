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
-- Module      : Network.AWS.AppStream.CreateDirectoryConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a directory configuration.
--
--
module Network.AWS.AppStream.CreateDirectoryConfig
    (
    -- * Creating a Request
      createDirectoryConfig
    , CreateDirectoryConfig
    -- * Request Lenses
    , cdcDirectoryName
    , cdcOrganizationalUnitDistinguishedNames
    , cdcServiceAccountCredentials

    -- * Destructuring the Response
    , createDirectoryConfigResponse
    , CreateDirectoryConfigResponse
    -- * Response Lenses
    , cdcrsDirectoryConfig
    , cdcrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
  { _cdcDirectoryName                        :: !Text
  , _cdcOrganizationalUnitDistinguishedNames :: ![Text]
  , _cdcServiceAccountCredentials            :: !ServiceAccountCredentials
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
--
-- * 'cdcOrganizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
--
-- * 'cdcServiceAccountCredentials' - The credentials for the service account used by the streaming instance to connect to the directory.
createDirectoryConfig
    :: Text -- ^ 'cdcDirectoryName'
    -> ServiceAccountCredentials -- ^ 'cdcServiceAccountCredentials'
    -> CreateDirectoryConfig
createDirectoryConfig pDirectoryName_ pServiceAccountCredentials_ =
  CreateDirectoryConfig'
    { _cdcDirectoryName = pDirectoryName_
    , _cdcOrganizationalUnitDistinguishedNames = mempty
    , _cdcServiceAccountCredentials = pServiceAccountCredentials_
    }


-- | The fully qualified name of the directory (for example, corp.example.com).
cdcDirectoryName :: Lens' CreateDirectoryConfig Text
cdcDirectoryName = lens _cdcDirectoryName (\ s a -> s{_cdcDirectoryName = a})

-- | The distinguished names of the organizational units for computer accounts.
cdcOrganizationalUnitDistinguishedNames :: Lens' CreateDirectoryConfig [Text]
cdcOrganizationalUnitDistinguishedNames = lens _cdcOrganizationalUnitDistinguishedNames (\ s a -> s{_cdcOrganizationalUnitDistinguishedNames = a}) . _Coerce

-- | The credentials for the service account used by the streaming instance to connect to the directory.
cdcServiceAccountCredentials :: Lens' CreateDirectoryConfig ServiceAccountCredentials
cdcServiceAccountCredentials = lens _cdcServiceAccountCredentials (\ s a -> s{_cdcServiceAccountCredentials = a})

instance AWSRequest CreateDirectoryConfig where
        type Rs CreateDirectoryConfig =
             CreateDirectoryConfigResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectoryConfigResponse' <$>
                   (x .?> "DirectoryConfig") <*> (pure (fromEnum s)))

instance Hashable CreateDirectoryConfig where

instance NFData CreateDirectoryConfig where

instance ToHeaders CreateDirectoryConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateDirectoryConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDirectoryConfig where
        toJSON CreateDirectoryConfig'{..}
          = object
              (catMaybes
                 [Just ("DirectoryName" .= _cdcDirectoryName),
                  Just
                    ("OrganizationalUnitDistinguishedNames" .=
                       _cdcOrganizationalUnitDistinguishedNames),
                  Just
                    ("ServiceAccountCredentials" .=
                       _cdcServiceAccountCredentials)])

instance ToPath CreateDirectoryConfig where
        toPath = const "/"

instance ToQuery CreateDirectoryConfig where
        toQuery = const mempty

-- | /See:/ 'createDirectoryConfigResponse' smart constructor.
data CreateDirectoryConfigResponse = CreateDirectoryConfigResponse'
  { _cdcrsDirectoryConfig :: !(Maybe DirectoryConfig)
  , _cdcrsResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDirectoryConfig' - Information about the directory configuration.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDirectoryConfigResponse
    :: Int -- ^ 'cdcrsResponseStatus'
    -> CreateDirectoryConfigResponse
createDirectoryConfigResponse pResponseStatus_ =
  CreateDirectoryConfigResponse'
    {_cdcrsDirectoryConfig = Nothing, _cdcrsResponseStatus = pResponseStatus_}


-- | Information about the directory configuration.
cdcrsDirectoryConfig :: Lens' CreateDirectoryConfigResponse (Maybe DirectoryConfig)
cdcrsDirectoryConfig = lens _cdcrsDirectoryConfig (\ s a -> s{_cdcrsDirectoryConfig = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDirectoryConfigResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\ s a -> s{_cdcrsResponseStatus = a})

instance NFData CreateDirectoryConfigResponse where
