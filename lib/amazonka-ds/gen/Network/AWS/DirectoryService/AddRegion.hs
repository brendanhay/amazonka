{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds two domain controllers in the specified Region for the specified directory.
module Network.AWS.DirectoryService.AddRegion
  ( -- * Creating a Request
    addRegion,
    AddRegion,

    -- * Request Lenses
    arDirectoryId,
    arRegionName,
    arVPCSettings,

    -- * Destructuring the Response
    addRegionResponse,
    AddRegionResponse,

    -- * Response Lenses
    arrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addRegion' smart constructor.
data AddRegion = AddRegion'
  { _arDirectoryId :: !Text,
    _arRegionName :: !Text,
    _arVPCSettings :: !DirectoryVPCSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddRegion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arDirectoryId' - The identifier of the directory to which you want to add Region replication.
--
-- * 'arRegionName' - The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
--
-- * 'arVPCSettings' - Undocumented member.
addRegion ::
  -- | 'arDirectoryId'
  Text ->
  -- | 'arRegionName'
  Text ->
  -- | 'arVPCSettings'
  DirectoryVPCSettings ->
  AddRegion
addRegion pDirectoryId_ pRegionName_ pVPCSettings_ =
  AddRegion'
    { _arDirectoryId = pDirectoryId_,
      _arRegionName = pRegionName_,
      _arVPCSettings = pVPCSettings_
    }

-- | The identifier of the directory to which you want to add Region replication.
arDirectoryId :: Lens' AddRegion Text
arDirectoryId = lens _arDirectoryId (\s a -> s {_arDirectoryId = a})

-- | The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
arRegionName :: Lens' AddRegion Text
arRegionName = lens _arRegionName (\s a -> s {_arRegionName = a})

-- | Undocumented member.
arVPCSettings :: Lens' AddRegion DirectoryVPCSettings
arVPCSettings = lens _arVPCSettings (\s a -> s {_arVPCSettings = a})

instance AWSRequest AddRegion where
  type Rs AddRegion = AddRegionResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> AddRegionResponse' <$> (pure (fromEnum s)))

instance Hashable AddRegion

instance NFData AddRegion

instance ToHeaders AddRegion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.AddRegion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AddRegion where
  toJSON AddRegion' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _arDirectoryId),
            Just ("RegionName" .= _arRegionName),
            Just ("VPCSettings" .= _arVPCSettings)
          ]
      )

instance ToPath AddRegion where
  toPath = const "/"

instance ToQuery AddRegion where
  toQuery = const mempty

-- | /See:/ 'addRegionResponse' smart constructor.
newtype AddRegionResponse = AddRegionResponse'
  { _arrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddRegionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arrsResponseStatus' - -- | The response status code.
addRegionResponse ::
  -- | 'arrsResponseStatus'
  Int ->
  AddRegionResponse
addRegionResponse pResponseStatus_ =
  AddRegionResponse' {_arrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
arrsResponseStatus :: Lens' AddRegionResponse Int
arrsResponseStatus = lens _arrsResponseStatus (\s a -> s {_arrsResponseStatus = a})

instance NFData AddRegionResponse
