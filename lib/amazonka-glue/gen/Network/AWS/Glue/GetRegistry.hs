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
-- Module      : Network.AWS.Glue.GetRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified registry in detail.
module Network.AWS.Glue.GetRegistry
  ( -- * Creating a Request
    getRegistry,
    GetRegistry,

    -- * Request Lenses
    grRegistryId,

    -- * Destructuring the Response
    getRegistryResponse,
    GetRegistryResponse,

    -- * Response Lenses
    grrsStatus,
    grrsRegistryName,
    grrsCreatedTime,
    grrsRegistryARN,
    grrsUpdatedTime,
    grrsDescription,
    grrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRegistry' smart constructor.
newtype GetRegistry = GetRegistry' {_grRegistryId :: RegistryId}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRegistry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRegistryId' - This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
getRegistry ::
  -- | 'grRegistryId'
  RegistryId ->
  GetRegistry
getRegistry pRegistryId_ =
  GetRegistry' {_grRegistryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
grRegistryId :: Lens' GetRegistry RegistryId
grRegistryId = lens _grRegistryId (\s a -> s {_grRegistryId = a})

instance AWSRequest GetRegistry where
  type Rs GetRegistry = GetRegistryResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetRegistryResponse'
            <$> (x .?> "Status")
            <*> (x .?> "RegistryName")
            <*> (x .?> "CreatedTime")
            <*> (x .?> "RegistryArn")
            <*> (x .?> "UpdatedTime")
            <*> (x .?> "Description")
            <*> (pure (fromEnum s))
      )

instance Hashable GetRegistry

instance NFData GetRegistry

instance ToHeaders GetRegistry where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetRegistry" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetRegistry where
  toJSON GetRegistry' {..} =
    object (catMaybes [Just ("RegistryId" .= _grRegistryId)])

instance ToPath GetRegistry where
  toPath = const "/"

instance ToQuery GetRegistry where
  toQuery = const mempty

-- | /See:/ 'getRegistryResponse' smart constructor.
data GetRegistryResponse = GetRegistryResponse'
  { _grrsStatus ::
      !(Maybe RegistryStatus),
    _grrsRegistryName :: !(Maybe Text),
    _grrsCreatedTime :: !(Maybe Text),
    _grrsRegistryARN :: !(Maybe Text),
    _grrsUpdatedTime :: !(Maybe Text),
    _grrsDescription :: !(Maybe Text),
    _grrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRegistryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsStatus' - The status of the registry.
--
-- * 'grrsRegistryName' - The name of the registry.
--
-- * 'grrsCreatedTime' - The date and time the registry was created.
--
-- * 'grrsRegistryARN' - The Amazon Resource Name (ARN) of the registry.
--
-- * 'grrsUpdatedTime' - The date and time the registry was updated.
--
-- * 'grrsDescription' - A description of the registry.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRegistryResponse ::
  -- | 'grrsResponseStatus'
  Int ->
  GetRegistryResponse
getRegistryResponse pResponseStatus_ =
  GetRegistryResponse'
    { _grrsStatus = Nothing,
      _grrsRegistryName = Nothing,
      _grrsCreatedTime = Nothing,
      _grrsRegistryARN = Nothing,
      _grrsUpdatedTime = Nothing,
      _grrsDescription = Nothing,
      _grrsResponseStatus = pResponseStatus_
    }

-- | The status of the registry.
grrsStatus :: Lens' GetRegistryResponse (Maybe RegistryStatus)
grrsStatus = lens _grrsStatus (\s a -> s {_grrsStatus = a})

-- | The name of the registry.
grrsRegistryName :: Lens' GetRegistryResponse (Maybe Text)
grrsRegistryName = lens _grrsRegistryName (\s a -> s {_grrsRegistryName = a})

-- | The date and time the registry was created.
grrsCreatedTime :: Lens' GetRegistryResponse (Maybe Text)
grrsCreatedTime = lens _grrsCreatedTime (\s a -> s {_grrsCreatedTime = a})

-- | The Amazon Resource Name (ARN) of the registry.
grrsRegistryARN :: Lens' GetRegistryResponse (Maybe Text)
grrsRegistryARN = lens _grrsRegistryARN (\s a -> s {_grrsRegistryARN = a})

-- | The date and time the registry was updated.
grrsUpdatedTime :: Lens' GetRegistryResponse (Maybe Text)
grrsUpdatedTime = lens _grrsUpdatedTime (\s a -> s {_grrsUpdatedTime = a})

-- | A description of the registry.
grrsDescription :: Lens' GetRegistryResponse (Maybe Text)
grrsDescription = lens _grrsDescription (\s a -> s {_grrsDescription = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRegistryResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\s a -> s {_grrsResponseStatus = a})

instance NFData GetRegistryResponse
