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
-- Module      : Network.AWS.SMS.UpdateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.SMS.UpdateApp
  ( -- * Creating a Request
    updateApp,
    UpdateApp,

    -- * Request Lenses
    uaRoleName,
    uaAppId,
    uaName,
    uaDescription,
    uaServerGroups,
    uaTags,

    -- * Destructuring the Response
    updateAppResponse,
    UpdateAppResponse,

    -- * Response Lenses
    uarsAppSummary,
    uarsServerGroups,
    uarsTags,
    uarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'updateApp' smart constructor.
data UpdateApp = UpdateApp'
  { _uaRoleName :: !(Maybe Text),
    _uaAppId :: !(Maybe Text),
    _uaName :: !(Maybe Text),
    _uaDescription :: !(Maybe Text),
    _uaServerGroups :: !(Maybe [ServerGroup]),
    _uaTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaRoleName' - The name of the service role in the customer's account used by AWS SMS.
--
-- * 'uaAppId' - The ID of the application.
--
-- * 'uaName' - The new name of the application.
--
-- * 'uaDescription' - The new description of the application.
--
-- * 'uaServerGroups' - The server groups in the application to update.
--
-- * 'uaTags' - The tags to associate with the application.
updateApp ::
  UpdateApp
updateApp =
  UpdateApp'
    { _uaRoleName = Nothing,
      _uaAppId = Nothing,
      _uaName = Nothing,
      _uaDescription = Nothing,
      _uaServerGroups = Nothing,
      _uaTags = Nothing
    }

-- | The name of the service role in the customer's account used by AWS SMS.
uaRoleName :: Lens' UpdateApp (Maybe Text)
uaRoleName = lens _uaRoleName (\s a -> s {_uaRoleName = a})

-- | The ID of the application.
uaAppId :: Lens' UpdateApp (Maybe Text)
uaAppId = lens _uaAppId (\s a -> s {_uaAppId = a})

-- | The new name of the application.
uaName :: Lens' UpdateApp (Maybe Text)
uaName = lens _uaName (\s a -> s {_uaName = a})

-- | The new description of the application.
uaDescription :: Lens' UpdateApp (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s {_uaDescription = a})

-- | The server groups in the application to update.
uaServerGroups :: Lens' UpdateApp [ServerGroup]
uaServerGroups = lens _uaServerGroups (\s a -> s {_uaServerGroups = a}) . _Default . _Coerce

-- | The tags to associate with the application.
uaTags :: Lens' UpdateApp [Tag]
uaTags = lens _uaTags (\s a -> s {_uaTags = a}) . _Default . _Coerce

instance AWSRequest UpdateApp where
  type Rs UpdateApp = UpdateAppResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            <$> (x .?> "appSummary")
            <*> (x .?> "serverGroups" .!@ mempty)
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateApp

instance NFData UpdateApp

instance ToHeaders UpdateApp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSServerMigrationService_V2016_10_24.UpdateApp" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    object
      ( catMaybes
          [ ("roleName" .=) <$> _uaRoleName,
            ("appId" .=) <$> _uaAppId,
            ("name" .=) <$> _uaName,
            ("description" .=) <$> _uaDescription,
            ("serverGroups" .=) <$> _uaServerGroups,
            ("tags" .=) <$> _uaTags
          ]
      )

instance ToPath UpdateApp where
  toPath = const "/"

instance ToQuery UpdateApp where
  toQuery = const mempty

-- | /See:/ 'updateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { _uarsAppSummary ::
      !(Maybe AppSummary),
    _uarsServerGroups :: !(Maybe [ServerGroup]),
    _uarsTags :: !(Maybe [Tag]),
    _uarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAppSummary' - A summary description of the application.
--
-- * 'uarsServerGroups' - The updated server groups in the application.
--
-- * 'uarsTags' - The tags associated with the application.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAppResponse ::
  -- | 'uarsResponseStatus'
  Int ->
  UpdateAppResponse
updateAppResponse pResponseStatus_ =
  UpdateAppResponse'
    { _uarsAppSummary = Nothing,
      _uarsServerGroups = Nothing,
      _uarsTags = Nothing,
      _uarsResponseStatus = pResponseStatus_
    }

-- | A summary description of the application.
uarsAppSummary :: Lens' UpdateAppResponse (Maybe AppSummary)
uarsAppSummary = lens _uarsAppSummary (\s a -> s {_uarsAppSummary = a})

-- | The updated server groups in the application.
uarsServerGroups :: Lens' UpdateAppResponse [ServerGroup]
uarsServerGroups = lens _uarsServerGroups (\s a -> s {_uarsServerGroups = a}) . _Default . _Coerce

-- | The tags associated with the application.
uarsTags :: Lens' UpdateAppResponse [Tag]
uarsTags = lens _uarsTags (\s a -> s {_uarsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAppResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\s a -> s {_uarsResponseStatus = a})

instance NFData UpdateAppResponse
