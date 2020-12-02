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
-- Module      : Network.AWS.AppStream.UpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fields for the specified stack.
module Network.AWS.AppStream.UpdateStack
  ( -- * Creating a Request
    updateStack,
    UpdateStack,

    -- * Request Lenses
    usUserSettings,
    usApplicationSettings,
    usFeedbackURL,
    usAttributesToDelete,
    usDeleteStorageConnectors,
    usStorageConnectors,
    usAccessEndpoints,
    usDisplayName,
    usEmbedHostDomains,
    usDescription,
    usRedirectURL,
    usName,

    -- * Destructuring the Response
    updateStackResponse,
    UpdateStackResponse,

    -- * Response Lenses
    usrsStack,
    usrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStack' smart constructor.
data UpdateStack = UpdateStack'
  { _usUserSettings ::
      !(Maybe (List1 UserSetting)),
    _usApplicationSettings :: !(Maybe ApplicationSettings),
    _usFeedbackURL :: !(Maybe Text),
    _usAttributesToDelete :: !(Maybe [StackAttribute]),
    _usDeleteStorageConnectors :: !(Maybe Bool),
    _usStorageConnectors :: !(Maybe [StorageConnector]),
    _usAccessEndpoints :: !(Maybe (List1 AccessEndpoint)),
    _usDisplayName :: !(Maybe Text),
    _usEmbedHostDomains :: !(Maybe (List1 Text)),
    _usDescription :: !(Maybe Text),
    _usRedirectURL :: !(Maybe Text),
    _usName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usUserSettings' - The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
--
-- * 'usApplicationSettings' - The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
--
-- * 'usFeedbackURL' - The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- * 'usAttributesToDelete' - The stack attributes to delete.
--
-- * 'usDeleteStorageConnectors' - Deletes the storage connectors currently enabled for the stack.
--
-- * 'usStorageConnectors' - The storage connectors to enable.
--
-- * 'usAccessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
--
-- * 'usDisplayName' - The stack name to display.
--
-- * 'usEmbedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
--
-- * 'usDescription' - The description to display.
--
-- * 'usRedirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- * 'usName' - The name of the stack.
updateStack ::
  -- | 'usName'
  Text ->
  UpdateStack
updateStack pName_ =
  UpdateStack'
    { _usUserSettings = Nothing,
      _usApplicationSettings = Nothing,
      _usFeedbackURL = Nothing,
      _usAttributesToDelete = Nothing,
      _usDeleteStorageConnectors = Nothing,
      _usStorageConnectors = Nothing,
      _usAccessEndpoints = Nothing,
      _usDisplayName = Nothing,
      _usEmbedHostDomains = Nothing,
      _usDescription = Nothing,
      _usRedirectURL = Nothing,
      _usName = pName_
    }

-- | The actions that are enabled or disabled for users during their streaming sessions. By default, these actions are enabled.
usUserSettings :: Lens' UpdateStack (Maybe (NonEmpty UserSetting))
usUserSettings = lens _usUserSettings (\s a -> s {_usUserSettings = a}) . mapping _List1

-- | The persistent application settings for users of a stack. When these settings are enabled, changes that users make to applications and Windows settings are automatically saved after each session and applied to the next session.
usApplicationSettings :: Lens' UpdateStack (Maybe ApplicationSettings)
usApplicationSettings = lens _usApplicationSettings (\s a -> s {_usApplicationSettings = a})

-- | The URL that users are redirected to after they choose the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
usFeedbackURL :: Lens' UpdateStack (Maybe Text)
usFeedbackURL = lens _usFeedbackURL (\s a -> s {_usFeedbackURL = a})

-- | The stack attributes to delete.
usAttributesToDelete :: Lens' UpdateStack [StackAttribute]
usAttributesToDelete = lens _usAttributesToDelete (\s a -> s {_usAttributesToDelete = a}) . _Default . _Coerce

-- | Deletes the storage connectors currently enabled for the stack.
usDeleteStorageConnectors :: Lens' UpdateStack (Maybe Bool)
usDeleteStorageConnectors = lens _usDeleteStorageConnectors (\s a -> s {_usDeleteStorageConnectors = a})

-- | The storage connectors to enable.
usStorageConnectors :: Lens' UpdateStack [StorageConnector]
usStorageConnectors = lens _usStorageConnectors (\s a -> s {_usStorageConnectors = a}) . _Default . _Coerce

-- | The list of interface VPC endpoint (interface endpoint) objects. Users of the stack can connect to AppStream 2.0 only through the specified endpoints.
usAccessEndpoints :: Lens' UpdateStack (Maybe (NonEmpty AccessEndpoint))
usAccessEndpoints = lens _usAccessEndpoints (\s a -> s {_usAccessEndpoints = a}) . mapping _List1

-- | The stack name to display.
usDisplayName :: Lens' UpdateStack (Maybe Text)
usDisplayName = lens _usDisplayName (\s a -> s {_usDisplayName = a})

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an iframe. You must approve the domains that you want to host embedded AppStream 2.0 streaming sessions.
usEmbedHostDomains :: Lens' UpdateStack (Maybe (NonEmpty Text))
usEmbedHostDomains = lens _usEmbedHostDomains (\s a -> s {_usEmbedHostDomains = a}) . mapping _List1

-- | The description to display.
usDescription :: Lens' UpdateStack (Maybe Text)
usDescription = lens _usDescription (\s a -> s {_usDescription = a})

-- | The URL that users are redirected to after their streaming session ends.
usRedirectURL :: Lens' UpdateStack (Maybe Text)
usRedirectURL = lens _usRedirectURL (\s a -> s {_usRedirectURL = a})

-- | The name of the stack.
usName :: Lens' UpdateStack Text
usName = lens _usName (\s a -> s {_usName = a})

instance AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request = postJSON appStream
  response =
    receiveJSON
      ( \s h x ->
          UpdateStackResponse' <$> (x .?> "Stack") <*> (pure (fromEnum s))
      )

instance Hashable UpdateStack

instance NFData UpdateStack

instance ToHeaders UpdateStack where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.UpdateStack" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    object
      ( catMaybes
          [ ("UserSettings" .=) <$> _usUserSettings,
            ("ApplicationSettings" .=) <$> _usApplicationSettings,
            ("FeedbackURL" .=) <$> _usFeedbackURL,
            ("AttributesToDelete" .=) <$> _usAttributesToDelete,
            ("DeleteStorageConnectors" .=) <$> _usDeleteStorageConnectors,
            ("StorageConnectors" .=) <$> _usStorageConnectors,
            ("AccessEndpoints" .=) <$> _usAccessEndpoints,
            ("DisplayName" .=) <$> _usDisplayName,
            ("EmbedHostDomains" .=) <$> _usEmbedHostDomains,
            ("Description" .=) <$> _usDescription,
            ("RedirectURL" .=) <$> _usRedirectURL,
            Just ("Name" .= _usName)
          ]
      )

instance ToPath UpdateStack where
  toPath = const "/"

instance ToQuery UpdateStack where
  toQuery = const mempty

-- | /See:/ 'updateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { _usrsStack ::
      !(Maybe Stack),
    _usrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsStack' - Information about the stack.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStackResponse ::
  -- | 'usrsResponseStatus'
  Int ->
  UpdateStackResponse
updateStackResponse pResponseStatus_ =
  UpdateStackResponse'
    { _usrsStack = Nothing,
      _usrsResponseStatus = pResponseStatus_
    }

-- | Information about the stack.
usrsStack :: Lens' UpdateStackResponse (Maybe Stack)
usrsStack = lens _usrsStack (\s a -> s {_usrsStack = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStackResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\s a -> s {_usrsResponseStatus = a})

instance NFData UpdateStackResponse
