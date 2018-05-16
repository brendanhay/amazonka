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
-- Module      : Network.AWS.AppStream.UpdateStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified stack.
--
--
module Network.AWS.AppStream.UpdateStack
    (
    -- * Creating a Request
      updateStack
    , UpdateStack
    -- * Request Lenses
    , usFeedbackURL
    , usAttributesToDelete
    , usDeleteStorageConnectors
    , usStorageConnectors
    , usDisplayName
    , usDescription
    , usRedirectURL
    , usName

    -- * Destructuring the Response
    , updateStackResponse
    , UpdateStackResponse
    -- * Response Lenses
    , usrsStack
    , usrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStack' smart constructor.
data UpdateStack = UpdateStack'
  { _usFeedbackURL             :: !(Maybe Text)
  , _usAttributesToDelete      :: !(Maybe [StackAttribute])
  , _usDeleteStorageConnectors :: !(Maybe Bool)
  , _usStorageConnectors       :: !(Maybe [StorageConnector])
  , _usDisplayName             :: !(Maybe Text)
  , _usDescription             :: !(Maybe Text)
  , _usRedirectURL             :: !(Maybe Text)
  , _usName                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usFeedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- * 'usAttributesToDelete' - The stack attributes to delete.
--
-- * 'usDeleteStorageConnectors' - Deletes the storage connectors currently enabled for the stack.
--
-- * 'usStorageConnectors' - The storage connectors to enable.
--
-- * 'usDisplayName' - The stack name for display.
--
-- * 'usDescription' - The description for display.
--
-- * 'usRedirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- * 'usName' - The name of the stack.
updateStack
    :: Text -- ^ 'usName'
    -> UpdateStack
updateStack pName_ =
  UpdateStack'
    { _usFeedbackURL = Nothing
    , _usAttributesToDelete = Nothing
    , _usDeleteStorageConnectors = Nothing
    , _usStorageConnectors = Nothing
    , _usDisplayName = Nothing
    , _usDescription = Nothing
    , _usRedirectURL = Nothing
    , _usName = pName_
    }


-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
usFeedbackURL :: Lens' UpdateStack (Maybe Text)
usFeedbackURL = lens _usFeedbackURL (\ s a -> s{_usFeedbackURL = a})

-- | The stack attributes to delete.
usAttributesToDelete :: Lens' UpdateStack [StackAttribute]
usAttributesToDelete = lens _usAttributesToDelete (\ s a -> s{_usAttributesToDelete = a}) . _Default . _Coerce

-- | Deletes the storage connectors currently enabled for the stack.
usDeleteStorageConnectors :: Lens' UpdateStack (Maybe Bool)
usDeleteStorageConnectors = lens _usDeleteStorageConnectors (\ s a -> s{_usDeleteStorageConnectors = a})

-- | The storage connectors to enable.
usStorageConnectors :: Lens' UpdateStack [StorageConnector]
usStorageConnectors = lens _usStorageConnectors (\ s a -> s{_usStorageConnectors = a}) . _Default . _Coerce

-- | The stack name for display.
usDisplayName :: Lens' UpdateStack (Maybe Text)
usDisplayName = lens _usDisplayName (\ s a -> s{_usDisplayName = a})

-- | The description for display.
usDescription :: Lens' UpdateStack (Maybe Text)
usDescription = lens _usDescription (\ s a -> s{_usDescription = a})

-- | The URL that users are redirected to after their streaming session ends.
usRedirectURL :: Lens' UpdateStack (Maybe Text)
usRedirectURL = lens _usRedirectURL (\ s a -> s{_usRedirectURL = a})

-- | The name of the stack.
usName :: Lens' UpdateStack Text
usName = lens _usName (\ s a -> s{_usName = a})

instance AWSRequest UpdateStack where
        type Rs UpdateStack = UpdateStackResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 UpdateStackResponse' <$>
                   (x .?> "Stack") <*> (pure (fromEnum s)))

instance Hashable UpdateStack where

instance NFData UpdateStack where

instance ToHeaders UpdateStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.UpdateStack" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateStack where
        toJSON UpdateStack'{..}
          = object
              (catMaybes
                 [("FeedbackURL" .=) <$> _usFeedbackURL,
                  ("AttributesToDelete" .=) <$> _usAttributesToDelete,
                  ("DeleteStorageConnectors" .=) <$>
                    _usDeleteStorageConnectors,
                  ("StorageConnectors" .=) <$> _usStorageConnectors,
                  ("DisplayName" .=) <$> _usDisplayName,
                  ("Description" .=) <$> _usDescription,
                  ("RedirectURL" .=) <$> _usRedirectURL,
                  Just ("Name" .= _usName)])

instance ToPath UpdateStack where
        toPath = const "/"

instance ToQuery UpdateStack where
        toQuery = const mempty

-- | /See:/ 'updateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { _usrsStack          :: !(Maybe Stack)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsStack' - Information about the stack.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStackResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateStackResponse
updateStackResponse pResponseStatus_ =
  UpdateStackResponse'
    {_usrsStack = Nothing, _usrsResponseStatus = pResponseStatus_}


-- | Information about the stack.
usrsStack :: Lens' UpdateStackResponse (Maybe Stack)
usrsStack = lens _usrsStack (\ s a -> s{_usrsStack = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStackResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateStackResponse where
