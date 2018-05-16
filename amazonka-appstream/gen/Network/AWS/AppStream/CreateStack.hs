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
-- Module      : Network.AWS.AppStream.CreateStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack.
--
--
module Network.AWS.AppStream.CreateStack
    (
    -- * Creating a Request
      createStack
    , CreateStack
    -- * Request Lenses
    , csFeedbackURL
    , csStorageConnectors
    , csDisplayName
    , csDescription
    , csRedirectURL
    , csName

    -- * Destructuring the Response
    , createStackResponse
    , CreateStackResponse
    -- * Response Lenses
    , csrsStack
    , csrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStack' smart constructor.
data CreateStack = CreateStack'
  { _csFeedbackURL       :: !(Maybe Text)
  , _csStorageConnectors :: !(Maybe [StorageConnector])
  , _csDisplayName       :: !(Maybe Text)
  , _csDescription       :: !(Maybe Text)
  , _csRedirectURL       :: !(Maybe Text)
  , _csName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csFeedbackURL' - The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
--
-- * 'csStorageConnectors' - The storage connectors to enable.
--
-- * 'csDisplayName' - The stack name for display.
--
-- * 'csDescription' - The description for display.
--
-- * 'csRedirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- * 'csName' - The name of the stack.
createStack
    :: Text -- ^ 'csName'
    -> CreateStack
createStack pName_ =
  CreateStack'
    { _csFeedbackURL = Nothing
    , _csStorageConnectors = Nothing
    , _csDisplayName = Nothing
    , _csDescription = Nothing
    , _csRedirectURL = Nothing
    , _csName = pName_
    }


-- | The URL that users are redirected to after they click the Send Feedback link. If no URL is specified, no Send Feedback link is displayed.
csFeedbackURL :: Lens' CreateStack (Maybe Text)
csFeedbackURL = lens _csFeedbackURL (\ s a -> s{_csFeedbackURL = a})

-- | The storage connectors to enable.
csStorageConnectors :: Lens' CreateStack [StorageConnector]
csStorageConnectors = lens _csStorageConnectors (\ s a -> s{_csStorageConnectors = a}) . _Default . _Coerce

-- | The stack name for display.
csDisplayName :: Lens' CreateStack (Maybe Text)
csDisplayName = lens _csDisplayName (\ s a -> s{_csDisplayName = a})

-- | The description for display.
csDescription :: Lens' CreateStack (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The URL that users are redirected to after their streaming session ends.
csRedirectURL :: Lens' CreateStack (Maybe Text)
csRedirectURL = lens _csRedirectURL (\ s a -> s{_csRedirectURL = a})

-- | The name of the stack.
csName :: Lens' CreateStack Text
csName = lens _csName (\ s a -> s{_csName = a})

instance AWSRequest CreateStack where
        type Rs CreateStack = CreateStackResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateStackResponse' <$>
                   (x .?> "Stack") <*> (pure (fromEnum s)))

instance Hashable CreateStack where

instance NFData CreateStack where

instance ToHeaders CreateStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateStack" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStack where
        toJSON CreateStack'{..}
          = object
              (catMaybes
                 [("FeedbackURL" .=) <$> _csFeedbackURL,
                  ("StorageConnectors" .=) <$> _csStorageConnectors,
                  ("DisplayName" .=) <$> _csDisplayName,
                  ("Description" .=) <$> _csDescription,
                  ("RedirectURL" .=) <$> _csRedirectURL,
                  Just ("Name" .= _csName)])

instance ToPath CreateStack where
        toPath = const "/"

instance ToQuery CreateStack where
        toQuery = const mempty

-- | /See:/ 'createStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { _csrsStack          :: !(Maybe Stack)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsStack' - Information about the stack.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createStackResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateStackResponse
createStackResponse pResponseStatus_ =
  CreateStackResponse'
    {_csrsStack = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | Information about the stack.
csrsStack :: Lens' CreateStackResponse (Maybe Stack)
csrsStack = lens _csrsStack (\ s a -> s{_csrsStack = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateStackResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateStackResponse where
