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
-- Module      : Network.AWS.WorkSpaces.ModifyClientProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of the specified Amazon WorkSpaces client.
--
--
module Network.AWS.WorkSpaces.ModifyClientProperties
    (
    -- * Creating a Request
      modifyClientProperties
    , ModifyClientProperties
    -- * Request Lenses
    , mcpResourceId
    , mcpClientProperties

    -- * Destructuring the Response
    , modifyClientPropertiesResponse
    , ModifyClientPropertiesResponse
    -- * Response Lenses
    , mcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'modifyClientProperties' smart constructor.
data ModifyClientProperties = ModifyClientProperties'
  { _mcpResourceId       :: !Text
  , _mcpClientProperties :: !ClientProperties
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClientProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcpResourceId' - The resource identifiers, in the form of directory IDs.
--
-- * 'mcpClientProperties' - Information about the Amazon WorkSpaces client.
modifyClientProperties
    :: Text -- ^ 'mcpResourceId'
    -> ClientProperties -- ^ 'mcpClientProperties'
    -> ModifyClientProperties
modifyClientProperties pResourceId_ pClientProperties_ =
  ModifyClientProperties'
    {_mcpResourceId = pResourceId_, _mcpClientProperties = pClientProperties_}


-- | The resource identifiers, in the form of directory IDs.
mcpResourceId :: Lens' ModifyClientProperties Text
mcpResourceId = lens _mcpResourceId (\ s a -> s{_mcpResourceId = a})

-- | Information about the Amazon WorkSpaces client.
mcpClientProperties :: Lens' ModifyClientProperties ClientProperties
mcpClientProperties = lens _mcpClientProperties (\ s a -> s{_mcpClientProperties = a})

instance AWSRequest ModifyClientProperties where
        type Rs ModifyClientProperties =
             ModifyClientPropertiesResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 ModifyClientPropertiesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ModifyClientProperties where

instance NFData ModifyClientProperties where

instance ToHeaders ModifyClientProperties where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.ModifyClientProperties" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyClientProperties where
        toJSON ModifyClientProperties'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _mcpResourceId),
                  Just ("ClientProperties" .= _mcpClientProperties)])

instance ToPath ModifyClientProperties where
        toPath = const "/"

instance ToQuery ModifyClientProperties where
        toQuery = const mempty

-- | /See:/ 'modifyClientPropertiesResponse' smart constructor.
newtype ModifyClientPropertiesResponse = ModifyClientPropertiesResponse'
  { _mcprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClientPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcprsResponseStatus' - -- | The response status code.
modifyClientPropertiesResponse
    :: Int -- ^ 'mcprsResponseStatus'
    -> ModifyClientPropertiesResponse
modifyClientPropertiesResponse pResponseStatus_ =
  ModifyClientPropertiesResponse' {_mcprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
mcprsResponseStatus :: Lens' ModifyClientPropertiesResponse Int
mcprsResponseStatus = lens _mcprsResponseStatus (\ s a -> s{_mcprsResponseStatus = a})

instance NFData ModifyClientPropertiesResponse where
