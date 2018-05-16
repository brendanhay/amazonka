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
-- Module      : Network.AWS.DirectoryService.CancelSchemaExtension
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; @Initializing@ , @CreatingSnapshot@ , and @UpdatingSchema@ .
--
--
module Network.AWS.DirectoryService.CancelSchemaExtension
    (
    -- * Creating a Request
      cancelSchemaExtension
    , CancelSchemaExtension
    -- * Request Lenses
    , cseDirectoryId
    , cseSchemaExtensionId

    -- * Destructuring the Response
    , cancelSchemaExtensionResponse
    , CancelSchemaExtensionResponse
    -- * Response Lenses
    , csersResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelSchemaExtension' smart constructor.
data CancelSchemaExtension = CancelSchemaExtension'
  { _cseDirectoryId       :: !Text
  , _cseSchemaExtensionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSchemaExtension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cseDirectoryId' - The identifier of the directory whose schema extension will be canceled.
--
-- * 'cseSchemaExtensionId' - The identifier of the schema extension that will be canceled.
cancelSchemaExtension
    :: Text -- ^ 'cseDirectoryId'
    -> Text -- ^ 'cseSchemaExtensionId'
    -> CancelSchemaExtension
cancelSchemaExtension pDirectoryId_ pSchemaExtensionId_ =
  CancelSchemaExtension'
    { _cseDirectoryId = pDirectoryId_
    , _cseSchemaExtensionId = pSchemaExtensionId_
    }


-- | The identifier of the directory whose schema extension will be canceled.
cseDirectoryId :: Lens' CancelSchemaExtension Text
cseDirectoryId = lens _cseDirectoryId (\ s a -> s{_cseDirectoryId = a})

-- | The identifier of the schema extension that will be canceled.
cseSchemaExtensionId :: Lens' CancelSchemaExtension Text
cseSchemaExtensionId = lens _cseSchemaExtensionId (\ s a -> s{_cseSchemaExtensionId = a})

instance AWSRequest CancelSchemaExtension where
        type Rs CancelSchemaExtension =
             CancelSchemaExtensionResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 CancelSchemaExtensionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CancelSchemaExtension where

instance NFData CancelSchemaExtension where

instance ToHeaders CancelSchemaExtension where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CancelSchemaExtension" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelSchemaExtension where
        toJSON CancelSchemaExtension'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _cseDirectoryId),
                  Just ("SchemaExtensionId" .= _cseSchemaExtensionId)])

instance ToPath CancelSchemaExtension where
        toPath = const "/"

instance ToQuery CancelSchemaExtension where
        toQuery = const mempty

-- | /See:/ 'cancelSchemaExtensionResponse' smart constructor.
newtype CancelSchemaExtensionResponse = CancelSchemaExtensionResponse'
  { _csersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSchemaExtensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csersResponseStatus' - -- | The response status code.
cancelSchemaExtensionResponse
    :: Int -- ^ 'csersResponseStatus'
    -> CancelSchemaExtensionResponse
cancelSchemaExtensionResponse pResponseStatus_ =
  CancelSchemaExtensionResponse' {_csersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
csersResponseStatus :: Lens' CancelSchemaExtensionResponse Int
csersResponseStatus = lens _csersResponseStatus (\ s a -> s{_csersResponseStatus = a})

instance NFData CancelSchemaExtensionResponse where
