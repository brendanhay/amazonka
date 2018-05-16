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
-- Module      : Network.AWS.DirectoryService.StartSchemaExtension
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a schema extension to a Microsoft AD directory.
--
--
module Network.AWS.DirectoryService.StartSchemaExtension
    (
    -- * Creating a Request
      startSchemaExtension
    , StartSchemaExtension
    -- * Request Lenses
    , sseDirectoryId
    , sseCreateSnapshotBeforeSchemaExtension
    , sseLdifContent
    , sseDescription

    -- * Destructuring the Response
    , startSchemaExtensionResponse
    , StartSchemaExtensionResponse
    -- * Response Lenses
    , ssersSchemaExtensionId
    , ssersResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startSchemaExtension' smart constructor.
data StartSchemaExtension = StartSchemaExtension'
  { _sseDirectoryId                         :: !Text
  , _sseCreateSnapshotBeforeSchemaExtension :: !Bool
  , _sseLdifContent                         :: !Text
  , _sseDescription                         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSchemaExtension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseDirectoryId' - The identifier of the directory for which the schema extension will be applied to.
--
-- * 'sseCreateSnapshotBeforeSchemaExtension' - If true, creates a snapshot of the directory before applying the schema extension.
--
-- * 'sseLdifContent' - The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
--
-- * 'sseDescription' - A description of the schema extension.
startSchemaExtension
    :: Text -- ^ 'sseDirectoryId'
    -> Bool -- ^ 'sseCreateSnapshotBeforeSchemaExtension'
    -> Text -- ^ 'sseLdifContent'
    -> Text -- ^ 'sseDescription'
    -> StartSchemaExtension
startSchemaExtension pDirectoryId_ pCreateSnapshotBeforeSchemaExtension_ pLdifContent_ pDescription_ =
  StartSchemaExtension'
    { _sseDirectoryId = pDirectoryId_
    , _sseCreateSnapshotBeforeSchemaExtension =
        pCreateSnapshotBeforeSchemaExtension_
    , _sseLdifContent = pLdifContent_
    , _sseDescription = pDescription_
    }


-- | The identifier of the directory for which the schema extension will be applied to.
sseDirectoryId :: Lens' StartSchemaExtension Text
sseDirectoryId = lens _sseDirectoryId (\ s a -> s{_sseDirectoryId = a})

-- | If true, creates a snapshot of the directory before applying the schema extension.
sseCreateSnapshotBeforeSchemaExtension :: Lens' StartSchemaExtension Bool
sseCreateSnapshotBeforeSchemaExtension = lens _sseCreateSnapshotBeforeSchemaExtension (\ s a -> s{_sseCreateSnapshotBeforeSchemaExtension = a})

-- | The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
sseLdifContent :: Lens' StartSchemaExtension Text
sseLdifContent = lens _sseLdifContent (\ s a -> s{_sseLdifContent = a})

-- | A description of the schema extension.
sseDescription :: Lens' StartSchemaExtension Text
sseDescription = lens _sseDescription (\ s a -> s{_sseDescription = a})

instance AWSRequest StartSchemaExtension where
        type Rs StartSchemaExtension =
             StartSchemaExtensionResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 StartSchemaExtensionResponse' <$>
                   (x .?> "SchemaExtensionId") <*> (pure (fromEnum s)))

instance Hashable StartSchemaExtension where

instance NFData StartSchemaExtension where

instance ToHeaders StartSchemaExtension where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.StartSchemaExtension" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartSchemaExtension where
        toJSON StartSchemaExtension'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _sseDirectoryId),
                  Just
                    ("CreateSnapshotBeforeSchemaExtension" .=
                       _sseCreateSnapshotBeforeSchemaExtension),
                  Just ("LdifContent" .= _sseLdifContent),
                  Just ("Description" .= _sseDescription)])

instance ToPath StartSchemaExtension where
        toPath = const "/"

instance ToQuery StartSchemaExtension where
        toQuery = const mempty

-- | /See:/ 'startSchemaExtensionResponse' smart constructor.
data StartSchemaExtensionResponse = StartSchemaExtensionResponse'
  { _ssersSchemaExtensionId :: !(Maybe Text)
  , _ssersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSchemaExtensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssersSchemaExtensionId' - The identifier of the schema extension that will be applied.
--
-- * 'ssersResponseStatus' - -- | The response status code.
startSchemaExtensionResponse
    :: Int -- ^ 'ssersResponseStatus'
    -> StartSchemaExtensionResponse
startSchemaExtensionResponse pResponseStatus_ =
  StartSchemaExtensionResponse'
    {_ssersSchemaExtensionId = Nothing, _ssersResponseStatus = pResponseStatus_}


-- | The identifier of the schema extension that will be applied.
ssersSchemaExtensionId :: Lens' StartSchemaExtensionResponse (Maybe Text)
ssersSchemaExtensionId = lens _ssersSchemaExtensionId (\ s a -> s{_ssersSchemaExtensionId = a})

-- | -- | The response status code.
ssersResponseStatus :: Lens' StartSchemaExtensionResponse Int
ssersResponseStatus = lens _ssersResponseStatus (\ s a -> s{_ssersResponseStatus = a})

instance NFData StartSchemaExtensionResponse where
