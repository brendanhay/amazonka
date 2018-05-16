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
-- Module      : Network.AWS.CloudDirectory.EnableDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.
--
--
module Network.AWS.CloudDirectory.EnableDirectory
    (
    -- * Creating a Request
      enableDirectory
    , EnableDirectory
    -- * Request Lenses
    , edDirectoryARN

    -- * Destructuring the Response
    , enableDirectoryResponse
    , EnableDirectoryResponse
    -- * Response Lenses
    , edrsResponseStatus
    , edrsDirectoryARN
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableDirectory' smart constructor.
newtype EnableDirectory = EnableDirectory'
  { _edDirectoryARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDirectoryARN' - The ARN of the directory to enable.
enableDirectory
    :: Text -- ^ 'edDirectoryARN'
    -> EnableDirectory
enableDirectory pDirectoryARN_ =
  EnableDirectory' {_edDirectoryARN = pDirectoryARN_}


-- | The ARN of the directory to enable.
edDirectoryARN :: Lens' EnableDirectory Text
edDirectoryARN = lens _edDirectoryARN (\ s a -> s{_edDirectoryARN = a})

instance AWSRequest EnableDirectory where
        type Rs EnableDirectory = EnableDirectoryResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 EnableDirectoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DirectoryArn"))

instance Hashable EnableDirectory where

instance NFData EnableDirectory where

instance ToHeaders EnableDirectory where
        toHeaders EnableDirectory'{..}
          = mconcat ["x-amz-data-partition" =# _edDirectoryARN]

instance ToJSON EnableDirectory where
        toJSON = const (Object mempty)

instance ToPath EnableDirectory where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/directory/enable"

instance ToQuery EnableDirectory where
        toQuery = const mempty

-- | /See:/ 'enableDirectoryResponse' smart constructor.
data EnableDirectoryResponse = EnableDirectoryResponse'
  { _edrsResponseStatus :: !Int
  , _edrsDirectoryARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edrsResponseStatus' - -- | The response status code.
--
-- * 'edrsDirectoryARN' - The ARN of the enabled directory.
enableDirectoryResponse
    :: Int -- ^ 'edrsResponseStatus'
    -> Text -- ^ 'edrsDirectoryARN'
    -> EnableDirectoryResponse
enableDirectoryResponse pResponseStatus_ pDirectoryARN_ =
  EnableDirectoryResponse'
    {_edrsResponseStatus = pResponseStatus_, _edrsDirectoryARN = pDirectoryARN_}


-- | -- | The response status code.
edrsResponseStatus :: Lens' EnableDirectoryResponse Int
edrsResponseStatus = lens _edrsResponseStatus (\ s a -> s{_edrsResponseStatus = a})

-- | The ARN of the enabled directory.
edrsDirectoryARN :: Lens' EnableDirectoryResponse Text
edrsDirectoryARN = lens _edrsDirectoryARN (\ s a -> s{_edrsDirectoryARN = a})

instance NFData EnableDirectoryResponse where
