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
-- Module      : Network.AWS.CloudDirectory.DisableDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.
--
--
module Network.AWS.CloudDirectory.DisableDirectory
    (
    -- * Creating a Request
      disableDirectory
    , DisableDirectory
    -- * Request Lenses
    , ddDirectoryARN

    -- * Destructuring the Response
    , disableDirectoryResponse
    , DisableDirectoryResponse
    -- * Response Lenses
    , drsResponseStatus
    , drsDirectoryARN
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableDirectory' smart constructor.
newtype DisableDirectory = DisableDirectory'
  { _ddDirectoryARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDirectoryARN' - The ARN of the directory to disable.
disableDirectory
    :: Text -- ^ 'ddDirectoryARN'
    -> DisableDirectory
disableDirectory pDirectoryARN_ =
  DisableDirectory' {_ddDirectoryARN = pDirectoryARN_}


-- | The ARN of the directory to disable.
ddDirectoryARN :: Lens' DisableDirectory Text
ddDirectoryARN = lens _ddDirectoryARN (\ s a -> s{_ddDirectoryARN = a})

instance AWSRequest DisableDirectory where
        type Rs DisableDirectory = DisableDirectoryResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 DisableDirectoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DirectoryArn"))

instance Hashable DisableDirectory where

instance NFData DisableDirectory where

instance ToHeaders DisableDirectory where
        toHeaders DisableDirectory'{..}
          = mconcat ["x-amz-data-partition" =# _ddDirectoryARN]

instance ToJSON DisableDirectory where
        toJSON = const (Object mempty)

instance ToPath DisableDirectory where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/directory/disable"

instance ToQuery DisableDirectory where
        toQuery = const mempty

-- | /See:/ 'disableDirectoryResponse' smart constructor.
data DisableDirectoryResponse = DisableDirectoryResponse'
  { _drsResponseStatus :: !Int
  , _drsDirectoryARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsDirectoryARN' - The ARN of the directory that has been disabled.
disableDirectoryResponse
    :: Int -- ^ 'drsResponseStatus'
    -> Text -- ^ 'drsDirectoryARN'
    -> DisableDirectoryResponse
disableDirectoryResponse pResponseStatus_ pDirectoryARN_ =
  DisableDirectoryResponse'
    {_drsResponseStatus = pResponseStatus_, _drsDirectoryARN = pDirectoryARN_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DisableDirectoryResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

-- | The ARN of the directory that has been disabled.
drsDirectoryARN :: Lens' DisableDirectoryResponse Text
drsDirectoryARN = lens _drsDirectoryARN (\ s a -> s{_drsDirectoryARN = a})

instance NFData DisableDirectoryResponse where
