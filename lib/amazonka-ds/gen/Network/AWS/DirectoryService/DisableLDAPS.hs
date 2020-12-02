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
-- Module      : Network.AWS.DirectoryService.DisableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates LDAP secure calls for the specified directory.
module Network.AWS.DirectoryService.DisableLDAPS
  ( -- * Creating a Request
    disableLDAPS,
    DisableLDAPS,

    -- * Request Lenses
    dldapsDirectoryId,
    dldapsType,

    -- * Destructuring the Response
    disableLDAPSResponse,
    DisableLDAPSResponse,

    -- * Response Lenses
    dldapsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { _dldapsDirectoryId :: !Text,
    _dldapsType :: !LDAPSType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableLDAPS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldapsDirectoryId' - The identifier of the directory.
--
-- * 'dldapsType' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
disableLDAPS ::
  -- | 'dldapsDirectoryId'
  Text ->
  -- | 'dldapsType'
  LDAPSType ->
  DisableLDAPS
disableLDAPS pDirectoryId_ pType_ =
  DisableLDAPS'
    { _dldapsDirectoryId = pDirectoryId_,
      _dldapsType = pType_
    }

-- | The identifier of the directory.
dldapsDirectoryId :: Lens' DisableLDAPS Text
dldapsDirectoryId = lens _dldapsDirectoryId (\s a -> s {_dldapsDirectoryId = a})

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
dldapsType :: Lens' DisableLDAPS LDAPSType
dldapsType = lens _dldapsType (\s a -> s {_dldapsType = a})

instance AWSRequest DisableLDAPS where
  type Rs DisableLDAPS = DisableLDAPSResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> DisableLDAPSResponse' <$> (pure (fromEnum s)))

instance Hashable DisableLDAPS

instance NFData DisableLDAPS

instance ToHeaders DisableLDAPS where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.DisableLDAPS" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisableLDAPS where
  toJSON DisableLDAPS' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _dldapsDirectoryId),
            Just ("Type" .= _dldapsType)
          ]
      )

instance ToPath DisableLDAPS where
  toPath = const "/"

instance ToQuery DisableLDAPS where
  toQuery = const mempty

-- | /See:/ 'disableLDAPSResponse' smart constructor.
newtype DisableLDAPSResponse = DisableLDAPSResponse'
  { _dldapsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableLDAPSResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldapsrsResponseStatus' - -- | The response status code.
disableLDAPSResponse ::
  -- | 'dldapsrsResponseStatus'
  Int ->
  DisableLDAPSResponse
disableLDAPSResponse pResponseStatus_ =
  DisableLDAPSResponse' {_dldapsrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dldapsrsResponseStatus :: Lens' DisableLDAPSResponse Int
dldapsrsResponseStatus = lens _dldapsrsResponseStatus (\s a -> s {_dldapsrsResponseStatus = a})

instance NFData DisableLDAPSResponse
