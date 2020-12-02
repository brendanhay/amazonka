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
-- Module      : Network.AWS.DirectoryService.EnableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the switch for the specific directory to always use LDAP secure calls.
module Network.AWS.DirectoryService.EnableLDAPS
  ( -- * Creating a Request
    enableLDAPS,
    EnableLDAPS,

    -- * Request Lenses
    eldapsDirectoryId,
    eldapsType,

    -- * Destructuring the Response
    enableLDAPSResponse,
    EnableLDAPSResponse,

    -- * Response Lenses
    eldapsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
  { _eldapsDirectoryId :: !Text,
    _eldapsType :: !LDAPSType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableLDAPS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eldapsDirectoryId' - The identifier of the directory.
--
-- * 'eldapsType' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
enableLDAPS ::
  -- | 'eldapsDirectoryId'
  Text ->
  -- | 'eldapsType'
  LDAPSType ->
  EnableLDAPS
enableLDAPS pDirectoryId_ pType_ =
  EnableLDAPS'
    { _eldapsDirectoryId = pDirectoryId_,
      _eldapsType = pType_
    }

-- | The identifier of the directory.
eldapsDirectoryId :: Lens' EnableLDAPS Text
eldapsDirectoryId = lens _eldapsDirectoryId (\s a -> s {_eldapsDirectoryId = a})

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
eldapsType :: Lens' EnableLDAPS LDAPSType
eldapsType = lens _eldapsType (\s a -> s {_eldapsType = a})

instance AWSRequest EnableLDAPS where
  type Rs EnableLDAPS = EnableLDAPSResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> EnableLDAPSResponse' <$> (pure (fromEnum s)))

instance Hashable EnableLDAPS

instance NFData EnableLDAPS

instance ToHeaders EnableLDAPS where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.EnableLDAPS" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON EnableLDAPS where
  toJSON EnableLDAPS' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _eldapsDirectoryId),
            Just ("Type" .= _eldapsType)
          ]
      )

instance ToPath EnableLDAPS where
  toPath = const "/"

instance ToQuery EnableLDAPS where
  toQuery = const mempty

-- | /See:/ 'enableLDAPSResponse' smart constructor.
newtype EnableLDAPSResponse = EnableLDAPSResponse'
  { _eldapsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableLDAPSResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eldapsrsResponseStatus' - -- | The response status code.
enableLDAPSResponse ::
  -- | 'eldapsrsResponseStatus'
  Int ->
  EnableLDAPSResponse
enableLDAPSResponse pResponseStatus_ =
  EnableLDAPSResponse' {_eldapsrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
eldapsrsResponseStatus :: Lens' EnableLDAPSResponse Int
eldapsrsResponseStatus = lens _eldapsrsResponseStatus (\s a -> s {_eldapsrsResponseStatus = a})

instance NFData EnableLDAPSResponse
