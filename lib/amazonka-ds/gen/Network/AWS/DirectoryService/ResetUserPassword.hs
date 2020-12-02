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
-- Module      : Network.AWS.DirectoryService.ResetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for any user in your AWS Managed Microsoft AD or Simple AD directory.
--
--
-- You can reset the password for any user in your directory with the following exceptions:
--
--     * For Simple AD, you cannot reset the password for any user that is a member of either the __Domain Admins__ or __Enterprise Admins__ group except for the administrator user.
--
--     * For AWS Managed Microsoft AD, you can only reset the password for a user that is in an OU based off of the NetBIOS name that you typed when you created your directory. For example, you cannot reset the password for a user in the __AWS Reserved__ OU. For more information about the OU structure for an AWS Managed Microsoft AD directory, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_getting_started_what_gets_created.html What Gets Created> in the /AWS Directory Service Administration Guide/ .
module Network.AWS.DirectoryService.ResetUserPassword
  ( -- * Creating a Request
    resetUserPassword,
    ResetUserPassword,

    -- * Request Lenses
    rupDirectoryId,
    rupUserName,
    rupNewPassword,

    -- * Destructuring the Response
    resetUserPasswordResponse,
    ResetUserPasswordResponse,

    -- * Response Lenses
    ruprsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetUserPassword' smart constructor.
data ResetUserPassword = ResetUserPassword'
  { _rupDirectoryId ::
      !Text,
    _rupUserName :: !Text,
    _rupNewPassword :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetUserPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rupDirectoryId' - Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
--
-- * 'rupUserName' - The user name of the user whose password will be reset.
--
-- * 'rupNewPassword' - The new password that will be reset.
resetUserPassword ::
  -- | 'rupDirectoryId'
  Text ->
  -- | 'rupUserName'
  Text ->
  -- | 'rupNewPassword'
  Text ->
  ResetUserPassword
resetUserPassword pDirectoryId_ pUserName_ pNewPassword_ =
  ResetUserPassword'
    { _rupDirectoryId = pDirectoryId_,
      _rupUserName = pUserName_,
      _rupNewPassword = _Sensitive # pNewPassword_
    }

-- | Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
rupDirectoryId :: Lens' ResetUserPassword Text
rupDirectoryId = lens _rupDirectoryId (\s a -> s {_rupDirectoryId = a})

-- | The user name of the user whose password will be reset.
rupUserName :: Lens' ResetUserPassword Text
rupUserName = lens _rupUserName (\s a -> s {_rupUserName = a})

-- | The new password that will be reset.
rupNewPassword :: Lens' ResetUserPassword Text
rupNewPassword = lens _rupNewPassword (\s a -> s {_rupNewPassword = a}) . _Sensitive

instance AWSRequest ResetUserPassword where
  type Rs ResetUserPassword = ResetUserPasswordResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> ResetUserPasswordResponse' <$> (pure (fromEnum s)))

instance Hashable ResetUserPassword

instance NFData ResetUserPassword

instance ToHeaders ResetUserPassword where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.ResetUserPassword" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ResetUserPassword where
  toJSON ResetUserPassword' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _rupDirectoryId),
            Just ("UserName" .= _rupUserName),
            Just ("NewPassword" .= _rupNewPassword)
          ]
      )

instance ToPath ResetUserPassword where
  toPath = const "/"

instance ToQuery ResetUserPassword where
  toQuery = const mempty

-- | /See:/ 'resetUserPasswordResponse' smart constructor.
newtype ResetUserPasswordResponse = ResetUserPasswordResponse'
  { _ruprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResetUserPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruprsResponseStatus' - -- | The response status code.
resetUserPasswordResponse ::
  -- | 'ruprsResponseStatus'
  Int ->
  ResetUserPasswordResponse
resetUserPasswordResponse pResponseStatus_ =
  ResetUserPasswordResponse'
    { _ruprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ruprsResponseStatus :: Lens' ResetUserPasswordResponse Int
ruprsResponseStatus = lens _ruprsResponseStatus (\s a -> s {_ruprsResponseStatus = a})

instance NFData ResetUserPasswordResponse
