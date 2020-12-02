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
-- Module      : Network.AWS.CodeBuild.DeleteSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials.
module Network.AWS.CodeBuild.DeleteSourceCredentials
  ( -- * Creating a Request
    deleteSourceCredentials,
    DeleteSourceCredentials,

    -- * Request Lenses
    dscArn,

    -- * Destructuring the Response
    deleteSourceCredentialsResponse,
    DeleteSourceCredentialsResponse,

    -- * Response Lenses
    dscrsArn,
    dscrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSourceCredentials' smart constructor.
newtype DeleteSourceCredentials = DeleteSourceCredentials'
  { _dscArn ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSourceCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscArn' - The Amazon Resource Name (ARN) of the token.
deleteSourceCredentials ::
  -- | 'dscArn'
  Text ->
  DeleteSourceCredentials
deleteSourceCredentials pArn_ =
  DeleteSourceCredentials' {_dscArn = pArn_}

-- | The Amazon Resource Name (ARN) of the token.
dscArn :: Lens' DeleteSourceCredentials Text
dscArn = lens _dscArn (\s a -> s {_dscArn = a})

instance AWSRequest DeleteSourceCredentials where
  type Rs DeleteSourceCredentials = DeleteSourceCredentialsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          DeleteSourceCredentialsResponse'
            <$> (x .?> "arn") <*> (pure (fromEnum s))
      )

instance Hashable DeleteSourceCredentials

instance NFData DeleteSourceCredentials

instance ToHeaders DeleteSourceCredentials where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DeleteSourceCredentials" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteSourceCredentials where
  toJSON DeleteSourceCredentials' {..} =
    object (catMaybes [Just ("arn" .= _dscArn)])

instance ToPath DeleteSourceCredentials where
  toPath = const "/"

instance ToQuery DeleteSourceCredentials where
  toQuery = const mempty

-- | /See:/ 'deleteSourceCredentialsResponse' smart constructor.
data DeleteSourceCredentialsResponse = DeleteSourceCredentialsResponse'
  { _dscrsArn ::
      !(Maybe Text),
    _dscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsArn' - The Amazon Resource Name (ARN) of the token.
--
-- * 'dscrsResponseStatus' - -- | The response status code.
deleteSourceCredentialsResponse ::
  -- | 'dscrsResponseStatus'
  Int ->
  DeleteSourceCredentialsResponse
deleteSourceCredentialsResponse pResponseStatus_ =
  DeleteSourceCredentialsResponse'
    { _dscrsArn = Nothing,
      _dscrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
dscrsArn :: Lens' DeleteSourceCredentialsResponse (Maybe Text)
dscrsArn = lens _dscrsArn (\s a -> s {_dscrsArn = a})

-- | -- | The response status code.
dscrsResponseStatus :: Lens' DeleteSourceCredentialsResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\s a -> s {_dscrsResponseStatus = a})

instance NFData DeleteSourceCredentialsResponse
