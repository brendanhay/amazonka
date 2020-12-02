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
-- Module      : Network.AWS.CodeBuild.RetryBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a build.
module Network.AWS.CodeBuild.RetryBuild
  ( -- * Creating a Request
    retryBuild,
    RetryBuild,

    -- * Request Lenses
    rbIdempotencyToken,
    rbId,

    -- * Destructuring the Response
    retryBuildResponse,
    RetryBuildResponse,

    -- * Response Lenses
    rbrsBuild,
    rbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'retryBuild' smart constructor.
data RetryBuild = RetryBuild'
  { _rbIdempotencyToken :: !(Maybe Text),
    _rbId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetryBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbIdempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- * 'rbId' - Specifies the identifier of the build to restart.
retryBuild ::
  RetryBuild
retryBuild =
  RetryBuild' {_rbIdempotencyToken = Nothing, _rbId = Nothing}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
rbIdempotencyToken :: Lens' RetryBuild (Maybe Text)
rbIdempotencyToken = lens _rbIdempotencyToken (\s a -> s {_rbIdempotencyToken = a})

-- | Specifies the identifier of the build to restart.
rbId :: Lens' RetryBuild (Maybe Text)
rbId = lens _rbId (\s a -> s {_rbId = a})

instance AWSRequest RetryBuild where
  type Rs RetryBuild = RetryBuildResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          RetryBuildResponse' <$> (x .?> "build") <*> (pure (fromEnum s))
      )

instance Hashable RetryBuild

instance NFData RetryBuild

instance ToHeaders RetryBuild where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("CodeBuild_20161006.RetryBuild" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RetryBuild where
  toJSON RetryBuild' {..} =
    object
      ( catMaybes
          [ ("idempotencyToken" .=) <$> _rbIdempotencyToken,
            ("id" .=) <$> _rbId
          ]
      )

instance ToPath RetryBuild where
  toPath = const "/"

instance ToQuery RetryBuild where
  toQuery = const mempty

-- | /See:/ 'retryBuildResponse' smart constructor.
data RetryBuildResponse = RetryBuildResponse'
  { _rbrsBuild ::
      !(Maybe Build),
    _rbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetryBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbrsBuild' - Undocumented member.
--
-- * 'rbrsResponseStatus' - -- | The response status code.
retryBuildResponse ::
  -- | 'rbrsResponseStatus'
  Int ->
  RetryBuildResponse
retryBuildResponse pResponseStatus_ =
  RetryBuildResponse'
    { _rbrsBuild = Nothing,
      _rbrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rbrsBuild :: Lens' RetryBuildResponse (Maybe Build)
rbrsBuild = lens _rbrsBuild (\s a -> s {_rbrsBuild = a})

-- | -- | The response status code.
rbrsResponseStatus :: Lens' RetryBuildResponse Int
rbrsResponseStatus = lens _rbrsResponseStatus (\s a -> s {_rbrsResponseStatus = a})

instance NFData RetryBuildResponse
