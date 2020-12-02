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
-- Module      : Network.AWS.Lambda.DeleteCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the code signing configuration. You can delete the code signing configuration only if no function is using it.
module Network.AWS.Lambda.DeleteCodeSigningConfig
  ( -- * Creating a Request
    deleteCodeSigningConfig,
    DeleteCodeSigningConfig,

    -- * Request Lenses
    dcscCodeSigningConfigARN,

    -- * Destructuring the Response
    deleteCodeSigningConfigResponse,
    DeleteCodeSigningConfigResponse,

    -- * Response Lenses
    dcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCodeSigningConfig' smart constructor.
newtype DeleteCodeSigningConfig = DeleteCodeSigningConfig'
  { _dcscCodeSigningConfigARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscCodeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
deleteCodeSigningConfig ::
  -- | 'dcscCodeSigningConfigARN'
  Text ->
  DeleteCodeSigningConfig
deleteCodeSigningConfig pCodeSigningConfigARN_ =
  DeleteCodeSigningConfig'
    { _dcscCodeSigningConfigARN =
        pCodeSigningConfigARN_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
dcscCodeSigningConfigARN :: Lens' DeleteCodeSigningConfig Text
dcscCodeSigningConfigARN = lens _dcscCodeSigningConfigARN (\s a -> s {_dcscCodeSigningConfigARN = a})

instance AWSRequest DeleteCodeSigningConfig where
  type Rs DeleteCodeSigningConfig = DeleteCodeSigningConfigResponse
  request = delete lambda
  response =
    receiveEmpty
      ( \s h x ->
          DeleteCodeSigningConfigResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteCodeSigningConfig

instance NFData DeleteCodeSigningConfig

instance ToHeaders DeleteCodeSigningConfig where
  toHeaders = const mempty

instance ToPath DeleteCodeSigningConfig where
  toPath DeleteCodeSigningConfig' {..} =
    mconcat
      [ "/2020-04-22/code-signing-configs/",
        toBS _dcscCodeSigningConfigARN
      ]

instance ToQuery DeleteCodeSigningConfig where
  toQuery = const mempty

-- | /See:/ 'deleteCodeSigningConfigResponse' smart constructor.
newtype DeleteCodeSigningConfigResponse = DeleteCodeSigningConfigResponse'
  { _dcscrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscrsResponseStatus' - -- | The response status code.
deleteCodeSigningConfigResponse ::
  -- | 'dcscrsResponseStatus'
  Int ->
  DeleteCodeSigningConfigResponse
deleteCodeSigningConfigResponse pResponseStatus_ =
  DeleteCodeSigningConfigResponse'
    { _dcscrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcscrsResponseStatus :: Lens' DeleteCodeSigningConfigResponse Int
dcscrsResponseStatus = lens _dcscrsResponseStatus (\s a -> s {_dcscrsResponseStatus = a})

instance NFData DeleteCodeSigningConfigResponse
