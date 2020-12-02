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
-- Module      : Network.AWS.Rekognition.StopProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running model. The operation might take a while to complete. To check the current status, call 'DescribeProjectVersions' .
module Network.AWS.Rekognition.StopProjectVersion
  ( -- * Creating a Request
    stopProjectVersion,
    StopProjectVersion,

    -- * Request Lenses
    sProjectVersionARN,

    -- * Destructuring the Response
    stopProjectVersionResponse,
    StopProjectVersionResponse,

    -- * Response Lenses
    srsStatus,
    srsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopProjectVersion' smart constructor.
newtype StopProjectVersion = StopProjectVersion'
  { _sProjectVersionARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopProjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProjectVersionARN' - The Amazon Resource Name (ARN) of the model version that you want to delete. This operation requires permissions to perform the @rekognition:StopProjectVersion@ action.
stopProjectVersion ::
  -- | 'sProjectVersionARN'
  Text ->
  StopProjectVersion
stopProjectVersion pProjectVersionARN_ =
  StopProjectVersion' {_sProjectVersionARN = pProjectVersionARN_}

-- | The Amazon Resource Name (ARN) of the model version that you want to delete. This operation requires permissions to perform the @rekognition:StopProjectVersion@ action.
sProjectVersionARN :: Lens' StopProjectVersion Text
sProjectVersionARN = lens _sProjectVersionARN (\s a -> s {_sProjectVersionARN = a})

instance AWSRequest StopProjectVersion where
  type Rs StopProjectVersion = StopProjectVersionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          StopProjectVersionResponse'
            <$> (x .?> "Status") <*> (pure (fromEnum s))
      )

instance Hashable StopProjectVersion

instance NFData StopProjectVersion

instance ToHeaders StopProjectVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.StopProjectVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopProjectVersion where
  toJSON StopProjectVersion' {..} =
    object
      (catMaybes [Just ("ProjectVersionArn" .= _sProjectVersionARN)])

instance ToPath StopProjectVersion where
  toPath = const "/"

instance ToQuery StopProjectVersion where
  toQuery = const mempty

-- | /See:/ 'stopProjectVersionResponse' smart constructor.
data StopProjectVersionResponse = StopProjectVersionResponse'
  { _srsStatus ::
      !(Maybe ProjectVersionStatus),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopProjectVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsStatus' - The current status of the stop operation.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopProjectVersionResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StopProjectVersionResponse
stopProjectVersionResponse pResponseStatus_ =
  StopProjectVersionResponse'
    { _srsStatus = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | The current status of the stop operation.
srsStatus :: Lens' StopProjectVersionResponse (Maybe ProjectVersionStatus)
srsStatus = lens _srsStatus (\s a -> s {_srsStatus = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopProjectVersionResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StopProjectVersionResponse
