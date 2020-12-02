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
-- Module      : Network.AWS.SageMaker.StopAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A method for forcing the termination of a running job.
module Network.AWS.SageMaker.StopAutoMLJob
  ( -- * Creating a Request
    stopAutoMLJob,
    StopAutoMLJob,

    -- * Request Lenses
    samljAutoMLJobName,

    -- * Destructuring the Response
    stopAutoMLJobResponse,
    StopAutoMLJobResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'stopAutoMLJob' smart constructor.
newtype StopAutoMLJob = StopAutoMLJob' {_samljAutoMLJobName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopAutoMLJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samljAutoMLJobName' - The name of the object you are requesting.
stopAutoMLJob ::
  -- | 'samljAutoMLJobName'
  Text ->
  StopAutoMLJob
stopAutoMLJob pAutoMLJobName_ =
  StopAutoMLJob' {_samljAutoMLJobName = pAutoMLJobName_}

-- | The name of the object you are requesting.
samljAutoMLJobName :: Lens' StopAutoMLJob Text
samljAutoMLJobName = lens _samljAutoMLJobName (\s a -> s {_samljAutoMLJobName = a})

instance AWSRequest StopAutoMLJob where
  type Rs StopAutoMLJob = StopAutoMLJobResponse
  request = postJSON sageMaker
  response = receiveNull StopAutoMLJobResponse'

instance Hashable StopAutoMLJob

instance NFData StopAutoMLJob

instance ToHeaders StopAutoMLJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.StopAutoMLJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopAutoMLJob where
  toJSON StopAutoMLJob' {..} =
    object
      (catMaybes [Just ("AutoMLJobName" .= _samljAutoMLJobName)])

instance ToPath StopAutoMLJob where
  toPath = const "/"

instance ToQuery StopAutoMLJob where
  toQuery = const mempty

-- | /See:/ 'stopAutoMLJobResponse' smart constructor.
data StopAutoMLJobResponse = StopAutoMLJobResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopAutoMLJobResponse' with the minimum fields required to make a request.
stopAutoMLJobResponse ::
  StopAutoMLJobResponse
stopAutoMLJobResponse = StopAutoMLJobResponse'

instance NFData StopAutoMLJobResponse
