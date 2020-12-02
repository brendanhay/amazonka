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
-- Module      : Network.AWS.MediaLive.CreateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new program in the multiplex.
module Network.AWS.MediaLive.CreateMultiplexProgram
  ( -- * Creating a Request
    createMultiplexProgram,
    CreateMultiplexProgram,

    -- * Request Lenses
    cmpMultiplexId,
    cmpRequestId,
    cmpMultiplexProgramSettings,
    cmpProgramName,

    -- * Destructuring the Response
    createMultiplexProgramResponse,
    CreateMultiplexProgramResponse,

    -- * Response Lenses
    cmprsMultiplexProgram,
    cmprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to create a program in a multiplex.
--
-- /See:/ 'createMultiplexProgram' smart constructor.
data CreateMultiplexProgram = CreateMultiplexProgram'
  { _cmpMultiplexId ::
      !Text,
    _cmpRequestId :: !Text,
    _cmpMultiplexProgramSettings ::
      !MultiplexProgramSettings,
    _cmpProgramName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmpMultiplexId' - ID of the multiplex where the program is to be created.
--
-- * 'cmpRequestId' - Unique request ID. This prevents retries from creating multiple resources.
--
-- * 'cmpMultiplexProgramSettings' - The settings for this multiplex program.
--
-- * 'cmpProgramName' - Name of multiplex program.
createMultiplexProgram ::
  -- | 'cmpMultiplexId'
  Text ->
  -- | 'cmpRequestId'
  Text ->
  -- | 'cmpMultiplexProgramSettings'
  MultiplexProgramSettings ->
  -- | 'cmpProgramName'
  Text ->
  CreateMultiplexProgram
createMultiplexProgram
  pMultiplexId_
  pRequestId_
  pMultiplexProgramSettings_
  pProgramName_ =
    CreateMultiplexProgram'
      { _cmpMultiplexId = pMultiplexId_,
        _cmpRequestId = pRequestId_,
        _cmpMultiplexProgramSettings = pMultiplexProgramSettings_,
        _cmpProgramName = pProgramName_
      }

-- | ID of the multiplex where the program is to be created.
cmpMultiplexId :: Lens' CreateMultiplexProgram Text
cmpMultiplexId = lens _cmpMultiplexId (\s a -> s {_cmpMultiplexId = a})

-- | Unique request ID. This prevents retries from creating multiple resources.
cmpRequestId :: Lens' CreateMultiplexProgram Text
cmpRequestId = lens _cmpRequestId (\s a -> s {_cmpRequestId = a})

-- | The settings for this multiplex program.
cmpMultiplexProgramSettings :: Lens' CreateMultiplexProgram MultiplexProgramSettings
cmpMultiplexProgramSettings = lens _cmpMultiplexProgramSettings (\s a -> s {_cmpMultiplexProgramSettings = a})

-- | Name of multiplex program.
cmpProgramName :: Lens' CreateMultiplexProgram Text
cmpProgramName = lens _cmpProgramName (\s a -> s {_cmpProgramName = a})

instance AWSRequest CreateMultiplexProgram where
  type Rs CreateMultiplexProgram = CreateMultiplexProgramResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          CreateMultiplexProgramResponse'
            <$> (x .?> "multiplexProgram") <*> (pure (fromEnum s))
      )

instance Hashable CreateMultiplexProgram

instance NFData CreateMultiplexProgram

instance ToHeaders CreateMultiplexProgram where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateMultiplexProgram where
  toJSON CreateMultiplexProgram' {..} =
    object
      ( catMaybes
          [ Just ("requestId" .= _cmpRequestId),
            Just ("multiplexProgramSettings" .= _cmpMultiplexProgramSettings),
            Just ("programName" .= _cmpProgramName)
          ]
      )

instance ToPath CreateMultiplexProgram where
  toPath CreateMultiplexProgram' {..} =
    mconcat ["/prod/multiplexes/", toBS _cmpMultiplexId, "/programs"]

instance ToQuery CreateMultiplexProgram where
  toQuery = const mempty

-- | Placeholder documentation for CreateMultiplexProgramResponse
--
-- /See:/ 'createMultiplexProgramResponse' smart constructor.
data CreateMultiplexProgramResponse = CreateMultiplexProgramResponse'
  { _cmprsMultiplexProgram ::
      !(Maybe MultiplexProgram),
    _cmprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmprsMultiplexProgram' - The newly created multiplex program.
--
-- * 'cmprsResponseStatus' - -- | The response status code.
createMultiplexProgramResponse ::
  -- | 'cmprsResponseStatus'
  Int ->
  CreateMultiplexProgramResponse
createMultiplexProgramResponse pResponseStatus_ =
  CreateMultiplexProgramResponse'
    { _cmprsMultiplexProgram = Nothing,
      _cmprsResponseStatus = pResponseStatus_
    }

-- | The newly created multiplex program.
cmprsMultiplexProgram :: Lens' CreateMultiplexProgramResponse (Maybe MultiplexProgram)
cmprsMultiplexProgram = lens _cmprsMultiplexProgram (\s a -> s {_cmprsMultiplexProgram = a})

-- | -- | The response status code.
cmprsResponseStatus :: Lens' CreateMultiplexProgramResponse Int
cmprsResponseStatus = lens _cmprsResponseStatus (\s a -> s {_cmprsResponseStatus = a})

instance NFData CreateMultiplexProgramResponse
