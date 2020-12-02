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
-- Module      : Network.AWS.MediaLive.UpdateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a program in a multiplex.
module Network.AWS.MediaLive.UpdateMultiplexProgram
  ( -- * Creating a Request
    updateMultiplexProgram,
    UpdateMultiplexProgram,

    -- * Request Lenses
    umpMultiplexProgramSettings,
    umpMultiplexId,
    umpProgramName,

    -- * Destructuring the Response
    updateMultiplexProgramResponse,
    UpdateMultiplexProgramResponse,

    -- * Response Lenses
    umprsMultiplexProgram,
    umprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to update a program in a multiplex.
--
-- /See:/ 'updateMultiplexProgram' smart constructor.
data UpdateMultiplexProgram = UpdateMultiplexProgram'
  { _umpMultiplexProgramSettings ::
      !(Maybe MultiplexProgramSettings),
    _umpMultiplexId :: !Text,
    _umpProgramName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umpMultiplexProgramSettings' - The new settings for a multiplex program.
--
-- * 'umpMultiplexId' - The ID of the multiplex of the program to update.
--
-- * 'umpProgramName' - The name of the program to update.
updateMultiplexProgram ::
  -- | 'umpMultiplexId'
  Text ->
  -- | 'umpProgramName'
  Text ->
  UpdateMultiplexProgram
updateMultiplexProgram pMultiplexId_ pProgramName_ =
  UpdateMultiplexProgram'
    { _umpMultiplexProgramSettings = Nothing,
      _umpMultiplexId = pMultiplexId_,
      _umpProgramName = pProgramName_
    }

-- | The new settings for a multiplex program.
umpMultiplexProgramSettings :: Lens' UpdateMultiplexProgram (Maybe MultiplexProgramSettings)
umpMultiplexProgramSettings = lens _umpMultiplexProgramSettings (\s a -> s {_umpMultiplexProgramSettings = a})

-- | The ID of the multiplex of the program to update.
umpMultiplexId :: Lens' UpdateMultiplexProgram Text
umpMultiplexId = lens _umpMultiplexId (\s a -> s {_umpMultiplexId = a})

-- | The name of the program to update.
umpProgramName :: Lens' UpdateMultiplexProgram Text
umpProgramName = lens _umpProgramName (\s a -> s {_umpProgramName = a})

instance AWSRequest UpdateMultiplexProgram where
  type Rs UpdateMultiplexProgram = UpdateMultiplexProgramResponse
  request = putJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          UpdateMultiplexProgramResponse'
            <$> (x .?> "multiplexProgram") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMultiplexProgram

instance NFData UpdateMultiplexProgram

instance ToHeaders UpdateMultiplexProgram where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateMultiplexProgram where
  toJSON UpdateMultiplexProgram' {..} =
    object
      ( catMaybes
          [("multiplexProgramSettings" .=) <$> _umpMultiplexProgramSettings]
      )

instance ToPath UpdateMultiplexProgram where
  toPath UpdateMultiplexProgram' {..} =
    mconcat
      [ "/prod/multiplexes/",
        toBS _umpMultiplexId,
        "/programs/",
        toBS _umpProgramName
      ]

instance ToQuery UpdateMultiplexProgram where
  toQuery = const mempty

-- | Placeholder documentation for UpdateMultiplexProgramResponse
--
-- /See:/ 'updateMultiplexProgramResponse' smart constructor.
data UpdateMultiplexProgramResponse = UpdateMultiplexProgramResponse'
  { _umprsMultiplexProgram ::
      !(Maybe MultiplexProgram),
    _umprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umprsMultiplexProgram' - The updated multiplex program.
--
-- * 'umprsResponseStatus' - -- | The response status code.
updateMultiplexProgramResponse ::
  -- | 'umprsResponseStatus'
  Int ->
  UpdateMultiplexProgramResponse
updateMultiplexProgramResponse pResponseStatus_ =
  UpdateMultiplexProgramResponse'
    { _umprsMultiplexProgram = Nothing,
      _umprsResponseStatus = pResponseStatus_
    }

-- | The updated multiplex program.
umprsMultiplexProgram :: Lens' UpdateMultiplexProgramResponse (Maybe MultiplexProgram)
umprsMultiplexProgram = lens _umprsMultiplexProgram (\s a -> s {_umprsMultiplexProgram = a})

-- | -- | The response status code.
umprsResponseStatus :: Lens' UpdateMultiplexProgramResponse Int
umprsResponseStatus = lens _umprsResponseStatus (\s a -> s {_umprsResponseStatus = a})

instance NFData UpdateMultiplexProgramResponse
