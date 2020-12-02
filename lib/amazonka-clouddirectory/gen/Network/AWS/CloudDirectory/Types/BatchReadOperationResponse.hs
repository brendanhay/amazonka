{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperationResponse where

import Network.AWS.CloudDirectory.Types.BatchReadException
import Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchRead@ response operation.
--
--
--
-- /See:/ 'batchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { _broExceptionResponse ::
      !(Maybe BatchReadException),
    _broSuccessfulResponse ::
      !(Maybe BatchReadSuccessfulResponse)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchReadOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'broExceptionResponse' - Identifies which operation in a batch has failed.
--
-- * 'broSuccessfulResponse' - Identifies which operation in a batch has succeeded.
batchReadOperationResponse ::
  BatchReadOperationResponse
batchReadOperationResponse =
  BatchReadOperationResponse'
    { _broExceptionResponse = Nothing,
      _broSuccessfulResponse = Nothing
    }

-- | Identifies which operation in a batch has failed.
broExceptionResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadException)
broExceptionResponse = lens _broExceptionResponse (\s a -> s {_broExceptionResponse = a})

-- | Identifies which operation in a batch has succeeded.
broSuccessfulResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadSuccessfulResponse)
broSuccessfulResponse = lens _broSuccessfulResponse (\s a -> s {_broSuccessfulResponse = a})

instance FromJSON BatchReadOperationResponse where
  parseJSON =
    withObject
      "BatchReadOperationResponse"
      ( \x ->
          BatchReadOperationResponse'
            <$> (x .:? "ExceptionResponse") <*> (x .:? "SuccessfulResponse")
      )

instance Hashable BatchReadOperationResponse

instance NFData BatchReadOperationResponse
