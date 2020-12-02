{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadException where

import Network.AWS.CloudDirectory.Types.BatchReadExceptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The batch read exception structure, which contains the exception type and message.
--
--
--
-- /See:/ 'batchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { _breType ::
      !(Maybe BatchReadExceptionType),
    _breMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchReadException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breType' - A type of exception, such as @InvalidArnException@ .
--
-- * 'breMessage' - An exception message that is associated with the failure.
batchReadException ::
  BatchReadException
batchReadException =
  BatchReadException' {_breType = Nothing, _breMessage = Nothing}

-- | A type of exception, such as @InvalidArnException@ .
breType :: Lens' BatchReadException (Maybe BatchReadExceptionType)
breType = lens _breType (\s a -> s {_breType = a})

-- | An exception message that is associated with the failure.
breMessage :: Lens' BatchReadException (Maybe Text)
breMessage = lens _breMessage (\s a -> s {_breMessage = a})

instance FromJSON BatchReadException where
  parseJSON =
    withObject
      "BatchReadException"
      ( \x ->
          BatchReadException' <$> (x .:? "Type") <*> (x .:? "Message")
      )

instance Hashable BatchReadException

instance NFData BatchReadException
