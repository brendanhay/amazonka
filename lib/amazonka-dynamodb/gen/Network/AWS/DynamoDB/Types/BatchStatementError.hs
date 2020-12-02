{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementError where

import Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An error associated with a statement in a PartiQL batch that was run.
--
--
--
-- /See:/ 'batchStatementError' smart constructor.
data BatchStatementError = BatchStatementError'
  { _bseCode ::
      !(Maybe BatchStatementErrorCodeEnum),
    _bseMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStatementError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bseCode' - The error code associated with the failed PartiQL batch statement.
--
-- * 'bseMessage' - The error message associated with the PartiQL batch resposne.
batchStatementError ::
  BatchStatementError
batchStatementError =
  BatchStatementError' {_bseCode = Nothing, _bseMessage = Nothing}

-- | The error code associated with the failed PartiQL batch statement.
bseCode :: Lens' BatchStatementError (Maybe BatchStatementErrorCodeEnum)
bseCode = lens _bseCode (\s a -> s {_bseCode = a})

-- | The error message associated with the PartiQL batch resposne.
bseMessage :: Lens' BatchStatementError (Maybe Text)
bseMessage = lens _bseMessage (\s a -> s {_bseMessage = a})

instance FromJSON BatchStatementError where
  parseJSON =
    withObject
      "BatchStatementError"
      ( \x ->
          BatchStatementError' <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable BatchStatementError

instance NFData BatchStatementError
