{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedQueryExecutionId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.UnprocessedQueryExecutionId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a query execution that failed to process.
--
--
--
-- /See:/ 'unprocessedQueryExecutionId' smart constructor.
data UnprocessedQueryExecutionId = UnprocessedQueryExecutionId'
  { _uqeiErrorCode ::
      !(Maybe Text),
    _uqeiQueryExecutionId ::
      !(Maybe Text),
    _uqeiErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedQueryExecutionId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqeiErrorCode' - The error code returned when the query execution failed to process, if applicable.
--
-- * 'uqeiQueryExecutionId' - The unique identifier of the query execution.
--
-- * 'uqeiErrorMessage' - The error message returned when the query execution failed to process, if applicable.
unprocessedQueryExecutionId ::
  UnprocessedQueryExecutionId
unprocessedQueryExecutionId =
  UnprocessedQueryExecutionId'
    { _uqeiErrorCode = Nothing,
      _uqeiQueryExecutionId = Nothing,
      _uqeiErrorMessage = Nothing
    }

-- | The error code returned when the query execution failed to process, if applicable.
uqeiErrorCode :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiErrorCode = lens _uqeiErrorCode (\s a -> s {_uqeiErrorCode = a})

-- | The unique identifier of the query execution.
uqeiQueryExecutionId :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiQueryExecutionId = lens _uqeiQueryExecutionId (\s a -> s {_uqeiQueryExecutionId = a})

-- | The error message returned when the query execution failed to process, if applicable.
uqeiErrorMessage :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiErrorMessage = lens _uqeiErrorMessage (\s a -> s {_uqeiErrorMessage = a})

instance FromJSON UnprocessedQueryExecutionId where
  parseJSON =
    withObject
      "UnprocessedQueryExecutionId"
      ( \x ->
          UnprocessedQueryExecutionId'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "QueryExecutionId")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable UnprocessedQueryExecutionId

instance NFData UnprocessedQueryExecutionId
