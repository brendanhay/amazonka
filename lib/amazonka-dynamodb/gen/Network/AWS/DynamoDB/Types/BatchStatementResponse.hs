{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementResponse where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.BatchStatementError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A PartiQL batch statement response..
--
--
--
-- /See:/ 'batchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { _bError ::
      !(Maybe BatchStatementError),
    _bItem ::
      !(Maybe (Map Text (AttributeValue))),
    _bTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStatementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bError' - The error associated with a failed PartiQL batch statement.
--
-- * 'bItem' - A DynamoDB item associated with a BatchStatementResponse
--
-- * 'bTableName' - The table name associated with a failed PartiQL batch statement.
batchStatementResponse ::
  BatchStatementResponse
batchStatementResponse =
  BatchStatementResponse'
    { _bError = Nothing,
      _bItem = Nothing,
      _bTableName = Nothing
    }

-- | The error associated with a failed PartiQL batch statement.
bError :: Lens' BatchStatementResponse (Maybe BatchStatementError)
bError = lens _bError (\s a -> s {_bError = a})

-- | A DynamoDB item associated with a BatchStatementResponse
bItem :: Lens' BatchStatementResponse (HashMap Text (AttributeValue))
bItem = lens _bItem (\s a -> s {_bItem = a}) . _Default . _Map

-- | The table name associated with a failed PartiQL batch statement.
bTableName :: Lens' BatchStatementResponse (Maybe Text)
bTableName = lens _bTableName (\s a -> s {_bTableName = a})

instance FromJSON BatchStatementResponse where
  parseJSON =
    withObject
      "BatchStatementResponse"
      ( \x ->
          BatchStatementResponse'
            <$> (x .:? "Error")
            <*> (x .:? "Item" .!= mempty)
            <*> (x .:? "TableName")
      )

instance Hashable BatchStatementResponse

instance NFData BatchStatementResponse
