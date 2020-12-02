{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementRequest where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A PartiQL batch statement request.
--
--
--
-- /See:/ 'batchStatementRequest' smart constructor.
data BatchStatementRequest = BatchStatementRequest'
  { _bsrConsistentRead ::
      !(Maybe Bool),
    _bsrParameters ::
      !(Maybe (List1 AttributeValue)),
    _bsrStatement :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStatementRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsrConsistentRead' - The read consistency of the PartiQL batch request.
--
-- * 'bsrParameters' - The parameters associated with a PartiQL statement in the batch request.
--
-- * 'bsrStatement' - A valid PartiQL statement.
batchStatementRequest ::
  -- | 'bsrStatement'
  Text ->
  BatchStatementRequest
batchStatementRequest pStatement_ =
  BatchStatementRequest'
    { _bsrConsistentRead = Nothing,
      _bsrParameters = Nothing,
      _bsrStatement = pStatement_
    }

-- | The read consistency of the PartiQL batch request.
bsrConsistentRead :: Lens' BatchStatementRequest (Maybe Bool)
bsrConsistentRead = lens _bsrConsistentRead (\s a -> s {_bsrConsistentRead = a})

-- | The parameters associated with a PartiQL statement in the batch request.
bsrParameters :: Lens' BatchStatementRequest (Maybe (NonEmpty AttributeValue))
bsrParameters = lens _bsrParameters (\s a -> s {_bsrParameters = a}) . mapping _List1

-- | A valid PartiQL statement.
bsrStatement :: Lens' BatchStatementRequest Text
bsrStatement = lens _bsrStatement (\s a -> s {_bsrStatement = a})

instance Hashable BatchStatementRequest

instance NFData BatchStatementRequest

instance ToJSON BatchStatementRequest where
  toJSON BatchStatementRequest' {..} =
    object
      ( catMaybes
          [ ("ConsistentRead" .=) <$> _bsrConsistentRead,
            ("Parameters" .=) <$> _bsrParameters,
            Just ("Statement" .= _bsrStatement)
          ]
      )
