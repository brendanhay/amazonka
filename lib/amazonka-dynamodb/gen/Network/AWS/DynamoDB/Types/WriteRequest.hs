{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.WriteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.WriteRequest where

import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.PutRequest
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@ . You can only request one of these operations, not both, in a single @WriteRequest@ . If you do need to perform both of these operations, you need to provide two separate @WriteRequest@ objects.
--
--
--
-- /See:/ 'writeRequest' smart constructor.
data WriteRequest = WriteRequest'
  { _wrDeleteRequest ::
      !(Maybe DeleteRequest),
    _wrPutRequest :: !(Maybe PutRequest)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WriteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrDeleteRequest' - A request to perform a @DeleteItem@ operation.
--
-- * 'wrPutRequest' - A request to perform a @PutItem@ operation.
writeRequest ::
  WriteRequest
writeRequest =
  WriteRequest'
    { _wrDeleteRequest = Nothing,
      _wrPutRequest = Nothing
    }

-- | A request to perform a @DeleteItem@ operation.
wrDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wrDeleteRequest = lens _wrDeleteRequest (\s a -> s {_wrDeleteRequest = a})

-- | A request to perform a @PutItem@ operation.
wrPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wrPutRequest = lens _wrPutRequest (\s a -> s {_wrPutRequest = a})

instance FromJSON WriteRequest where
  parseJSON =
    withObject
      "WriteRequest"
      ( \x ->
          WriteRequest' <$> (x .:? "DeleteRequest") <*> (x .:? "PutRequest")
      )

instance Hashable WriteRequest

instance NFData WriteRequest

instance ToJSON WriteRequest where
  toJSON WriteRequest' {..} =
    object
      ( catMaybes
          [ ("DeleteRequest" .=) <$> _wrDeleteRequest,
            ("PutRequest" .=) <$> _wrPutRequest
          ]
      )
