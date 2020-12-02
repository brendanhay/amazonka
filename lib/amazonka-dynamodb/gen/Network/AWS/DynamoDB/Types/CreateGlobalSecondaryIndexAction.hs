{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a new global secondary index to be added to an existing table.
--
--
--
-- /See:/ 'createGlobalSecondaryIndexAction' smart constructor.
data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction'
  { _cgsiaProvisionedThroughput ::
      !( Maybe
           ProvisionedThroughput
       ),
    _cgsiaIndexName :: !Text,
    _cgsiaKeySchema ::
      !(List1 KeySchemaElement),
    _cgsiaProjection ::
      !Projection
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsiaProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'cgsiaIndexName' - The name of the global secondary index to be created.
--
-- * 'cgsiaKeySchema' - The key schema for the global secondary index.
--
-- * 'cgsiaProjection' - Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
createGlobalSecondaryIndexAction ::
  -- | 'cgsiaIndexName'
  Text ->
  -- | 'cgsiaKeySchema'
  NonEmpty KeySchemaElement ->
  -- | 'cgsiaProjection'
  Projection ->
  CreateGlobalSecondaryIndexAction
createGlobalSecondaryIndexAction
  pIndexName_
  pKeySchema_
  pProjection_ =
    CreateGlobalSecondaryIndexAction'
      { _cgsiaProvisionedThroughput =
          Nothing,
        _cgsiaIndexName = pIndexName_,
        _cgsiaKeySchema = _List1 # pKeySchema_,
        _cgsiaProjection = pProjection_
      }

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
cgsiaProvisionedThroughput :: Lens' CreateGlobalSecondaryIndexAction (Maybe ProvisionedThroughput)
cgsiaProvisionedThroughput = lens _cgsiaProvisionedThroughput (\s a -> s {_cgsiaProvisionedThroughput = a})

-- | The name of the global secondary index to be created.
cgsiaIndexName :: Lens' CreateGlobalSecondaryIndexAction Text
cgsiaIndexName = lens _cgsiaIndexName (\s a -> s {_cgsiaIndexName = a})

-- | The key schema for the global secondary index.
cgsiaKeySchema :: Lens' CreateGlobalSecondaryIndexAction (NonEmpty KeySchemaElement)
cgsiaKeySchema = lens _cgsiaKeySchema (\s a -> s {_cgsiaKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
cgsiaProjection :: Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = lens _cgsiaProjection (\s a -> s {_cgsiaProjection = a})

instance Hashable CreateGlobalSecondaryIndexAction

instance NFData CreateGlobalSecondaryIndexAction

instance ToJSON CreateGlobalSecondaryIndexAction where
  toJSON CreateGlobalSecondaryIndexAction' {..} =
    object
      ( catMaybes
          [ ("ProvisionedThroughput" .=) <$> _cgsiaProvisionedThroughput,
            Just ("IndexName" .= _cgsiaIndexName),
            Just ("KeySchema" .= _cgsiaKeySchema),
            Just ("Projection" .= _cgsiaProjection)
          ]
      )
