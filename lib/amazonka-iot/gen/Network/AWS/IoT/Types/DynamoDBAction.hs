{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoDBAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamoDBAction where

import Network.AWS.IoT.Types.DynamoKeyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to write to a DynamoDB table.
--
--
-- The @tableName@ , @hashKeyField@ , and @rangeKeyField@ values must match the values used when you created the table.
--
-- The @hashKeyValue@ and @rangeKeyvalue@ fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${/sql-expression/ }.
--
-- You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:
--
-- @"hashKeyValue": "${topic(3)}"@
--
-- The following field uses the timestamp:
--
-- @"rangeKeyValue": "${timestamp()}"@
--
--
-- /See:/ 'dynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
  { _ddbaHashKeyType ::
      !(Maybe DynamoKeyType),
    _ddbaOperation :: !(Maybe Text),
    _ddbaRangeKeyType :: !(Maybe DynamoKeyType),
    _ddbaPayloadField :: !(Maybe Text),
    _ddbaRangeKeyField :: !(Maybe Text),
    _ddbaRangeKeyValue :: !(Maybe Text),
    _ddbaTableName :: !Text,
    _ddbaRoleARN :: !Text,
    _ddbaHashKeyField :: !Text,
    _ddbaHashKeyValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DynamoDBAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbaHashKeyType' - The hash key type. Valid values are "STRING" or "NUMBER"
--
-- * 'ddbaOperation' - The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
--
-- * 'ddbaRangeKeyType' - The range key type. Valid values are "STRING" or "NUMBER"
--
-- * 'ddbaPayloadField' - The action payload. This name can be customized.
--
-- * 'ddbaRangeKeyField' - The range key name.
--
-- * 'ddbaRangeKeyValue' - The range key value.
--
-- * 'ddbaTableName' - The name of the DynamoDB table.
--
-- * 'ddbaRoleARN' - The ARN of the IAM role that grants access to the DynamoDB table.
--
-- * 'ddbaHashKeyField' - The hash key name.
--
-- * 'ddbaHashKeyValue' - The hash key value.
dynamoDBAction ::
  -- | 'ddbaTableName'
  Text ->
  -- | 'ddbaRoleARN'
  Text ->
  -- | 'ddbaHashKeyField'
  Text ->
  -- | 'ddbaHashKeyValue'
  Text ->
  DynamoDBAction
dynamoDBAction pTableName_ pRoleARN_ pHashKeyField_ pHashKeyValue_ =
  DynamoDBAction'
    { _ddbaHashKeyType = Nothing,
      _ddbaOperation = Nothing,
      _ddbaRangeKeyType = Nothing,
      _ddbaPayloadField = Nothing,
      _ddbaRangeKeyField = Nothing,
      _ddbaRangeKeyValue = Nothing,
      _ddbaTableName = pTableName_,
      _ddbaRoleARN = pRoleARN_,
      _ddbaHashKeyField = pHashKeyField_,
      _ddbaHashKeyValue = pHashKeyValue_
    }

-- | The hash key type. Valid values are "STRING" or "NUMBER"
ddbaHashKeyType :: Lens' DynamoDBAction (Maybe DynamoKeyType)
ddbaHashKeyType = lens _ddbaHashKeyType (\s a -> s {_ddbaHashKeyType = a})

-- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
ddbaOperation :: Lens' DynamoDBAction (Maybe Text)
ddbaOperation = lens _ddbaOperation (\s a -> s {_ddbaOperation = a})

-- | The range key type. Valid values are "STRING" or "NUMBER"
ddbaRangeKeyType :: Lens' DynamoDBAction (Maybe DynamoKeyType)
ddbaRangeKeyType = lens _ddbaRangeKeyType (\s a -> s {_ddbaRangeKeyType = a})

-- | The action payload. This name can be customized.
ddbaPayloadField :: Lens' DynamoDBAction (Maybe Text)
ddbaPayloadField = lens _ddbaPayloadField (\s a -> s {_ddbaPayloadField = a})

-- | The range key name.
ddbaRangeKeyField :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyField = lens _ddbaRangeKeyField (\s a -> s {_ddbaRangeKeyField = a})

-- | The range key value.
ddbaRangeKeyValue :: Lens' DynamoDBAction (Maybe Text)
ddbaRangeKeyValue = lens _ddbaRangeKeyValue (\s a -> s {_ddbaRangeKeyValue = a})

-- | The name of the DynamoDB table.
ddbaTableName :: Lens' DynamoDBAction Text
ddbaTableName = lens _ddbaTableName (\s a -> s {_ddbaTableName = a})

-- | The ARN of the IAM role that grants access to the DynamoDB table.
ddbaRoleARN :: Lens' DynamoDBAction Text
ddbaRoleARN = lens _ddbaRoleARN (\s a -> s {_ddbaRoleARN = a})

-- | The hash key name.
ddbaHashKeyField :: Lens' DynamoDBAction Text
ddbaHashKeyField = lens _ddbaHashKeyField (\s a -> s {_ddbaHashKeyField = a})

-- | The hash key value.
ddbaHashKeyValue :: Lens' DynamoDBAction Text
ddbaHashKeyValue = lens _ddbaHashKeyValue (\s a -> s {_ddbaHashKeyValue = a})

instance FromJSON DynamoDBAction where
  parseJSON =
    withObject
      "DynamoDBAction"
      ( \x ->
          DynamoDBAction'
            <$> (x .:? "hashKeyType")
            <*> (x .:? "operation")
            <*> (x .:? "rangeKeyType")
            <*> (x .:? "payloadField")
            <*> (x .:? "rangeKeyField")
            <*> (x .:? "rangeKeyValue")
            <*> (x .: "tableName")
            <*> (x .: "roleArn")
            <*> (x .: "hashKeyField")
            <*> (x .: "hashKeyValue")
      )

instance Hashable DynamoDBAction

instance NFData DynamoDBAction

instance ToJSON DynamoDBAction where
  toJSON DynamoDBAction' {..} =
    object
      ( catMaybes
          [ ("hashKeyType" .=) <$> _ddbaHashKeyType,
            ("operation" .=) <$> _ddbaOperation,
            ("rangeKeyType" .=) <$> _ddbaRangeKeyType,
            ("payloadField" .=) <$> _ddbaPayloadField,
            ("rangeKeyField" .=) <$> _ddbaRangeKeyField,
            ("rangeKeyValue" .=) <$> _ddbaRangeKeyValue,
            Just ("tableName" .= _ddbaTableName),
            Just ("roleArn" .= _ddbaRoleARN),
            Just ("hashKeyField" .= _ddbaHashKeyField),
            Just ("hashKeyValue" .= _ddbaHashKeyValue)
          ]
      )
