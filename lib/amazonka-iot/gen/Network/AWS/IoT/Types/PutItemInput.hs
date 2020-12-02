{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutItemInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.
--
--
--
-- /See:/ 'putItemInput' smart constructor.
newtype PutItemInput = PutItemInput' {_piiTableName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutItemInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piiTableName' - The table where the message data will be written.
putItemInput ::
  -- | 'piiTableName'
  Text ->
  PutItemInput
putItemInput pTableName_ =
  PutItemInput' {_piiTableName = pTableName_}

-- | The table where the message data will be written.
piiTableName :: Lens' PutItemInput Text
piiTableName = lens _piiTableName (\s a -> s {_piiTableName = a})

instance FromJSON PutItemInput where
  parseJSON =
    withObject
      "PutItemInput"
      (\x -> PutItemInput' <$> (x .: "tableName"))

instance Hashable PutItemInput

instance NFData PutItemInput

instance ToJSON PutItemInput where
  toJSON PutItemInput' {..} =
    object (catMaybes [Just ("tableName" .= _piiTableName)])
