{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SourceTableDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SourceTableDetails where

import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of the table when the backup was created.
--
--
--
-- /See:/ 'sourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { _stdTableSizeBytes ::
      !(Maybe Integer),
    _stdTableARN :: !(Maybe Text),
    _stdBillingMode :: !(Maybe BillingMode),
    _stdItemCount :: !(Maybe Nat),
    _stdTableName :: !Text,
    _stdTableId :: !Text,
    _stdKeySchema :: !(List1 KeySchemaElement),
    _stdTableCreationDateTime :: !POSIX,
    _stdProvisionedThroughput :: !ProvisionedThroughput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceTableDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdTableSizeBytes' - Size of the table in bytes. Note that this is an approximate value.
--
-- * 'stdTableARN' - ARN of the table for which backup was created.
--
-- * 'stdBillingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
--
-- * 'stdItemCount' - Number of items in the table. Note that this is an approximate value.
--
-- * 'stdTableName' - The name of the table for which the backup was created.
--
-- * 'stdTableId' - Unique identifier for the table for which the backup was created.
--
-- * 'stdKeySchema' - Schema of the table.
--
-- * 'stdTableCreationDateTime' - Time when the source table was created.
--
-- * 'stdProvisionedThroughput' - Read IOPs and Write IOPS on the table when the backup was created.
sourceTableDetails ::
  -- | 'stdTableName'
  Text ->
  -- | 'stdTableId'
  Text ->
  -- | 'stdKeySchema'
  NonEmpty KeySchemaElement ->
  -- | 'stdTableCreationDateTime'
  UTCTime ->
  -- | 'stdProvisionedThroughput'
  ProvisionedThroughput ->
  SourceTableDetails
sourceTableDetails
  pTableName_
  pTableId_
  pKeySchema_
  pTableCreationDateTime_
  pProvisionedThroughput_ =
    SourceTableDetails'
      { _stdTableSizeBytes = Nothing,
        _stdTableARN = Nothing,
        _stdBillingMode = Nothing,
        _stdItemCount = Nothing,
        _stdTableName = pTableName_,
        _stdTableId = pTableId_,
        _stdKeySchema = _List1 # pKeySchema_,
        _stdTableCreationDateTime = _Time # pTableCreationDateTime_,
        _stdProvisionedThroughput = pProvisionedThroughput_
      }

-- | Size of the table in bytes. Note that this is an approximate value.
stdTableSizeBytes :: Lens' SourceTableDetails (Maybe Integer)
stdTableSizeBytes = lens _stdTableSizeBytes (\s a -> s {_stdTableSizeBytes = a})

-- | ARN of the table for which backup was created.
stdTableARN :: Lens' SourceTableDetails (Maybe Text)
stdTableARN = lens _stdTableARN (\s a -> s {_stdTableARN = a})

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
stdBillingMode :: Lens' SourceTableDetails (Maybe BillingMode)
stdBillingMode = lens _stdBillingMode (\s a -> s {_stdBillingMode = a})

-- | Number of items in the table. Note that this is an approximate value.
stdItemCount :: Lens' SourceTableDetails (Maybe Natural)
stdItemCount = lens _stdItemCount (\s a -> s {_stdItemCount = a}) . mapping _Nat

-- | The name of the table for which the backup was created.
stdTableName :: Lens' SourceTableDetails Text
stdTableName = lens _stdTableName (\s a -> s {_stdTableName = a})

-- | Unique identifier for the table for which the backup was created.
stdTableId :: Lens' SourceTableDetails Text
stdTableId = lens _stdTableId (\s a -> s {_stdTableId = a})

-- | Schema of the table.
stdKeySchema :: Lens' SourceTableDetails (NonEmpty KeySchemaElement)
stdKeySchema = lens _stdKeySchema (\s a -> s {_stdKeySchema = a}) . _List1

-- | Time when the source table was created.
stdTableCreationDateTime :: Lens' SourceTableDetails UTCTime
stdTableCreationDateTime = lens _stdTableCreationDateTime (\s a -> s {_stdTableCreationDateTime = a}) . _Time

-- | Read IOPs and Write IOPS on the table when the backup was created.
stdProvisionedThroughput :: Lens' SourceTableDetails ProvisionedThroughput
stdProvisionedThroughput = lens _stdProvisionedThroughput (\s a -> s {_stdProvisionedThroughput = a})

instance FromJSON SourceTableDetails where
  parseJSON =
    withObject
      "SourceTableDetails"
      ( \x ->
          SourceTableDetails'
            <$> (x .:? "TableSizeBytes")
            <*> (x .:? "TableArn")
            <*> (x .:? "BillingMode")
            <*> (x .:? "ItemCount")
            <*> (x .: "TableName")
            <*> (x .: "TableId")
            <*> (x .: "KeySchema")
            <*> (x .: "TableCreationDateTime")
            <*> (x .: "ProvisionedThroughput")
      )

instance Hashable SourceTableDetails

instance NFData SourceTableDetails
