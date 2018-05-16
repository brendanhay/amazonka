{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Product where

import Network.AWS.DynamoDB.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an attribute for describing the key schema for the table and indexes.
--
--
--
-- /See:/ 'attributeDefinition' smart constructor.
data AttributeDefinition = AttributeDefinition'
  { _adAttributeName :: !Text
  , _adAttributeType :: !ScalarAttributeType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttributeName' - A name for the attribute.
--
-- * 'adAttributeType' - The data type for the attribute, where:     * @S@ - the attribute is of type String     * @N@ - the attribute is of type Number     * @B@ - the attribute is of type Binary
attributeDefinition
    :: Text -- ^ 'adAttributeName'
    -> ScalarAttributeType -- ^ 'adAttributeType'
    -> AttributeDefinition
attributeDefinition pAttributeName_ pAttributeType_ =
  AttributeDefinition'
    {_adAttributeName = pAttributeName_, _adAttributeType = pAttributeType_}


-- | A name for the attribute.
adAttributeName :: Lens' AttributeDefinition Text
adAttributeName = lens _adAttributeName (\ s a -> s{_adAttributeName = a})

-- | The data type for the attribute, where:     * @S@ - the attribute is of type String     * @N@ - the attribute is of type Number     * @B@ - the attribute is of type Binary
adAttributeType :: Lens' AttributeDefinition ScalarAttributeType
adAttributeType = lens _adAttributeType (\ s a -> s{_adAttributeType = a})

instance FromJSON AttributeDefinition where
        parseJSON
          = withObject "AttributeDefinition"
              (\ x ->
                 AttributeDefinition' <$>
                   (x .: "AttributeName") <*> (x .: "AttributeType"))

instance Hashable AttributeDefinition where

instance NFData AttributeDefinition where

instance ToJSON AttributeDefinition where
        toJSON AttributeDefinition'{..}
          = object
              (catMaybes
                 [Just ("AttributeName" .= _adAttributeName),
                  Just ("AttributeType" .= _adAttributeType)])

-- | Represents the data for an attribute.
--
--
-- Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself.
--
-- For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
--
-- /See:/ 'attributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { _avL    :: !(Maybe [AttributeValue])
  , _avNS   :: !(Maybe [Text])
  , _avM    :: !(Maybe (Map Text AttributeValue))
  , _avNULL :: !(Maybe Bool)
  , _avN    :: !(Maybe Text)
  , _avBS   :: !(Maybe [Base64])
  , _avB    :: !(Maybe Base64)
  , _avSS   :: !(Maybe [Text])
  , _avS    :: !(Maybe Text)
  , _avBOOL :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avL' - An attribute of type List. For example: @"L": ["Cookies", "Coffee", 3.14159]@
--
-- * 'avNS' - An attribute of type Number Set. For example: @"NS": ["42.2", "-19", "7.5", "3.14"]@  Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- * 'avM' - An attribute of type Map. For example: @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
--
-- * 'avNULL' - An attribute of type Null. For example: @"NULL": true@
--
-- * 'avN' - An attribute of type Number. For example: @"N": "123.45"@  Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- * 'avBS' - An attribute of type Binary Set. For example: @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
--
-- * 'avB' - An attribute of type Binary. For example: @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'avSS' - An attribute of type String Set. For example: @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
--
-- * 'avS' - An attribute of type String. For example: @"S": "Hello"@
--
-- * 'avBOOL' - An attribute of type Boolean. For example: @"BOOL": true@
attributeValue
    :: AttributeValue
attributeValue =
  AttributeValue'
    { _avL = Nothing
    , _avNS = Nothing
    , _avM = Nothing
    , _avNULL = Nothing
    , _avN = Nothing
    , _avBS = Nothing
    , _avB = Nothing
    , _avSS = Nothing
    , _avS = Nothing
    , _avBOOL = Nothing
    }


-- | An attribute of type List. For example: @"L": ["Cookies", "Coffee", 3.14159]@
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\ s a -> s{_avL = a}) . _Default . _Coerce

-- | An attribute of type Number Set. For example: @"NS": ["42.2", "-19", "7.5", "3.14"]@  Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\ s a -> s{_avNS = a}) . _Default . _Coerce

-- | An attribute of type Map. For example: @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\ s a -> s{_avM = a}) . _Default . _Map

-- | An attribute of type Null. For example: @"NULL": true@
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\ s a -> s{_avNULL = a})

-- | An attribute of type Number. For example: @"N": "123.45"@  Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\ s a -> s{_avN = a})

-- | An attribute of type Binary Set. For example: @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
avBS :: Lens' AttributeValue [ByteString]
avBS = lens _avBS (\ s a -> s{_avBS = a}) . _Default . _Coerce

-- | An attribute of type Binary. For example: @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
avB :: Lens' AttributeValue (Maybe ByteString)
avB = lens _avB (\ s a -> s{_avB = a}) . mapping _Base64

-- | An attribute of type String Set. For example: @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\ s a -> s{_avSS = a}) . _Default . _Coerce

-- | An attribute of type String. For example: @"S": "Hello"@
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a})

-- | An attribute of type Boolean. For example: @"BOOL": true@
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\ s a -> s{_avBOOL = a})

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "L" .!= mempty) <*> (x .:? "NS" .!= mempty)
                     <*> (x .:? "M" .!= mempty)
                     <*> (x .:? "NULL")
                     <*> (x .:? "N")
                     <*> (x .:? "BS" .!= mempty)
                     <*> (x .:? "B")
                     <*> (x .:? "SS" .!= mempty)
                     <*> (x .:? "S")
                     <*> (x .:? "BOOL"))

instance Hashable AttributeValue where

instance NFData AttributeValue where

instance ToJSON AttributeValue where
        toJSON AttributeValue'{..}
          = object
              (catMaybes
                 [("L" .=) <$> _avL, ("NS" .=) <$> _avNS,
                  ("M" .=) <$> _avM, ("NULL" .=) <$> _avNULL,
                  ("N" .=) <$> _avN, ("BS" .=) <$> _avBS,
                  ("B" .=) <$> _avB, ("SS" .=) <$> _avSS,
                  ("S" .=) <$> _avS, ("BOOL" .=) <$> _avBOOL])

-- | For the @UpdateItem@ operation, represents the attributes to be modified, the action to perform on each, and the new value for each.
--
--
-- Attribute values cannot be null; string and binary type attributes must have lengths greater than zero; and set type attributes must not be empty. Requests with empty values will be rejected with a @ValidationException@ exception.
--
--
-- /See:/ 'attributeValueUpdate' smart constructor.
data AttributeValueUpdate = AttributeValueUpdate'
  { _avuValue  :: !(Maybe AttributeValue)
  , _avuAction :: !(Maybe AttributeAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValueUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avuValue' - Represents the data for an attribute. Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'avuAction' - Specifies how to perform the update. Valid values are @PUT@ (default), @DELETE@ , and @ADD@ . The behavior depends on whether the specified primary key already exists in the table. __If an item with the specified /Key/ is found in the table:__      * @PUT@ - Adds the specified attribute to the item. If the attribute already exists, it is replaced by the new value.      * @DELETE@ - If no value is specified, the attribute and its value are removed from the item. The data type of the specified value must match the existing value's data type. If a /set/ of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specified @[a,c]@ , then the final attribute value would be @[b]@ . Specifying an empty set is an error.     * @ADD@ - If the attribute does not already exist, then the attribute and its values are added to the item. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:     * If the existing attribute is a number, and if @Value@ is also a number, then the @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.     * If the existing data type is a set, and if the @Value@ is also a set, then the @Value@ is added to the existing set. (This is a /set/ operation, not mathematical addition.) For example, if the attribute value was the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value would be @[1,2,3]@ . An error occurs if an Add action is specified for a set attribute and the attribute type specified does not match the existing set type.  Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings. The same holds true for number sets and binary sets. This action is only valid for an existing attribute whose data type is number or is a set. Do not use @ADD@ for any other data types. __If no item with the specified /Key/ is found:__      * @PUT@ - DynamoDB creates a new item with the specified primary key, and then adds the attribute.      * @DELETE@ - Nothing happens; there is no attribute to delete.     * @ADD@ - DynamoDB creates an item with the supplied primary key and number (or set of numbers) for the attribute value. The only data types allowed are number and number set; no other data types can be specified.
attributeValueUpdate
    :: AttributeValueUpdate
attributeValueUpdate =
  AttributeValueUpdate' {_avuValue = Nothing, _avuAction = Nothing}


-- | Represents the data for an attribute. Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
avuValue :: Lens' AttributeValueUpdate (Maybe AttributeValue)
avuValue = lens _avuValue (\ s a -> s{_avuValue = a})

-- | Specifies how to perform the update. Valid values are @PUT@ (default), @DELETE@ , and @ADD@ . The behavior depends on whether the specified primary key already exists in the table. __If an item with the specified /Key/ is found in the table:__      * @PUT@ - Adds the specified attribute to the item. If the attribute already exists, it is replaced by the new value.      * @DELETE@ - If no value is specified, the attribute and its value are removed from the item. The data type of the specified value must match the existing value's data type. If a /set/ of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specified @[a,c]@ , then the final attribute value would be @[b]@ . Specifying an empty set is an error.     * @ADD@ - If the attribute does not already exist, then the attribute and its values are added to the item. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:     * If the existing attribute is a number, and if @Value@ is also a number, then the @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.     * If the existing data type is a set, and if the @Value@ is also a set, then the @Value@ is added to the existing set. (This is a /set/ operation, not mathematical addition.) For example, if the attribute value was the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value would be @[1,2,3]@ . An error occurs if an Add action is specified for a set attribute and the attribute type specified does not match the existing set type.  Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings. The same holds true for number sets and binary sets. This action is only valid for an existing attribute whose data type is number or is a set. Do not use @ADD@ for any other data types. __If no item with the specified /Key/ is found:__      * @PUT@ - DynamoDB creates a new item with the specified primary key, and then adds the attribute.      * @DELETE@ - Nothing happens; there is no attribute to delete.     * @ADD@ - DynamoDB creates an item with the supplied primary key and number (or set of numbers) for the attribute value. The only data types allowed are number and number set; no other data types can be specified.
avuAction :: Lens' AttributeValueUpdate (Maybe AttributeAction)
avuAction = lens _avuAction (\ s a -> s{_avuAction = a})

instance Hashable AttributeValueUpdate where

instance NFData AttributeValueUpdate where

instance ToJSON AttributeValueUpdate where
        toJSON AttributeValueUpdate'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _avuValue,
                  ("Action" .=) <$> _avuAction])

-- | Contains the description of the backup created for the table.
--
--
--
-- /See:/ 'backupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { _bdBackupDetails             :: !(Maybe BackupDetails)
  , _bdSourceTableDetails        :: !(Maybe SourceTableDetails)
  , _bdSourceTableFeatureDetails :: !(Maybe SourceTableFeatureDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBackupDetails' - Contains the details of the backup created for the table.
--
-- * 'bdSourceTableDetails' - Contains the details of the table when the backup was created.
--
-- * 'bdSourceTableFeatureDetails' - Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
backupDescription
    :: BackupDescription
backupDescription =
  BackupDescription'
    { _bdBackupDetails = Nothing
    , _bdSourceTableDetails = Nothing
    , _bdSourceTableFeatureDetails = Nothing
    }


-- | Contains the details of the backup created for the table.
bdBackupDetails :: Lens' BackupDescription (Maybe BackupDetails)
bdBackupDetails = lens _bdBackupDetails (\ s a -> s{_bdBackupDetails = a})

-- | Contains the details of the table when the backup was created.
bdSourceTableDetails :: Lens' BackupDescription (Maybe SourceTableDetails)
bdSourceTableDetails = lens _bdSourceTableDetails (\ s a -> s{_bdSourceTableDetails = a})

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
bdSourceTableFeatureDetails :: Lens' BackupDescription (Maybe SourceTableFeatureDetails)
bdSourceTableFeatureDetails = lens _bdSourceTableFeatureDetails (\ s a -> s{_bdSourceTableFeatureDetails = a})

instance FromJSON BackupDescription where
        parseJSON
          = withObject "BackupDescription"
              (\ x ->
                 BackupDescription' <$>
                   (x .:? "BackupDetails") <*>
                     (x .:? "SourceTableDetails")
                     <*> (x .:? "SourceTableFeatureDetails"))

instance Hashable BackupDescription where

instance NFData BackupDescription where

-- | Contains the details of the backup created for the table.
--
--
--
-- /See:/ 'backupDetails' smart constructor.
data BackupDetails = BackupDetails'
  { _bdBackupSizeBytes        :: !(Maybe Nat)
  , _bdBackupARN              :: !Text
  , _bdBackupName             :: !Text
  , _bdBackupStatus           :: !BackupStatus
  , _bdBackupCreationDateTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBackupSizeBytes' - Size of the backup in bytes.
--
-- * 'bdBackupARN' - ARN associated with the backup.
--
-- * 'bdBackupName' - Name of the requested backup.
--
-- * 'bdBackupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- * 'bdBackupCreationDateTime' - Time at which the backup was created. This is the request time of the backup.
backupDetails
    :: Text -- ^ 'bdBackupARN'
    -> Text -- ^ 'bdBackupName'
    -> BackupStatus -- ^ 'bdBackupStatus'
    -> UTCTime -- ^ 'bdBackupCreationDateTime'
    -> BackupDetails
backupDetails pBackupARN_ pBackupName_ pBackupStatus_ pBackupCreationDateTime_ =
  BackupDetails'
    { _bdBackupSizeBytes = Nothing
    , _bdBackupARN = pBackupARN_
    , _bdBackupName = pBackupName_
    , _bdBackupStatus = pBackupStatus_
    , _bdBackupCreationDateTime = _Time # pBackupCreationDateTime_
    }


-- | Size of the backup in bytes.
bdBackupSizeBytes :: Lens' BackupDetails (Maybe Natural)
bdBackupSizeBytes = lens _bdBackupSizeBytes (\ s a -> s{_bdBackupSizeBytes = a}) . mapping _Nat

-- | ARN associated with the backup.
bdBackupARN :: Lens' BackupDetails Text
bdBackupARN = lens _bdBackupARN (\ s a -> s{_bdBackupARN = a})

-- | Name of the requested backup.
bdBackupName :: Lens' BackupDetails Text
bdBackupName = lens _bdBackupName (\ s a -> s{_bdBackupName = a})

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
bdBackupStatus :: Lens' BackupDetails BackupStatus
bdBackupStatus = lens _bdBackupStatus (\ s a -> s{_bdBackupStatus = a})

-- | Time at which the backup was created. This is the request time of the backup.
bdBackupCreationDateTime :: Lens' BackupDetails UTCTime
bdBackupCreationDateTime = lens _bdBackupCreationDateTime (\ s a -> s{_bdBackupCreationDateTime = a}) . _Time

instance FromJSON BackupDetails where
        parseJSON
          = withObject "BackupDetails"
              (\ x ->
                 BackupDetails' <$>
                   (x .:? "BackupSizeBytes") <*> (x .: "BackupArn") <*>
                     (x .: "BackupName")
                     <*> (x .: "BackupStatus")
                     <*> (x .: "BackupCreationDateTime"))

instance Hashable BackupDetails where

instance NFData BackupDetails where

-- | Contains details for the backup.
--
--
--
-- /See:/ 'backupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { _bsTableARN               :: !(Maybe Text)
  , _bsBackupName             :: !(Maybe Text)
  , _bsBackupStatus           :: !(Maybe BackupStatus)
  , _bsBackupSizeBytes        :: !(Maybe Nat)
  , _bsBackupARN              :: !(Maybe Text)
  , _bsTableId                :: !(Maybe Text)
  , _bsBackupCreationDateTime :: !(Maybe POSIX)
  , _bsTableName              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsTableARN' - ARN associated with the table.
--
-- * 'bsBackupName' - Name of the specified backup.
--
-- * 'bsBackupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- * 'bsBackupSizeBytes' - Size of the backup in bytes.
--
-- * 'bsBackupARN' - ARN associated with the backup.
--
-- * 'bsTableId' - Unique identifier for the table.
--
-- * 'bsBackupCreationDateTime' - Time at which the backup was created.
--
-- * 'bsTableName' - Name of the table.
backupSummary
    :: BackupSummary
backupSummary =
  BackupSummary'
    { _bsTableARN = Nothing
    , _bsBackupName = Nothing
    , _bsBackupStatus = Nothing
    , _bsBackupSizeBytes = Nothing
    , _bsBackupARN = Nothing
    , _bsTableId = Nothing
    , _bsBackupCreationDateTime = Nothing
    , _bsTableName = Nothing
    }


-- | ARN associated with the table.
bsTableARN :: Lens' BackupSummary (Maybe Text)
bsTableARN = lens _bsTableARN (\ s a -> s{_bsTableARN = a})

-- | Name of the specified backup.
bsBackupName :: Lens' BackupSummary (Maybe Text)
bsBackupName = lens _bsBackupName (\ s a -> s{_bsBackupName = a})

-- | Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
bsBackupStatus :: Lens' BackupSummary (Maybe BackupStatus)
bsBackupStatus = lens _bsBackupStatus (\ s a -> s{_bsBackupStatus = a})

-- | Size of the backup in bytes.
bsBackupSizeBytes :: Lens' BackupSummary (Maybe Natural)
bsBackupSizeBytes = lens _bsBackupSizeBytes (\ s a -> s{_bsBackupSizeBytes = a}) . mapping _Nat

-- | ARN associated with the backup.
bsBackupARN :: Lens' BackupSummary (Maybe Text)
bsBackupARN = lens _bsBackupARN (\ s a -> s{_bsBackupARN = a})

-- | Unique identifier for the table.
bsTableId :: Lens' BackupSummary (Maybe Text)
bsTableId = lens _bsTableId (\ s a -> s{_bsTableId = a})

-- | Time at which the backup was created.
bsBackupCreationDateTime :: Lens' BackupSummary (Maybe UTCTime)
bsBackupCreationDateTime = lens _bsBackupCreationDateTime (\ s a -> s{_bsBackupCreationDateTime = a}) . mapping _Time

-- | Name of the table.
bsTableName :: Lens' BackupSummary (Maybe Text)
bsTableName = lens _bsTableName (\ s a -> s{_bsTableName = a})

instance FromJSON BackupSummary where
        parseJSON
          = withObject "BackupSummary"
              (\ x ->
                 BackupSummary' <$>
                   (x .:? "TableArn") <*> (x .:? "BackupName") <*>
                     (x .:? "BackupStatus")
                     <*> (x .:? "BackupSizeBytes")
                     <*> (x .:? "BackupArn")
                     <*> (x .:? "TableId")
                     <*> (x .:? "BackupCreationDateTime")
                     <*> (x .:? "TableName"))

instance Hashable BackupSummary where

instance NFData BackupSummary where

-- | Represents the amount of provisioned throughput capacity consumed on a table or an index.
--
--
--
-- /See:/ 'capacity' smart constructor.
newtype Capacity = Capacity'
  { _cCapacityUnits :: Maybe Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Capacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCapacityUnits' - The total number of capacity units consumed on a table or an index.
capacity
    :: Capacity
capacity = Capacity' {_cCapacityUnits = Nothing}


-- | The total number of capacity units consumed on a table or an index.
cCapacityUnits :: Lens' Capacity (Maybe Double)
cCapacityUnits = lens _cCapacityUnits (\ s a -> s{_cCapacityUnits = a})

instance FromJSON Capacity where
        parseJSON
          = withObject "Capacity"
              (\ x -> Capacity' <$> (x .:? "CapacityUnits"))

instance Hashable Capacity where

instance NFData Capacity where

-- | Represents the selection criteria for a @Query@ or @Scan@ operation:
--
--
--     * For a @Query@ operation, @Condition@ is used for specifying the @KeyConditions@ to use when querying a table or an index. For @KeyConditions@ , only the following comparison operators are supported:
--
-- @EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN@
--
-- @Condition@ is also used in a @QueryFilter@ , which evaluates the query results and returns only the desired values.
--
--     * For a @Scan@ operation, @Condition@ is used in a @ScanFilter@ , which evaluates the scan results and returns only the desired values.
--
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cAttributeValueList :: !(Maybe [AttributeValue])
  , _cComparisonOperator :: !ComparisonOperator
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAttributeValueList' - One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
--
-- * 'cComparisonOperator' - A comparator for evaluating attributes. For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@  For usage examples of @AttributeValueList@ and @ComparisonOperator@ , see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
condition
    :: ComparisonOperator -- ^ 'cComparisonOperator'
    -> Condition
condition pComparisonOperator_ =
  Condition'
    { _cAttributeValueList = Nothing
    , _cComparisonOperator = pComparisonOperator_
    }


-- | One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
cAttributeValueList :: Lens' Condition [AttributeValue]
cAttributeValueList = lens _cAttributeValueList (\ s a -> s{_cAttributeValueList = a}) . _Default . _Coerce

-- | A comparator for evaluating attributes. For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@  For usage examples of @AttributeValueList@ and @ComparisonOperator@ , see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
cComparisonOperator :: Lens' Condition ComparisonOperator
cComparisonOperator = lens _cComparisonOperator (\ s a -> s{_cComparisonOperator = a})

instance Hashable Condition where

instance NFData Condition where

instance ToJSON Condition where
        toJSON Condition'{..}
          = object
              (catMaybes
                 [("AttributeValueList" .=) <$> _cAttributeValueList,
                  Just ("ComparisonOperator" .= _cComparisonOperator)])

-- | The capacity units consumed by an operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the request asked for it. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
--
--
-- /See:/ 'consumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { _ccGlobalSecondaryIndexes :: !(Maybe (Map Text Capacity))
  , _ccCapacityUnits          :: !(Maybe Double)
  , _ccLocalSecondaryIndexes  :: !(Maybe (Map Text Capacity))
  , _ccTable                  :: !(Maybe Capacity)
  , _ccTableName              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConsumedCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccGlobalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the operation.
--
-- * 'ccCapacityUnits' - The total number of capacity units consumed by the operation.
--
-- * 'ccLocalSecondaryIndexes' - The amount of throughput consumed on each local index affected by the operation.
--
-- * 'ccTable' - The amount of throughput consumed on the table affected by the operation.
--
-- * 'ccTableName' - The name of the table that was affected by the operation.
consumedCapacity
    :: ConsumedCapacity
consumedCapacity =
  ConsumedCapacity'
    { _ccGlobalSecondaryIndexes = Nothing
    , _ccCapacityUnits = Nothing
    , _ccLocalSecondaryIndexes = Nothing
    , _ccTable = Nothing
    , _ccTableName = Nothing
    }


-- | The amount of throughput consumed on each global index affected by the operation.
ccGlobalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccGlobalSecondaryIndexes = lens _ccGlobalSecondaryIndexes (\ s a -> s{_ccGlobalSecondaryIndexes = a}) . _Default . _Map

-- | The total number of capacity units consumed by the operation.
ccCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
ccCapacityUnits = lens _ccCapacityUnits (\ s a -> s{_ccCapacityUnits = a})

-- | The amount of throughput consumed on each local index affected by the operation.
ccLocalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
ccLocalSecondaryIndexes = lens _ccLocalSecondaryIndexes (\ s a -> s{_ccLocalSecondaryIndexes = a}) . _Default . _Map

-- | The amount of throughput consumed on the table affected by the operation.
ccTable :: Lens' ConsumedCapacity (Maybe Capacity)
ccTable = lens _ccTable (\ s a -> s{_ccTable = a})

-- | The name of the table that was affected by the operation.
ccTableName :: Lens' ConsumedCapacity (Maybe Text)
ccTableName = lens _ccTableName (\ s a -> s{_ccTableName = a})

instance FromJSON ConsumedCapacity where
        parseJSON
          = withObject "ConsumedCapacity"
              (\ x ->
                 ConsumedCapacity' <$>
                   (x .:? "GlobalSecondaryIndexes" .!= mempty) <*>
                     (x .:? "CapacityUnits")
                     <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "Table")
                     <*> (x .:? "TableName"))

instance Hashable ConsumedCapacity where

instance NFData ConsumedCapacity where

-- | Represents the continuous backups and point in time recovery settings on the table.
--
--
--
-- /See:/ 'continuousBackupsDescription' smart constructor.
data ContinuousBackupsDescription = ContinuousBackupsDescription'
  { _cbdPointInTimeRecoveryDescription :: !(Maybe PointInTimeRecoveryDescription)
  , _cbdContinuousBackupsStatus :: !ContinuousBackupsStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinuousBackupsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbdPointInTimeRecoveryDescription' - The description of the point in time recovery settings applied to the table.
--
-- * 'cbdContinuousBackupsStatus' - @ContinuousBackupsStatus@ can be one of the following states : ENABLED, DISABLED
continuousBackupsDescription
    :: ContinuousBackupsStatus -- ^ 'cbdContinuousBackupsStatus'
    -> ContinuousBackupsDescription
continuousBackupsDescription pContinuousBackupsStatus_ =
  ContinuousBackupsDescription'
    { _cbdPointInTimeRecoveryDescription = Nothing
    , _cbdContinuousBackupsStatus = pContinuousBackupsStatus_
    }


-- | The description of the point in time recovery settings applied to the table.
cbdPointInTimeRecoveryDescription :: Lens' ContinuousBackupsDescription (Maybe PointInTimeRecoveryDescription)
cbdPointInTimeRecoveryDescription = lens _cbdPointInTimeRecoveryDescription (\ s a -> s{_cbdPointInTimeRecoveryDescription = a})

-- | @ContinuousBackupsStatus@ can be one of the following states : ENABLED, DISABLED
cbdContinuousBackupsStatus :: Lens' ContinuousBackupsDescription ContinuousBackupsStatus
cbdContinuousBackupsStatus = lens _cbdContinuousBackupsStatus (\ s a -> s{_cbdContinuousBackupsStatus = a})

instance FromJSON ContinuousBackupsDescription where
        parseJSON
          = withObject "ContinuousBackupsDescription"
              (\ x ->
                 ContinuousBackupsDescription' <$>
                   (x .:? "PointInTimeRecoveryDescription") <*>
                     (x .: "ContinuousBackupsStatus"))

instance Hashable ContinuousBackupsDescription where

instance NFData ContinuousBackupsDescription where

-- | Represents a new global secondary index to be added to an existing table.
--
--
--
-- /See:/ 'createGlobalSecondaryIndexAction' smart constructor.
data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction'
  { _cgsiaIndexName             :: !Text
  , _cgsiaKeySchema             :: !(List1 KeySchemaElement)
  , _cgsiaProjection            :: !Projection
  , _cgsiaProvisionedThroughput :: !ProvisionedThroughput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsiaIndexName' - The name of the global secondary index to be created.
--
-- * 'cgsiaKeySchema' - The key schema for the global secondary index.
--
-- * 'cgsiaProjection' - Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'cgsiaProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
createGlobalSecondaryIndexAction
    :: Text -- ^ 'cgsiaIndexName'
    -> NonEmpty KeySchemaElement -- ^ 'cgsiaKeySchema'
    -> Projection -- ^ 'cgsiaProjection'
    -> ProvisionedThroughput -- ^ 'cgsiaProvisionedThroughput'
    -> CreateGlobalSecondaryIndexAction
createGlobalSecondaryIndexAction pIndexName_ pKeySchema_ pProjection_ pProvisionedThroughput_ =
  CreateGlobalSecondaryIndexAction'
    { _cgsiaIndexName = pIndexName_
    , _cgsiaKeySchema = _List1 # pKeySchema_
    , _cgsiaProjection = pProjection_
    , _cgsiaProvisionedThroughput = pProvisionedThroughput_
    }


-- | The name of the global secondary index to be created.
cgsiaIndexName :: Lens' CreateGlobalSecondaryIndexAction Text
cgsiaIndexName = lens _cgsiaIndexName (\ s a -> s{_cgsiaIndexName = a})

-- | The key schema for the global secondary index.
cgsiaKeySchema :: Lens' CreateGlobalSecondaryIndexAction (NonEmpty KeySchemaElement)
cgsiaKeySchema = lens _cgsiaKeySchema (\ s a -> s{_cgsiaKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
cgsiaProjection :: Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = lens _cgsiaProjection (\ s a -> s{_cgsiaProjection = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
cgsiaProvisionedThroughput :: Lens' CreateGlobalSecondaryIndexAction ProvisionedThroughput
cgsiaProvisionedThroughput = lens _cgsiaProvisionedThroughput (\ s a -> s{_cgsiaProvisionedThroughput = a})

instance Hashable CreateGlobalSecondaryIndexAction
         where

instance NFData CreateGlobalSecondaryIndexAction
         where

instance ToJSON CreateGlobalSecondaryIndexAction
         where
        toJSON CreateGlobalSecondaryIndexAction'{..}
          = object
              (catMaybes
                 [Just ("IndexName" .= _cgsiaIndexName),
                  Just ("KeySchema" .= _cgsiaKeySchema),
                  Just ("Projection" .= _cgsiaProjection),
                  Just
                    ("ProvisionedThroughput" .=
                       _cgsiaProvisionedThroughput)])

-- | Represents a replica to be added.
--
--
--
-- /See:/ 'createReplicaAction' smart constructor.
newtype CreateReplicaAction = CreateReplicaAction'
  { _craRegionName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craRegionName' - The region of the replica to be added.
createReplicaAction
    :: Text -- ^ 'craRegionName'
    -> CreateReplicaAction
createReplicaAction pRegionName_ =
  CreateReplicaAction' {_craRegionName = pRegionName_}


-- | The region of the replica to be added.
craRegionName :: Lens' CreateReplicaAction Text
craRegionName = lens _craRegionName (\ s a -> s{_craRegionName = a})

instance Hashable CreateReplicaAction where

instance NFData CreateReplicaAction where

instance ToJSON CreateReplicaAction where
        toJSON CreateReplicaAction'{..}
          = object
              (catMaybes [Just ("RegionName" .= _craRegionName)])

-- | Represents a global secondary index to be deleted from an existing table.
--
--
--
-- /See:/ 'deleteGlobalSecondaryIndexAction' smart constructor.
newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { _dgsiaIndexName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsiaIndexName' - The name of the global secondary index to be deleted.
deleteGlobalSecondaryIndexAction
    :: Text -- ^ 'dgsiaIndexName'
    -> DeleteGlobalSecondaryIndexAction
deleteGlobalSecondaryIndexAction pIndexName_ =
  DeleteGlobalSecondaryIndexAction' {_dgsiaIndexName = pIndexName_}


-- | The name of the global secondary index to be deleted.
dgsiaIndexName :: Lens' DeleteGlobalSecondaryIndexAction Text
dgsiaIndexName = lens _dgsiaIndexName (\ s a -> s{_dgsiaIndexName = a})

instance Hashable DeleteGlobalSecondaryIndexAction
         where

instance NFData DeleteGlobalSecondaryIndexAction
         where

instance ToJSON DeleteGlobalSecondaryIndexAction
         where
        toJSON DeleteGlobalSecondaryIndexAction'{..}
          = object
              (catMaybes [Just ("IndexName" .= _dgsiaIndexName)])

-- | Represents a replica to be removed.
--
--
--
-- /See:/ 'deleteReplicaAction' smart constructor.
newtype DeleteReplicaAction = DeleteReplicaAction'
  { _draRegionName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReplicaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draRegionName' - The region of the replica to be removed.
deleteReplicaAction
    :: Text -- ^ 'draRegionName'
    -> DeleteReplicaAction
deleteReplicaAction pRegionName_ =
  DeleteReplicaAction' {_draRegionName = pRegionName_}


-- | The region of the replica to be removed.
draRegionName :: Lens' DeleteReplicaAction Text
draRegionName = lens _draRegionName (\ s a -> s{_draRegionName = a})

instance Hashable DeleteReplicaAction where

instance NFData DeleteReplicaAction where

instance ToJSON DeleteReplicaAction where
        toJSON DeleteReplicaAction'{..}
          = object
              (catMaybes [Just ("RegionName" .= _draRegionName)])

-- | Represents a request to perform a @DeleteItem@ operation on an item.
--
--
--
-- /See:/ 'deleteRequest' smart constructor.
newtype DeleteRequest = DeleteRequest'
  { _drKey :: Map Text AttributeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drKey' - A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
deleteRequest
    :: DeleteRequest
deleteRequest = DeleteRequest' {_drKey = mempty}


-- | A map of attribute name to attribute values, representing the primary key of the item to delete. All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema.
drKey :: Lens' DeleteRequest (HashMap Text AttributeValue)
drKey = lens _drKey (\ s a -> s{_drKey = a}) . _Map

instance FromJSON DeleteRequest where
        parseJSON
          = withObject "DeleteRequest"
              (\ x -> DeleteRequest' <$> (x .:? "Key" .!= mempty))

instance Hashable DeleteRequest where

instance NFData DeleteRequest where

instance ToJSON DeleteRequest where
        toJSON DeleteRequest'{..}
          = object (catMaybes [Just ("Key" .= _drKey)])

-- | Represents a condition to be compared with an attribute value. This condition can be used with @DeleteItem@ , @PutItem@ or @UpdateItem@ operations; if the comparison evaluates to true, the operation succeeds; if not, the operation fails. You can use @ExpectedAttributeValue@ in one of two different ways:
--
--
--     * Use @AttributeValueList@ to specify one or more values to compare against an attribute. Use @ComparisonOperator@ to specify how you want to perform the comparison. If the comparison evaluates to true, then the conditional operation succeeds.
--
--     * Use @Value@ to specify a value that DynamoDB will compare against an attribute. If the values match, then @ExpectedAttributeValue@ evaluates to true and the conditional operation succeeds. Optionally, you can also set @Exists@ to false, indicating that you /do not/ expect to find the attribute value in the table. In this case, the conditional operation succeeds only if the comparison evaluates to false.
--
--
--
-- @Value@ and @Exists@ are incompatible with @AttributeValueList@ and @ComparisonOperator@ . Note that if you use both sets of parameters at once, DynamoDB will return a @ValidationException@ exception.
--
--
-- /See:/ 'expectedAttributeValue' smart constructor.
data ExpectedAttributeValue = ExpectedAttributeValue'
  { _eavAttributeValueList :: !(Maybe [AttributeValue])
  , _eavExists             :: !(Maybe Bool)
  , _eavValue              :: !(Maybe AttributeValue)
  , _eavComparisonOperator :: !(Maybe ComparisonOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExpectedAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eavAttributeValueList' - One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values. For information on specifying data types in JSON, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'eavExists' - Causes DynamoDB to evaluate the value before attempting a conditional operation:     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionalCheckFailedException@ .     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionalCheckFailedException@ . The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied. DynamoDB returns a @ValidationException@ if:     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
--
-- * 'eavValue' - Represents the data for the expected attribute. Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'eavComparisonOperator' - A comparator for evaluating attributes in the @AttributeValueList@ . For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@
expectedAttributeValue
    :: ExpectedAttributeValue
expectedAttributeValue =
  ExpectedAttributeValue'
    { _eavAttributeValueList = Nothing
    , _eavExists = Nothing
    , _eavValue = Nothing
    , _eavComparisonOperator = Nothing
    }


-- | One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values. For information on specifying data types in JSON, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format> in the /Amazon DynamoDB Developer Guide/ .
eavAttributeValueList :: Lens' ExpectedAttributeValue [AttributeValue]
eavAttributeValueList = lens _eavAttributeValueList (\ s a -> s{_eavAttributeValueList = a}) . _Default . _Coerce

-- | Causes DynamoDB to evaluate the value before attempting a conditional operation:     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionalCheckFailedException@ .     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionalCheckFailedException@ . The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied. DynamoDB returns a @ValidationException@ if:     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
eavExists :: Lens' ExpectedAttributeValue (Maybe Bool)
eavExists = lens _eavExists (\ s a -> s{_eavExists = a})

-- | Represents the data for the expected attribute. Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
eavValue :: Lens' ExpectedAttributeValue (Maybe AttributeValue)
eavValue = lens _eavValue (\ s a -> s{_eavValue = a})

-- | A comparator for evaluating attributes in the @AttributeValueList@ . For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@
eavComparisonOperator :: Lens' ExpectedAttributeValue (Maybe ComparisonOperator)
eavComparisonOperator = lens _eavComparisonOperator (\ s a -> s{_eavComparisonOperator = a})

instance Hashable ExpectedAttributeValue where

instance NFData ExpectedAttributeValue where

instance ToJSON ExpectedAttributeValue where
        toJSON ExpectedAttributeValue'{..}
          = object
              (catMaybes
                 [("AttributeValueList" .=) <$>
                    _eavAttributeValueList,
                  ("Exists" .=) <$> _eavExists,
                  ("Value" .=) <$> _eavValue,
                  ("ComparisonOperator" .=) <$>
                    _eavComparisonOperator])

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'globalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { _gsiIndexName             :: !Text
  , _gsiKeySchema             :: !(List1 KeySchemaElement)
  , _gsiProjection            :: !Projection
  , _gsiProvisionedThroughput :: !ProvisionedThroughput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- * 'gsiKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'gsiProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
globalSecondaryIndex
    :: Text -- ^ 'gsiIndexName'
    -> NonEmpty KeySchemaElement -- ^ 'gsiKeySchema'
    -> Projection -- ^ 'gsiProjection'
    -> ProvisionedThroughput -- ^ 'gsiProvisionedThroughput'
    -> GlobalSecondaryIndex
globalSecondaryIndex pIndexName_ pKeySchema_ pProjection_ pProvisionedThroughput_ =
  GlobalSecondaryIndex'
    { _gsiIndexName = pIndexName_
    , _gsiKeySchema = _List1 # pKeySchema_
    , _gsiProjection = pProjection_
    , _gsiProvisionedThroughput = pProvisionedThroughput_
    }


-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\ s a -> s{_gsiIndexName = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\ s a -> s{_gsiKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\ s a -> s{_gsiProjection = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex ProvisionedThroughput
gsiProvisionedThroughput = lens _gsiProvisionedThroughput (\ s a -> s{_gsiProvisionedThroughput = a})

instance Hashable GlobalSecondaryIndex where

instance NFData GlobalSecondaryIndex where

instance ToJSON GlobalSecondaryIndex where
        toJSON GlobalSecondaryIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexName" .= _gsiIndexName),
                  Just ("KeySchema" .= _gsiKeySchema),
                  Just ("Projection" .= _gsiProjection),
                  Just
                    ("ProvisionedThroughput" .=
                       _gsiProvisionedThroughput)])

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'globalSecondaryIndexDescription' smart constructor.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
  { _gsidBackfilling           :: !(Maybe Bool)
  , _gsidIndexSizeBytes        :: !(Maybe Integer)
  , _gsidIndexStatus           :: !(Maybe IndexStatus)
  , _gsidProvisionedThroughput :: !(Maybe ProvisionedThroughputDescription)
  , _gsidIndexARN              :: !(Maybe Text)
  , _gsidKeySchema             :: !(Maybe (List1 KeySchemaElement))
  , _gsidProjection            :: !(Maybe Projection)
  , _gsidItemCount             :: !(Maybe Integer)
  , _gsidIndexName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsidBackfilling' - Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
--
-- * 'gsidIndexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'gsidIndexStatus' - The current state of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
--
-- * 'gsidProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'gsidIndexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- * 'gsidKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsidProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'gsidItemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'gsidIndexName' - The name of the global secondary index.
globalSecondaryIndexDescription
    :: GlobalSecondaryIndexDescription
globalSecondaryIndexDescription =
  GlobalSecondaryIndexDescription'
    { _gsidBackfilling = Nothing
    , _gsidIndexSizeBytes = Nothing
    , _gsidIndexStatus = Nothing
    , _gsidProvisionedThroughput = Nothing
    , _gsidIndexARN = Nothing
    , _gsidKeySchema = Nothing
    , _gsidProjection = Nothing
    , _gsidItemCount = Nothing
    , _gsidIndexName = Nothing
    }


-- | Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
gsidBackfilling :: Lens' GlobalSecondaryIndexDescription (Maybe Bool)
gsidBackfilling = lens _gsidBackfilling (\ s a -> s{_gsidBackfilling = a})

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
gsidIndexSizeBytes :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidIndexSizeBytes = lens _gsidIndexSizeBytes (\ s a -> s{_gsidIndexSizeBytes = a})

-- | The current state of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.
gsidIndexStatus :: Lens' GlobalSecondaryIndexDescription (Maybe IndexStatus)
gsidIndexStatus = lens _gsidIndexStatus (\ s a -> s{_gsidIndexStatus = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
gsidProvisionedThroughput :: Lens' GlobalSecondaryIndexDescription (Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput = lens _gsidProvisionedThroughput (\ s a -> s{_gsidProvisionedThroughput = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
gsidIndexARN :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexARN = lens _gsidIndexARN (\ s a -> s{_gsidIndexARN = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsidKeySchema :: Lens' GlobalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
gsidKeySchema = lens _gsidKeySchema (\ s a -> s{_gsidKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsidProjection :: Lens' GlobalSecondaryIndexDescription (Maybe Projection)
gsidProjection = lens _gsidProjection (\ s a -> s{_gsidProjection = a})

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
gsidItemCount :: Lens' GlobalSecondaryIndexDescription (Maybe Integer)
gsidItemCount = lens _gsidItemCount (\ s a -> s{_gsidItemCount = a})

-- | The name of the global secondary index.
gsidIndexName :: Lens' GlobalSecondaryIndexDescription (Maybe Text)
gsidIndexName = lens _gsidIndexName (\ s a -> s{_gsidIndexName = a})

instance FromJSON GlobalSecondaryIndexDescription
         where
        parseJSON
          = withObject "GlobalSecondaryIndexDescription"
              (\ x ->
                 GlobalSecondaryIndexDescription' <$>
                   (x .:? "Backfilling") <*> (x .:? "IndexSizeBytes")
                     <*> (x .:? "IndexStatus")
                     <*> (x .:? "ProvisionedThroughput")
                     <*> (x .:? "IndexArn")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "Projection")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "IndexName"))

instance Hashable GlobalSecondaryIndexDescription
         where

instance NFData GlobalSecondaryIndexDescription where

-- | Represents the properties of a global secondary index for the table when the backup was created.
--
--
--
-- /See:/ 'globalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { _gsiiProvisionedThroughput :: !(Maybe ProvisionedThroughput)
  , _gsiiKeySchema             :: !(Maybe (List1 KeySchemaElement))
  , _gsiiProjection            :: !(Maybe Projection)
  , _gsiiIndexName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSecondaryIndexInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiiProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index.
--
-- * 'gsiiKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsiiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'gsiiIndexName' - The name of the global secondary index.
globalSecondaryIndexInfo
    :: GlobalSecondaryIndexInfo
globalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { _gsiiProvisionedThroughput = Nothing
    , _gsiiKeySchema = Nothing
    , _gsiiProjection = Nothing
    , _gsiiIndexName = Nothing
    }


-- | Represents the provisioned throughput settings for the specified global secondary index.
gsiiProvisionedThroughput :: Lens' GlobalSecondaryIndexInfo (Maybe ProvisionedThroughput)
gsiiProvisionedThroughput = lens _gsiiProvisionedThroughput (\ s a -> s{_gsiiProvisionedThroughput = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsiiKeySchema :: Lens' GlobalSecondaryIndexInfo (Maybe (NonEmpty KeySchemaElement))
gsiiKeySchema = lens _gsiiKeySchema (\ s a -> s{_gsiiKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsiiProjection :: Lens' GlobalSecondaryIndexInfo (Maybe Projection)
gsiiProjection = lens _gsiiProjection (\ s a -> s{_gsiiProjection = a})

-- | The name of the global secondary index.
gsiiIndexName :: Lens' GlobalSecondaryIndexInfo (Maybe Text)
gsiiIndexName = lens _gsiiIndexName (\ s a -> s{_gsiiIndexName = a})

instance FromJSON GlobalSecondaryIndexInfo where
        parseJSON
          = withObject "GlobalSecondaryIndexInfo"
              (\ x ->
                 GlobalSecondaryIndexInfo' <$>
                   (x .:? "ProvisionedThroughput") <*>
                     (x .:? "KeySchema")
                     <*> (x .:? "Projection")
                     <*> (x .:? "IndexName"))

instance Hashable GlobalSecondaryIndexInfo where

instance NFData GlobalSecondaryIndexInfo where

-- | Represents one of the following:
--
--
--     * A new global secondary index to be added to an existing table.
--
--     * New provisioned throughput parameters for an existing global secondary index.
--
--     * An existing global secondary index to be removed from an existing table.
--
--
--
--
-- /See:/ 'globalSecondaryIndexUpdate' smart constructor.
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
  { _gsiuCreate :: !(Maybe CreateGlobalSecondaryIndexAction)
  , _gsiuDelete :: !(Maybe DeleteGlobalSecondaryIndexAction)
  , _gsiuUpdate :: !(Maybe UpdateGlobalSecondaryIndexAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSecondaryIndexUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiuCreate' - The parameters required for creating a global secondary index on an existing table:     * @IndexName @      * @KeySchema @      * @AttributeDefinitions @      * @Projection @      * @ProvisionedThroughput @
--
-- * 'gsiuDelete' - The name of an existing global secondary index to be removed.
--
-- * 'gsiuUpdate' - The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
globalSecondaryIndexUpdate
    :: GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate =
  GlobalSecondaryIndexUpdate'
    {_gsiuCreate = Nothing, _gsiuDelete = Nothing, _gsiuUpdate = Nothing}


-- | The parameters required for creating a global secondary index on an existing table:     * @IndexName @      * @KeySchema @      * @AttributeDefinitions @      * @Projection @      * @ProvisionedThroughput @
gsiuCreate :: Lens' GlobalSecondaryIndexUpdate (Maybe CreateGlobalSecondaryIndexAction)
gsiuCreate = lens _gsiuCreate (\ s a -> s{_gsiuCreate = a})

-- | The name of an existing global secondary index to be removed.
gsiuDelete :: Lens' GlobalSecondaryIndexUpdate (Maybe DeleteGlobalSecondaryIndexAction)
gsiuDelete = lens _gsiuDelete (\ s a -> s{_gsiuDelete = a})

-- | The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\ s a -> s{_gsiuUpdate = a})

instance Hashable GlobalSecondaryIndexUpdate where

instance NFData GlobalSecondaryIndexUpdate where

instance ToJSON GlobalSecondaryIndexUpdate where
        toJSON GlobalSecondaryIndexUpdate'{..}
          = object
              (catMaybes
                 [("Create" .=) <$> _gsiuCreate,
                  ("Delete" .=) <$> _gsiuDelete,
                  ("Update" .=) <$> _gsiuUpdate])

-- | Represents the properties of a global table.
--
--
--
-- /See:/ 'globalTable' smart constructor.
data GlobalTable = GlobalTable'
  { _gtGlobalTableName  :: !(Maybe Text)
  , _gtReplicationGroup :: !(Maybe [Replica])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtGlobalTableName' - The global table name.
--
-- * 'gtReplicationGroup' - The regions where the global table has replicas.
globalTable
    :: GlobalTable
globalTable =
  GlobalTable' {_gtGlobalTableName = Nothing, _gtReplicationGroup = Nothing}


-- | The global table name.
gtGlobalTableName :: Lens' GlobalTable (Maybe Text)
gtGlobalTableName = lens _gtGlobalTableName (\ s a -> s{_gtGlobalTableName = a})

-- | The regions where the global table has replicas.
gtReplicationGroup :: Lens' GlobalTable [Replica]
gtReplicationGroup = lens _gtReplicationGroup (\ s a -> s{_gtReplicationGroup = a}) . _Default . _Coerce

instance FromJSON GlobalTable where
        parseJSON
          = withObject "GlobalTable"
              (\ x ->
                 GlobalTable' <$>
                   (x .:? "GlobalTableName") <*>
                     (x .:? "ReplicationGroup" .!= mempty))

instance Hashable GlobalTable where

instance NFData GlobalTable where

-- | Contains details about the global table.
--
--
--
-- /See:/ 'globalTableDescription' smart constructor.
data GlobalTableDescription = GlobalTableDescription'
  { _gtdGlobalTableStatus :: !(Maybe GlobalTableStatus)
  , _gtdGlobalTableName   :: !(Maybe Text)
  , _gtdGlobalTableARN    :: !(Maybe Text)
  , _gtdCreationDateTime  :: !(Maybe POSIX)
  , _gtdReplicationGroup  :: !(Maybe [ReplicaDescription])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalTableDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdGlobalTableStatus' - The current state of the global table:     * @CREATING@ - The global table is being created.     * @UPDATING@ - The global table is being updated.     * @DELETING@ - The global table is being deleted.     * @ACTIVE@ - The global table is ready for use.
--
-- * 'gtdGlobalTableName' - The global table name.
--
-- * 'gtdGlobalTableARN' - The unique identifier of the global table.
--
-- * 'gtdCreationDateTime' - The creation time of the global table.
--
-- * 'gtdReplicationGroup' - The regions where the global table has replicas.
globalTableDescription
    :: GlobalTableDescription
globalTableDescription =
  GlobalTableDescription'
    { _gtdGlobalTableStatus = Nothing
    , _gtdGlobalTableName = Nothing
    , _gtdGlobalTableARN = Nothing
    , _gtdCreationDateTime = Nothing
    , _gtdReplicationGroup = Nothing
    }


-- | The current state of the global table:     * @CREATING@ - The global table is being created.     * @UPDATING@ - The global table is being updated.     * @DELETING@ - The global table is being deleted.     * @ACTIVE@ - The global table is ready for use.
gtdGlobalTableStatus :: Lens' GlobalTableDescription (Maybe GlobalTableStatus)
gtdGlobalTableStatus = lens _gtdGlobalTableStatus (\ s a -> s{_gtdGlobalTableStatus = a})

-- | The global table name.
gtdGlobalTableName :: Lens' GlobalTableDescription (Maybe Text)
gtdGlobalTableName = lens _gtdGlobalTableName (\ s a -> s{_gtdGlobalTableName = a})

-- | The unique identifier of the global table.
gtdGlobalTableARN :: Lens' GlobalTableDescription (Maybe Text)
gtdGlobalTableARN = lens _gtdGlobalTableARN (\ s a -> s{_gtdGlobalTableARN = a})

-- | The creation time of the global table.
gtdCreationDateTime :: Lens' GlobalTableDescription (Maybe UTCTime)
gtdCreationDateTime = lens _gtdCreationDateTime (\ s a -> s{_gtdCreationDateTime = a}) . mapping _Time

-- | The regions where the global table has replicas.
gtdReplicationGroup :: Lens' GlobalTableDescription [ReplicaDescription]
gtdReplicationGroup = lens _gtdReplicationGroup (\ s a -> s{_gtdReplicationGroup = a}) . _Default . _Coerce

instance FromJSON GlobalTableDescription where
        parseJSON
          = withObject "GlobalTableDescription"
              (\ x ->
                 GlobalTableDescription' <$>
                   (x .:? "GlobalTableStatus") <*>
                     (x .:? "GlobalTableName")
                     <*> (x .:? "GlobalTableArn")
                     <*> (x .:? "CreationDateTime")
                     <*> (x .:? "ReplicationGroup" .!= mempty))

instance Hashable GlobalTableDescription where

instance NFData GlobalTableDescription where

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
--
--
-- /See:/ 'globalTableGlobalSecondaryIndexSettingsUpdate' smart constructor.
data GlobalTableGlobalSecondaryIndexSettingsUpdate = GlobalTableGlobalSecondaryIndexSettingsUpdate'
  { _gtgsisuProvisionedWriteCapacityUnits :: !(Maybe Nat)
  , _gtgsisuIndexName                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalTableGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgsisuProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- * 'gtgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
globalTableGlobalSecondaryIndexSettingsUpdate
    :: Text -- ^ 'gtgsisuIndexName'
    -> GlobalTableGlobalSecondaryIndexSettingsUpdate
globalTableGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  GlobalTableGlobalSecondaryIndexSettingsUpdate'
    { _gtgsisuProvisionedWriteCapacityUnits = Nothing
    , _gtgsisuIndexName = pIndexName_
    }


-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
gtgsisuProvisionedWriteCapacityUnits :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Maybe Natural)
gtgsisuProvisionedWriteCapacityUnits = lens _gtgsisuProvisionedWriteCapacityUnits (\ s a -> s{_gtgsisuProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
gtgsisuIndexName :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate Text
gtgsisuIndexName = lens _gtgsisuIndexName (\ s a -> s{_gtgsisuIndexName = a})

instance Hashable
           GlobalTableGlobalSecondaryIndexSettingsUpdate
         where

instance NFData
           GlobalTableGlobalSecondaryIndexSettingsUpdate
         where

instance ToJSON
           GlobalTableGlobalSecondaryIndexSettingsUpdate
         where
        toJSON
          GlobalTableGlobalSecondaryIndexSettingsUpdate'{..}
          = object
              (catMaybes
                 [("ProvisionedWriteCapacityUnits" .=) <$>
                    _gtgsisuProvisionedWriteCapacityUnits,
                  Just ("IndexName" .= _gtgsisuIndexName)])

-- | Information about item collections, if any, that were affected by the operation. @ItemCollectionMetrics@ is only returned if the request asked for it. If the table does not have any local secondary indexes, this information is not returned in the response.
--
--
--
-- /See:/ 'itemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { _icmItemCollectionKey   :: !(Maybe (Map Text AttributeValue))
  , _icmSizeEstimateRangeGB :: !(Maybe [Double])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ItemCollectionMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icmItemCollectionKey' - The partition key value of the item collection. This value is the same as the partition key value of the item.
--
-- * 'icmSizeEstimateRangeGB' - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
itemCollectionMetrics
    :: ItemCollectionMetrics
itemCollectionMetrics =
  ItemCollectionMetrics'
    {_icmItemCollectionKey = Nothing, _icmSizeEstimateRangeGB = Nothing}


-- | The partition key value of the item collection. This value is the same as the partition key value of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (HashMap Text AttributeValue)
icmItemCollectionKey = lens _icmItemCollectionKey (\ s a -> s{_icmItemCollectionKey = a}) . _Default . _Map

-- | An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
icmSizeEstimateRangeGB :: Lens' ItemCollectionMetrics [Double]
icmSizeEstimateRangeGB = lens _icmSizeEstimateRangeGB (\ s a -> s{_icmSizeEstimateRangeGB = a}) . _Default . _Coerce

instance FromJSON ItemCollectionMetrics where
        parseJSON
          = withObject "ItemCollectionMetrics"
              (\ x ->
                 ItemCollectionMetrics' <$>
                   (x .:? "ItemCollectionKey" .!= mempty) <*>
                     (x .:? "SizeEstimateRangeGB" .!= mempty))

instance Hashable ItemCollectionMetrics where

instance NFData ItemCollectionMetrics where

-- | Represents /a single element/ of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.
--
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary key. For example, a simple primary key would be represented by one @KeySchemaElement@ (for the partition key). A composite primary key would require one @KeySchemaElement@ for the partition key, and another @KeySchemaElement@ for the sort key.
--
-- A @KeySchemaElement@ must be a scalar, top-level attribute (not a nested attribute). The data type must be one of String, Number, or Binary. The attribute cannot be nested within a List or a Map.
--
--
-- /See:/ 'keySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { _kseAttributeName :: !Text
  , _kseKeyType       :: !KeyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kseAttributeName' - The name of a key attribute.
--
-- * 'kseKeyType' - The role that this key attribute will assume:     * @HASH@ - partition key     * @RANGE@ - sort key
keySchemaElement
    :: Text -- ^ 'kseAttributeName'
    -> KeyType -- ^ 'kseKeyType'
    -> KeySchemaElement
keySchemaElement pAttributeName_ pKeyType_ =
  KeySchemaElement'
    {_kseAttributeName = pAttributeName_, _kseKeyType = pKeyType_}


-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\ s a -> s{_kseAttributeName = a})

-- | The role that this key attribute will assume:     * @HASH@ - partition key     * @RANGE@ - sort key
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\ s a -> s{_kseKeyType = a})

instance FromJSON KeySchemaElement where
        parseJSON
          = withObject "KeySchemaElement"
              (\ x ->
                 KeySchemaElement' <$>
                   (x .: "AttributeName") <*> (x .: "KeyType"))

instance Hashable KeySchemaElement where

instance NFData KeySchemaElement where

instance ToJSON KeySchemaElement where
        toJSON KeySchemaElement'{..}
          = object
              (catMaybes
                 [Just ("AttributeName" .= _kseAttributeName),
                  Just ("KeyType" .= _kseKeyType)])

-- | Represents a set of primary keys and, for each key, the attributes to retrieve from the table.
--
--
-- For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key. For a composite primary key, you must provide /both/ the partition key and the sort key.
--
--
-- /See:/ 'keysAndAttributes' smart constructor.
data KeysAndAttributes = KeysAndAttributes'
  { _kaaProjectionExpression     :: !(Maybe Text)
  , _kaaAttributesToGet          :: !(Maybe (List1 Text))
  , _kaaExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _kaaConsistentRead           :: !(Maybe Bool)
  , _kaaKeys                     :: !(List1 (Map Text AttributeValue))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeysAndAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kaaProjectionExpression' - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaAttributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'kaaConsistentRead' - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
-- * 'kaaKeys' - The primary key attribute values that define the items and the attributes associated with the items.
keysAndAttributes
    :: NonEmpty (HashMap Text AttributeValue) -- ^ 'kaaKeys'
    -> KeysAndAttributes
keysAndAttributes pKeys_ =
  KeysAndAttributes'
    { _kaaProjectionExpression = Nothing
    , _kaaAttributesToGet = Nothing
    , _kaaExpressionAttributeNames = Nothing
    , _kaaConsistentRead = Nothing
    , _kaaKeys = _List1 # pKeys_
    }


-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the @ProjectionExpression@ must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
kaaProjectionExpression :: Lens' KeysAndAttributes (Maybe Text)
kaaProjectionExpression = lens _kaaProjectionExpression (\ s a -> s{_kaaProjectionExpression = a})

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
kaaAttributesToGet :: Lens' KeysAndAttributes (Maybe (NonEmpty Text))
kaaAttributesToGet = lens _kaaAttributesToGet (\ s a -> s{_kaaAttributesToGet = a}) . mapping _List1

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
kaaExpressionAttributeNames :: Lens' KeysAndAttributes (HashMap Text Text)
kaaExpressionAttributeNames = lens _kaaExpressionAttributeNames (\ s a -> s{_kaaExpressionAttributeNames = a}) . _Default . _Map

-- | The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
kaaConsistentRead :: Lens' KeysAndAttributes (Maybe Bool)
kaaConsistentRead = lens _kaaConsistentRead (\ s a -> s{_kaaConsistentRead = a})

-- | The primary key attribute values that define the items and the attributes associated with the items.
kaaKeys :: Lens' KeysAndAttributes (NonEmpty (HashMap Text AttributeValue))
kaaKeys = lens _kaaKeys (\ s a -> s{_kaaKeys = a}) . _List1

instance FromJSON KeysAndAttributes where
        parseJSON
          = withObject "KeysAndAttributes"
              (\ x ->
                 KeysAndAttributes' <$>
                   (x .:? "ProjectionExpression") <*>
                     (x .:? "AttributesToGet")
                     <*> (x .:? "ExpressionAttributeNames" .!= mempty)
                     <*> (x .:? "ConsistentRead")
                     <*> (x .: "Keys"))

instance Hashable KeysAndAttributes where

instance NFData KeysAndAttributes where

instance ToJSON KeysAndAttributes where
        toJSON KeysAndAttributes'{..}
          = object
              (catMaybes
                 [("ProjectionExpression" .=) <$>
                    _kaaProjectionExpression,
                  ("AttributesToGet" .=) <$> _kaaAttributesToGet,
                  ("ExpressionAttributeNames" .=) <$>
                    _kaaExpressionAttributeNames,
                  ("ConsistentRead" .=) <$> _kaaConsistentRead,
                  Just ("Keys" .= _kaaKeys)])

-- | Represents the properties of a local secondary index.
--
--
--
-- /See:/ 'localSecondaryIndex' smart constructor.
data LocalSecondaryIndex = LocalSecondaryIndex'
  { _lsiIndexName  :: !Text
  , _lsiKeySchema  :: !(List1 KeySchemaElement)
  , _lsiProjection :: !Projection
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsiIndexName' - The name of the local secondary index. The name must be unique among all other indexes on this table.
--
-- * 'lsiKeySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'lsiProjection' - Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
localSecondaryIndex
    :: Text -- ^ 'lsiIndexName'
    -> NonEmpty KeySchemaElement -- ^ 'lsiKeySchema'
    -> Projection -- ^ 'lsiProjection'
    -> LocalSecondaryIndex
localSecondaryIndex pIndexName_ pKeySchema_ pProjection_ =
  LocalSecondaryIndex'
    { _lsiIndexName = pIndexName_
    , _lsiKeySchema = _List1 # pKeySchema_
    , _lsiProjection = pProjection_
    }


-- | The name of the local secondary index. The name must be unique among all other indexes on this table.
lsiIndexName :: Lens' LocalSecondaryIndex Text
lsiIndexName = lens _lsiIndexName (\ s a -> s{_lsiIndexName = a})

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
lsiKeySchema :: Lens' LocalSecondaryIndex (NonEmpty KeySchemaElement)
lsiKeySchema = lens _lsiKeySchema (\ s a -> s{_lsiKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
lsiProjection :: Lens' LocalSecondaryIndex Projection
lsiProjection = lens _lsiProjection (\ s a -> s{_lsiProjection = a})

instance Hashable LocalSecondaryIndex where

instance NFData LocalSecondaryIndex where

instance ToJSON LocalSecondaryIndex where
        toJSON LocalSecondaryIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexName" .= _lsiIndexName),
                  Just ("KeySchema" .= _lsiKeySchema),
                  Just ("Projection" .= _lsiProjection)])

-- | Represents the properties of a local secondary index.
--
--
--
-- /See:/ 'localSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { _lsidIndexSizeBytes :: !(Maybe Integer)
  , _lsidIndexARN       :: !(Maybe Text)
  , _lsidKeySchema      :: !(Maybe (List1 KeySchemaElement))
  , _lsidProjection     :: !(Maybe Projection)
  , _lsidItemCount      :: !(Maybe Integer)
  , _lsidIndexName      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsidIndexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'lsidIndexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- * 'lsidKeySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'lsidProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'lsidItemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'lsidIndexName' - Represents the name of the local secondary index.
localSecondaryIndexDescription
    :: LocalSecondaryIndexDescription
localSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { _lsidIndexSizeBytes = Nothing
    , _lsidIndexARN = Nothing
    , _lsidKeySchema = Nothing
    , _lsidProjection = Nothing
    , _lsidItemCount = Nothing
    , _lsidIndexName = Nothing
    }


-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
lsidIndexSizeBytes :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidIndexSizeBytes = lens _lsidIndexSizeBytes (\ s a -> s{_lsidIndexSizeBytes = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
lsidIndexARN :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexARN = lens _lsidIndexARN (\ s a -> s{_lsidIndexARN = a})

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
lsidKeySchema :: Lens' LocalSecondaryIndexDescription (Maybe (NonEmpty KeySchemaElement))
lsidKeySchema = lens _lsidKeySchema (\ s a -> s{_lsidKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
lsidProjection :: Lens' LocalSecondaryIndexDescription (Maybe Projection)
lsidProjection = lens _lsidProjection (\ s a -> s{_lsidProjection = a})

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
lsidItemCount :: Lens' LocalSecondaryIndexDescription (Maybe Integer)
lsidItemCount = lens _lsidItemCount (\ s a -> s{_lsidItemCount = a})

-- | Represents the name of the local secondary index.
lsidIndexName :: Lens' LocalSecondaryIndexDescription (Maybe Text)
lsidIndexName = lens _lsidIndexName (\ s a -> s{_lsidIndexName = a})

instance FromJSON LocalSecondaryIndexDescription
         where
        parseJSON
          = withObject "LocalSecondaryIndexDescription"
              (\ x ->
                 LocalSecondaryIndexDescription' <$>
                   (x .:? "IndexSizeBytes") <*> (x .:? "IndexArn") <*>
                     (x .:? "KeySchema")
                     <*> (x .:? "Projection")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "IndexName"))

instance Hashable LocalSecondaryIndexDescription
         where

instance NFData LocalSecondaryIndexDescription where

-- | Represents the properties of a local secondary index for the table when the backup was created.
--
--
--
-- /See:/ 'localSecondaryIndexInfo' smart constructor.
data LocalSecondaryIndexInfo = LocalSecondaryIndexInfo'
  { _lsiiKeySchema  :: !(Maybe (List1 KeySchemaElement))
  , _lsiiProjection :: !(Maybe Projection)
  , _lsiiIndexName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalSecondaryIndexInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsiiKeySchema' - The complete key schema for a local secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'lsiiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- * 'lsiiIndexName' - Represents the name of the local secondary index.
localSecondaryIndexInfo
    :: LocalSecondaryIndexInfo
localSecondaryIndexInfo =
  LocalSecondaryIndexInfo'
    { _lsiiKeySchema = Nothing
    , _lsiiProjection = Nothing
    , _lsiiIndexName = Nothing
    }


-- | The complete key schema for a local secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
lsiiKeySchema :: Lens' LocalSecondaryIndexInfo (Maybe (NonEmpty KeySchemaElement))
lsiiKeySchema = lens _lsiiKeySchema (\ s a -> s{_lsiiKeySchema = a}) . mapping _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
lsiiProjection :: Lens' LocalSecondaryIndexInfo (Maybe Projection)
lsiiProjection = lens _lsiiProjection (\ s a -> s{_lsiiProjection = a})

-- | Represents the name of the local secondary index.
lsiiIndexName :: Lens' LocalSecondaryIndexInfo (Maybe Text)
lsiiIndexName = lens _lsiiIndexName (\ s a -> s{_lsiiIndexName = a})

instance FromJSON LocalSecondaryIndexInfo where
        parseJSON
          = withObject "LocalSecondaryIndexInfo"
              (\ x ->
                 LocalSecondaryIndexInfo' <$>
                   (x .:? "KeySchema") <*> (x .:? "Projection") <*>
                     (x .:? "IndexName"))

instance Hashable LocalSecondaryIndexInfo where

instance NFData LocalSecondaryIndexInfo where

-- | The description of the point in time settings applied to the table.
--
--
--
-- /See:/ 'pointInTimeRecoveryDescription' smart constructor.
data PointInTimeRecoveryDescription = PointInTimeRecoveryDescription'
  { _pitrdPointInTimeRecoveryStatus  :: !(Maybe PointInTimeRecoveryStatus)
  , _pitrdEarliestRestorableDateTime :: !(Maybe POSIX)
  , _pitrdLatestRestorableDateTime   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PointInTimeRecoveryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitrdPointInTimeRecoveryStatus' - The current state of point in time recovery:     * @ENABLING@ - Point in time recovery is being enabled.     * @ENABLED@ - Point in time recovery is enabled.     * @DISABLED@ - Point in time recovery is disabled.
--
-- * 'pitrdEarliestRestorableDateTime' - Specifies the earliest point in time you can restore your table to. It You can restore your table to any point in time during the last 35 days.
--
-- * 'pitrdLatestRestorableDateTime' - @LatestRestorableDateTime@ is typically 5 minutes before the current time.
pointInTimeRecoveryDescription
    :: PointInTimeRecoveryDescription
pointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { _pitrdPointInTimeRecoveryStatus = Nothing
    , _pitrdEarliestRestorableDateTime = Nothing
    , _pitrdLatestRestorableDateTime = Nothing
    }


-- | The current state of point in time recovery:     * @ENABLING@ - Point in time recovery is being enabled.     * @ENABLED@ - Point in time recovery is enabled.     * @DISABLED@ - Point in time recovery is disabled.
pitrdPointInTimeRecoveryStatus :: Lens' PointInTimeRecoveryDescription (Maybe PointInTimeRecoveryStatus)
pitrdPointInTimeRecoveryStatus = lens _pitrdPointInTimeRecoveryStatus (\ s a -> s{_pitrdPointInTimeRecoveryStatus = a})

-- | Specifies the earliest point in time you can restore your table to. It You can restore your table to any point in time during the last 35 days.
pitrdEarliestRestorableDateTime :: Lens' PointInTimeRecoveryDescription (Maybe UTCTime)
pitrdEarliestRestorableDateTime = lens _pitrdEarliestRestorableDateTime (\ s a -> s{_pitrdEarliestRestorableDateTime = a}) . mapping _Time

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
pitrdLatestRestorableDateTime :: Lens' PointInTimeRecoveryDescription (Maybe UTCTime)
pitrdLatestRestorableDateTime = lens _pitrdLatestRestorableDateTime (\ s a -> s{_pitrdLatestRestorableDateTime = a}) . mapping _Time

instance FromJSON PointInTimeRecoveryDescription
         where
        parseJSON
          = withObject "PointInTimeRecoveryDescription"
              (\ x ->
                 PointInTimeRecoveryDescription' <$>
                   (x .:? "PointInTimeRecoveryStatus") <*>
                     (x .:? "EarliestRestorableDateTime")
                     <*> (x .:? "LatestRestorableDateTime"))

instance Hashable PointInTimeRecoveryDescription
         where

instance NFData PointInTimeRecoveryDescription where

-- | Represents the settings used to enable point in time recovery.
--
--
--
-- /See:/ 'pointInTimeRecoverySpecification' smart constructor.
newtype PointInTimeRecoverySpecification = PointInTimeRecoverySpecification'
  { _pitrsPointInTimeRecoveryEnabled :: Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PointInTimeRecoverySpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitrsPointInTimeRecoveryEnabled' - Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
pointInTimeRecoverySpecification
    :: Bool -- ^ 'pitrsPointInTimeRecoveryEnabled'
    -> PointInTimeRecoverySpecification
pointInTimeRecoverySpecification pPointInTimeRecoveryEnabled_ =
  PointInTimeRecoverySpecification'
    {_pitrsPointInTimeRecoveryEnabled = pPointInTimeRecoveryEnabled_}


-- | Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
pitrsPointInTimeRecoveryEnabled :: Lens' PointInTimeRecoverySpecification Bool
pitrsPointInTimeRecoveryEnabled = lens _pitrsPointInTimeRecoveryEnabled (\ s a -> s{_pitrsPointInTimeRecoveryEnabled = a})

instance Hashable PointInTimeRecoverySpecification
         where

instance NFData PointInTimeRecoverySpecification
         where

instance ToJSON PointInTimeRecoverySpecification
         where
        toJSON PointInTimeRecoverySpecification'{..}
          = object
              (catMaybes
                 [Just
                    ("PointInTimeRecoveryEnabled" .=
                       _pitrsPointInTimeRecoveryEnabled)])

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
--
--
-- /See:/ 'projection' smart constructor.
data Projection = Projection'
  { _pProjectionType   :: !(Maybe ProjectionType)
  , _pNonKeyAttributes :: !(Maybe (List1 Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Projection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pProjectionType' - The set of attributes that are projected into the index:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.
--
-- * 'pNonKeyAttributes' - Represents the non-key attribute names which will be projected into the index. For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
projection
    :: Projection
projection =
  Projection' {_pProjectionType = Nothing, _pNonKeyAttributes = Nothing}


-- | The set of attributes that are projected into the index:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.
pProjectionType :: Lens' Projection (Maybe ProjectionType)
pProjectionType = lens _pProjectionType (\ s a -> s{_pProjectionType = a})

-- | Represents the non-key attribute names which will be projected into the index. For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
pNonKeyAttributes :: Lens' Projection (Maybe (NonEmpty Text))
pNonKeyAttributes = lens _pNonKeyAttributes (\ s a -> s{_pNonKeyAttributes = a}) . mapping _List1

instance FromJSON Projection where
        parseJSON
          = withObject "Projection"
              (\ x ->
                 Projection' <$>
                   (x .:? "ProjectionType") <*>
                     (x .:? "NonKeyAttributes"))

instance Hashable Projection where

instance NFData Projection where

instance ToJSON Projection where
        toJSON Projection'{..}
          = object
              (catMaybes
                 [("ProjectionType" .=) <$> _pProjectionType,
                  ("NonKeyAttributes" .=) <$> _pNonKeyAttributes])

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
--
-- For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
--
--
-- /See:/ 'provisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { _ptReadCapacityUnits  :: !Nat
  , _ptWriteCapacityUnits :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedThroughput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'ptWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
provisionedThroughput
    :: Natural -- ^ 'ptReadCapacityUnits'
    -> Natural -- ^ 'ptWriteCapacityUnits'
    -> ProvisionedThroughput
provisionedThroughput pReadCapacityUnits_ pWriteCapacityUnits_ =
  ProvisionedThroughput'
    { _ptReadCapacityUnits = _Nat # pReadCapacityUnits_
    , _ptWriteCapacityUnits = _Nat # pWriteCapacityUnits_
    }


-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits = lens _ptReadCapacityUnits (\ s a -> s{_ptReadCapacityUnits = a}) . _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Natural
ptWriteCapacityUnits = lens _ptWriteCapacityUnits (\ s a -> s{_ptWriteCapacityUnits = a}) . _Nat

instance FromJSON ProvisionedThroughput where
        parseJSON
          = withObject "ProvisionedThroughput"
              (\ x ->
                 ProvisionedThroughput' <$>
                   (x .: "ReadCapacityUnits") <*>
                     (x .: "WriteCapacityUnits"))

instance Hashable ProvisionedThroughput where

instance NFData ProvisionedThroughput where

instance ToJSON ProvisionedThroughput where
        toJSON ProvisionedThroughput'{..}
          = object
              (catMaybes
                 [Just ("ReadCapacityUnits" .= _ptReadCapacityUnits),
                  Just
                    ("WriteCapacityUnits" .= _ptWriteCapacityUnits)])

-- | Represents the provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
--
--
-- /See:/ 'provisionedThroughputDescription' smart constructor.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
  { _ptdReadCapacityUnits      :: !(Maybe Nat)
  , _ptdLastDecreaseDateTime   :: !(Maybe POSIX)
  , _ptdWriteCapacityUnits     :: !(Maybe Nat)
  , _ptdNumberOfDecreasesToday :: !(Maybe Nat)
  , _ptdLastIncreaseDateTime   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedThroughputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptdReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
--
-- * 'ptdLastDecreaseDateTime' - The date and time of the last provisioned throughput decrease for this table.
--
-- * 'ptdWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'ptdNumberOfDecreasesToday' - The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'ptdLastIncreaseDateTime' - The date and time of the last provisioned throughput increase for this table.
provisionedThroughputDescription
    :: ProvisionedThroughputDescription
provisionedThroughputDescription =
  ProvisionedThroughputDescription'
    { _ptdReadCapacityUnits = Nothing
    , _ptdLastDecreaseDateTime = Nothing
    , _ptdWriteCapacityUnits = Nothing
    , _ptdNumberOfDecreasesToday = Nothing
    , _ptdLastIncreaseDateTime = Nothing
    }


-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdReadCapacityUnits = lens _ptdReadCapacityUnits (\ s a -> s{_ptdReadCapacityUnits = a}) . mapping _Nat

-- | The date and time of the last provisioned throughput decrease for this table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastDecreaseDateTime = lens _ptdLastDecreaseDateTime (\ s a -> s{_ptdLastDecreaseDateTime = a}) . mapping _Time

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdWriteCapacityUnits = lens _ptdWriteCapacityUnits (\ s a -> s{_ptdWriteCapacityUnits = a}) . mapping _Nat

-- | The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdNumberOfDecreasesToday = lens _ptdNumberOfDecreasesToday (\ s a -> s{_ptdNumberOfDecreasesToday = a}) . mapping _Nat

-- | The date and time of the last provisioned throughput increase for this table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastIncreaseDateTime = lens _ptdLastIncreaseDateTime (\ s a -> s{_ptdLastIncreaseDateTime = a}) . mapping _Time

instance FromJSON ProvisionedThroughputDescription
         where
        parseJSON
          = withObject "ProvisionedThroughputDescription"
              (\ x ->
                 ProvisionedThroughputDescription' <$>
                   (x .:? "ReadCapacityUnits") <*>
                     (x .:? "LastDecreaseDateTime")
                     <*> (x .:? "WriteCapacityUnits")
                     <*> (x .:? "NumberOfDecreasesToday")
                     <*> (x .:? "LastIncreaseDateTime"))

instance Hashable ProvisionedThroughputDescription
         where

instance NFData ProvisionedThroughputDescription
         where

-- | Represents a request to perform a @PutItem@ operation on an item.
--
--
--
-- /See:/ 'putRequest' smart constructor.
newtype PutRequest = PutRequest'
  { _prItem :: Map Text AttributeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prItem' - A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item which are part of an index key schema for the table, their types must match the index key schema.
putRequest
    :: PutRequest
putRequest = PutRequest' {_prItem = mempty}


-- | A map of attribute name to attribute values, representing the primary key of an item to be processed by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item which are part of an index key schema for the table, their types must match the index key schema.
prItem :: Lens' PutRequest (HashMap Text AttributeValue)
prItem = lens _prItem (\ s a -> s{_prItem = a}) . _Map

instance FromJSON PutRequest where
        parseJSON
          = withObject "PutRequest"
              (\ x -> PutRequest' <$> (x .:? "Item" .!= mempty))

instance Hashable PutRequest where

instance NFData PutRequest where

instance ToJSON PutRequest where
        toJSON PutRequest'{..}
          = object (catMaybes [Just ("Item" .= _prItem)])

-- | Represents the properties of a replica.
--
--
--
-- /See:/ 'replica' smart constructor.
newtype Replica = Replica'
  { _rRegionName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Replica' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rRegionName' - The region where the replica needs to be created.
replica
    :: Replica
replica = Replica' {_rRegionName = Nothing}


-- | The region where the replica needs to be created.
rRegionName :: Lens' Replica (Maybe Text)
rRegionName = lens _rRegionName (\ s a -> s{_rRegionName = a})

instance FromJSON Replica where
        parseJSON
          = withObject "Replica"
              (\ x -> Replica' <$> (x .:? "RegionName"))

instance Hashable Replica where

instance NFData Replica where

instance ToJSON Replica where
        toJSON Replica'{..}
          = object
              (catMaybes [("RegionName" .=) <$> _rRegionName])

-- | Contains the details of the replica.
--
--
--
-- /See:/ 'replicaDescription' smart constructor.
newtype ReplicaDescription = ReplicaDescription'
  { _rdRegionName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdRegionName' - The name of the region.
replicaDescription
    :: ReplicaDescription
replicaDescription = ReplicaDescription' {_rdRegionName = Nothing}


-- | The name of the region.
rdRegionName :: Lens' ReplicaDescription (Maybe Text)
rdRegionName = lens _rdRegionName (\ s a -> s{_rdRegionName = a})

instance FromJSON ReplicaDescription where
        parseJSON
          = withObject "ReplicaDescription"
              (\ x -> ReplicaDescription' <$> (x .:? "RegionName"))

instance Hashable ReplicaDescription where

instance NFData ReplicaDescription where

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { _rgsisdIndexStatus                   :: !(Maybe IndexStatus)
  , _rgsisdProvisionedReadCapacityUnits  :: !(Maybe Nat)
  , _rgsisdProvisionedWriteCapacityUnits :: !(Maybe Nat)
  , _rgsisdIndexName                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsisdIndexStatus' - The current status of the global secondary index:     * @CREATING@ - The global secondary index is being created.     * @UPDATING@ - The global secondary index is being updated.     * @DELETING@ - The global secondary index is being deleted.     * @ACTIVE@ - The global secondary index is ready for use.
--
-- * 'rgsisdProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisdProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisdIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
replicaGlobalSecondaryIndexSettingsDescription
    :: Text -- ^ 'rgsisdIndexName'
    -> ReplicaGlobalSecondaryIndexSettingsDescription
replicaGlobalSecondaryIndexSettingsDescription pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsDescription'
    { _rgsisdIndexStatus = Nothing
    , _rgsisdProvisionedReadCapacityUnits = Nothing
    , _rgsisdProvisionedWriteCapacityUnits = Nothing
    , _rgsisdIndexName = pIndexName_
    }


-- | The current status of the global secondary index:     * @CREATING@ - The global secondary index is being created.     * @UPDATING@ - The global secondary index is being updated.     * @DELETING@ - The global secondary index is being deleted.     * @ACTIVE@ - The global secondary index is ready for use.
rgsisdIndexStatus :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe IndexStatus)
rgsisdIndexStatus = lens _rgsisdIndexStatus (\ s a -> s{_rgsisdIndexStatus = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisdProvisionedReadCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe Natural)
rgsisdProvisionedReadCapacityUnits = lens _rgsisdProvisionedReadCapacityUnits (\ s a -> s{_rgsisdProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisdProvisionedWriteCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe Natural)
rgsisdProvisionedWriteCapacityUnits = lens _rgsisdProvisionedWriteCapacityUnits (\ s a -> s{_rgsisdProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
rgsisdIndexName :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription Text
rgsisdIndexName = lens _rgsisdIndexName (\ s a -> s{_rgsisdIndexName = a})

instance FromJSON
           ReplicaGlobalSecondaryIndexSettingsDescription
         where
        parseJSON
          = withObject
              "ReplicaGlobalSecondaryIndexSettingsDescription"
              (\ x ->
                 ReplicaGlobalSecondaryIndexSettingsDescription' <$>
                   (x .:? "IndexStatus") <*>
                     (x .:? "ProvisionedReadCapacityUnits")
                     <*> (x .:? "ProvisionedWriteCapacityUnits")
                     <*> (x .: "IndexName"))

instance Hashable
           ReplicaGlobalSecondaryIndexSettingsDescription
         where

instance NFData
           ReplicaGlobalSecondaryIndexSettingsDescription
         where

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
--
--
-- /See:/ 'replicaGlobalSecondaryIndexSettingsUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsUpdate = ReplicaGlobalSecondaryIndexSettingsUpdate'
  { _rgsisuProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rgsisuIndexName                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsisuProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
replicaGlobalSecondaryIndexSettingsUpdate
    :: Text -- ^ 'rgsisuIndexName'
    -> ReplicaGlobalSecondaryIndexSettingsUpdate
replicaGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsUpdate'
    { _rgsisuProvisionedReadCapacityUnits = Nothing
    , _rgsisuIndexName = pIndexName_
    }


-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
rgsisuProvisionedReadCapacityUnits :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Maybe Natural)
rgsisuProvisionedReadCapacityUnits = lens _rgsisuProvisionedReadCapacityUnits (\ s a -> s{_rgsisuProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
rgsisuIndexName :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate Text
rgsisuIndexName = lens _rgsisuIndexName (\ s a -> s{_rgsisuIndexName = a})

instance Hashable
           ReplicaGlobalSecondaryIndexSettingsUpdate
         where

instance NFData
           ReplicaGlobalSecondaryIndexSettingsUpdate
         where

instance ToJSON
           ReplicaGlobalSecondaryIndexSettingsUpdate
         where
        toJSON ReplicaGlobalSecondaryIndexSettingsUpdate'{..}
          = object
              (catMaybes
                 [("ProvisionedReadCapacityUnits" .=) <$>
                    _rgsisuProvisionedReadCapacityUnits,
                  Just ("IndexName" .= _rgsisuIndexName)])

-- | Represents the properties of a replica.
--
--
--
-- /See:/ 'replicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { _rsdReplicaStatus :: !(Maybe ReplicaStatus)
  , _rsdReplicaProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rsdReplicaProvisionedWriteCapacityUnits :: !(Maybe Nat)
  , _rsdReplicaGlobalSecondaryIndexSettings :: !(Maybe [ReplicaGlobalSecondaryIndexSettingsDescription])
  , _rsdRegionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsdReplicaStatus' - The current state of the region:     * @CREATING@ - The region is being created.     * @UPDATING@ - The region is being updated.     * @DELETING@ - The region is being deleted.     * @ACTIVE@ - The region is ready for use.
--
-- * 'rsdReplicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsdReplicaProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsdReplicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
--
-- * 'rsdRegionName' - The region name of the replica.
replicaSettingsDescription
    :: Text -- ^ 'rsdRegionName'
    -> ReplicaSettingsDescription
replicaSettingsDescription pRegionName_ =
  ReplicaSettingsDescription'
    { _rsdReplicaStatus = Nothing
    , _rsdReplicaProvisionedReadCapacityUnits = Nothing
    , _rsdReplicaProvisionedWriteCapacityUnits = Nothing
    , _rsdReplicaGlobalSecondaryIndexSettings = Nothing
    , _rsdRegionName = pRegionName_
    }


-- | The current state of the region:     * @CREATING@ - The region is being created.     * @UPDATING@ - The region is being updated.     * @DELETING@ - The region is being deleted.     * @ACTIVE@ - The region is ready for use.
rsdReplicaStatus :: Lens' ReplicaSettingsDescription (Maybe ReplicaStatus)
rsdReplicaStatus = lens _rsdReplicaStatus (\ s a -> s{_rsdReplicaStatus = a})

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsdReplicaProvisionedReadCapacityUnits :: Lens' ReplicaSettingsDescription (Maybe Natural)
rsdReplicaProvisionedReadCapacityUnits = lens _rsdReplicaProvisionedReadCapacityUnits (\ s a -> s{_rsdReplicaProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsdReplicaProvisionedWriteCapacityUnits :: Lens' ReplicaSettingsDescription (Maybe Natural)
rsdReplicaProvisionedWriteCapacityUnits = lens _rsdReplicaProvisionedWriteCapacityUnits (\ s a -> s{_rsdReplicaProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | Replica global secondary index settings for the global table.
rsdReplicaGlobalSecondaryIndexSettings :: Lens' ReplicaSettingsDescription [ReplicaGlobalSecondaryIndexSettingsDescription]
rsdReplicaGlobalSecondaryIndexSettings = lens _rsdReplicaGlobalSecondaryIndexSettings (\ s a -> s{_rsdReplicaGlobalSecondaryIndexSettings = a}) . _Default . _Coerce

-- | The region name of the replica.
rsdRegionName :: Lens' ReplicaSettingsDescription Text
rsdRegionName = lens _rsdRegionName (\ s a -> s{_rsdRegionName = a})

instance FromJSON ReplicaSettingsDescription where
        parseJSON
          = withObject "ReplicaSettingsDescription"
              (\ x ->
                 ReplicaSettingsDescription' <$>
                   (x .:? "ReplicaStatus") <*>
                     (x .:? "ReplicaProvisionedReadCapacityUnits")
                     <*> (x .:? "ReplicaProvisionedWriteCapacityUnits")
                     <*>
                     (x .:? "ReplicaGlobalSecondaryIndexSettings" .!=
                        mempty)
                     <*> (x .: "RegionName"))

instance Hashable ReplicaSettingsDescription where

instance NFData ReplicaSettingsDescription where

-- | Represents the settings for a global table in a region that will be modified.
--
--
--
-- /See:/ 'replicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { _rsuReplicaProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rsuReplicaGlobalSecondaryIndexSettingsUpdate :: !(Maybe (List1 ReplicaGlobalSecondaryIndexSettingsUpdate))
  , _rsuRegionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsuReplicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'rsuReplicaGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table that will be modified.
--
-- * 'rsuRegionName' - The region of the replica to be added.
replicaSettingsUpdate
    :: Text -- ^ 'rsuRegionName'
    -> ReplicaSettingsUpdate
replicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { _rsuReplicaProvisionedReadCapacityUnits = Nothing
    , _rsuReplicaGlobalSecondaryIndexSettingsUpdate = Nothing
    , _rsuRegionName = pRegionName_
    }


-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
rsuReplicaProvisionedReadCapacityUnits :: Lens' ReplicaSettingsUpdate (Maybe Natural)
rsuReplicaProvisionedReadCapacityUnits = lens _rsuReplicaProvisionedReadCapacityUnits (\ s a -> s{_rsuReplicaProvisionedReadCapacityUnits = a}) . mapping _Nat

-- | Represents the settings of a global secondary index for a global table that will be modified.
rsuReplicaGlobalSecondaryIndexSettingsUpdate :: Lens' ReplicaSettingsUpdate (Maybe (NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
rsuReplicaGlobalSecondaryIndexSettingsUpdate = lens _rsuReplicaGlobalSecondaryIndexSettingsUpdate (\ s a -> s{_rsuReplicaGlobalSecondaryIndexSettingsUpdate = a}) . mapping _List1

-- | The region of the replica to be added.
rsuRegionName :: Lens' ReplicaSettingsUpdate Text
rsuRegionName = lens _rsuRegionName (\ s a -> s{_rsuRegionName = a})

instance Hashable ReplicaSettingsUpdate where

instance NFData ReplicaSettingsUpdate where

instance ToJSON ReplicaSettingsUpdate where
        toJSON ReplicaSettingsUpdate'{..}
          = object
              (catMaybes
                 [("ReplicaProvisionedReadCapacityUnits" .=) <$>
                    _rsuReplicaProvisionedReadCapacityUnits,
                  ("ReplicaGlobalSecondaryIndexSettingsUpdate" .=) <$>
                    _rsuReplicaGlobalSecondaryIndexSettingsUpdate,
                  Just ("RegionName" .= _rsuRegionName)])

-- | Represents one of the following:
--
--
--     * A new replica to be added to an existing global table.
--
--     * New parameters for an existing replica.
--
--     * An existing replica to be removed from an existing global table.
--
--
--
--
-- /See:/ 'replicaUpdate' smart constructor.
data ReplicaUpdate = ReplicaUpdate'
  { _ruCreate :: !(Maybe CreateReplicaAction)
  , _ruDelete :: !(Maybe DeleteReplicaAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruCreate' - The parameters required for creating a replica on an existing global table.
--
-- * 'ruDelete' - The name of the existing replica to be removed.
replicaUpdate
    :: ReplicaUpdate
replicaUpdate = ReplicaUpdate' {_ruCreate = Nothing, _ruDelete = Nothing}


-- | The parameters required for creating a replica on an existing global table.
ruCreate :: Lens' ReplicaUpdate (Maybe CreateReplicaAction)
ruCreate = lens _ruCreate (\ s a -> s{_ruCreate = a})

-- | The name of the existing replica to be removed.
ruDelete :: Lens' ReplicaUpdate (Maybe DeleteReplicaAction)
ruDelete = lens _ruDelete (\ s a -> s{_ruDelete = a})

instance Hashable ReplicaUpdate where

instance NFData ReplicaUpdate where

instance ToJSON ReplicaUpdate where
        toJSON ReplicaUpdate'{..}
          = object
              (catMaybes
                 [("Create" .=) <$> _ruCreate,
                  ("Delete" .=) <$> _ruDelete])

-- | Contains details for the restore.
--
--
--
-- /See:/ 'restoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { _rsSourceTableARN    :: !(Maybe Text)
  , _rsSourceBackupARN   :: !(Maybe Text)
  , _rsRestoreDateTime   :: !POSIX
  , _rsRestoreInProgress :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsSourceTableARN' - ARN of the source table of the backup that is being restored.
--
-- * 'rsSourceBackupARN' - ARN of the backup from which the table was restored.
--
-- * 'rsRestoreDateTime' - Point in time or source backup time.
--
-- * 'rsRestoreInProgress' - Indicates if a restore is in progress or not.
restoreSummary
    :: UTCTime -- ^ 'rsRestoreDateTime'
    -> Bool -- ^ 'rsRestoreInProgress'
    -> RestoreSummary
restoreSummary pRestoreDateTime_ pRestoreInProgress_ =
  RestoreSummary'
    { _rsSourceTableARN = Nothing
    , _rsSourceBackupARN = Nothing
    , _rsRestoreDateTime = _Time # pRestoreDateTime_
    , _rsRestoreInProgress = pRestoreInProgress_
    }


-- | ARN of the source table of the backup that is being restored.
rsSourceTableARN :: Lens' RestoreSummary (Maybe Text)
rsSourceTableARN = lens _rsSourceTableARN (\ s a -> s{_rsSourceTableARN = a})

-- | ARN of the backup from which the table was restored.
rsSourceBackupARN :: Lens' RestoreSummary (Maybe Text)
rsSourceBackupARN = lens _rsSourceBackupARN (\ s a -> s{_rsSourceBackupARN = a})

-- | Point in time or source backup time.
rsRestoreDateTime :: Lens' RestoreSummary UTCTime
rsRestoreDateTime = lens _rsRestoreDateTime (\ s a -> s{_rsRestoreDateTime = a}) . _Time

-- | Indicates if a restore is in progress or not.
rsRestoreInProgress :: Lens' RestoreSummary Bool
rsRestoreInProgress = lens _rsRestoreInProgress (\ s a -> s{_rsRestoreInProgress = a})

instance FromJSON RestoreSummary where
        parseJSON
          = withObject "RestoreSummary"
              (\ x ->
                 RestoreSummary' <$>
                   (x .:? "SourceTableArn") <*>
                     (x .:? "SourceBackupArn")
                     <*> (x .: "RestoreDateTime")
                     <*> (x .: "RestoreInProgress"))

instance Hashable RestoreSummary where

instance NFData RestoreSummary where

-- | The description of the server-side encryption status on the specified table.
--
--
--
-- /See:/ 'sSEDescription' smart constructor.
newtype SSEDescription = SSEDescription'
  { _ssedStatus :: Maybe SSEStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssedStatus' - The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.
sSEDescription
    :: SSEDescription
sSEDescription = SSEDescription' {_ssedStatus = Nothing}


-- | The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.
ssedStatus :: Lens' SSEDescription (Maybe SSEStatus)
ssedStatus = lens _ssedStatus (\ s a -> s{_ssedStatus = a})

instance FromJSON SSEDescription where
        parseJSON
          = withObject "SSEDescription"
              (\ x -> SSEDescription' <$> (x .:? "Status"))

instance Hashable SSEDescription where

instance NFData SSEDescription where

-- | Represents the settings used to enable server-side encryption.
--
--
--
-- /See:/ 'sSESpecification' smart constructor.
newtype SSESpecification = SSESpecification'
  { _ssesEnabled :: Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSESpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssesEnabled' - Indicates whether server-side encryption is enabled (true) or disabled (false) on the table.
sSESpecification
    :: Bool -- ^ 'ssesEnabled'
    -> SSESpecification
sSESpecification pEnabled_ = SSESpecification' {_ssesEnabled = pEnabled_}


-- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the table.
ssesEnabled :: Lens' SSESpecification Bool
ssesEnabled = lens _ssesEnabled (\ s a -> s{_ssesEnabled = a})

instance Hashable SSESpecification where

instance NFData SSESpecification where

instance ToJSON SSESpecification where
        toJSON SSESpecification'{..}
          = object
              (catMaybes [Just ("Enabled" .= _ssesEnabled)])

-- | Contains the details of the table when the backup was created.
--
--
--
-- /See:/ 'sourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { _stdTableSizeBytes        :: !(Maybe Integer)
  , _stdTableARN              :: !(Maybe Text)
  , _stdItemCount             :: !(Maybe Nat)
  , _stdTableName             :: !Text
  , _stdTableId               :: !Text
  , _stdKeySchema             :: !(List1 KeySchemaElement)
  , _stdTableCreationDateTime :: !POSIX
  , _stdProvisionedThroughput :: !ProvisionedThroughput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceTableDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdTableSizeBytes' - Size of the table in bytes. Please note this is an approximate value.
--
-- * 'stdTableARN' - ARN of the table for which backup was created.
--
-- * 'stdItemCount' - Number of items in the table. Please note this is an approximate value.
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
sourceTableDetails
    :: Text -- ^ 'stdTableName'
    -> Text -- ^ 'stdTableId'
    -> NonEmpty KeySchemaElement -- ^ 'stdKeySchema'
    -> UTCTime -- ^ 'stdTableCreationDateTime'
    -> ProvisionedThroughput -- ^ 'stdProvisionedThroughput'
    -> SourceTableDetails
sourceTableDetails pTableName_ pTableId_ pKeySchema_ pTableCreationDateTime_ pProvisionedThroughput_ =
  SourceTableDetails'
    { _stdTableSizeBytes = Nothing
    , _stdTableARN = Nothing
    , _stdItemCount = Nothing
    , _stdTableName = pTableName_
    , _stdTableId = pTableId_
    , _stdKeySchema = _List1 # pKeySchema_
    , _stdTableCreationDateTime = _Time # pTableCreationDateTime_
    , _stdProvisionedThroughput = pProvisionedThroughput_
    }


-- | Size of the table in bytes. Please note this is an approximate value.
stdTableSizeBytes :: Lens' SourceTableDetails (Maybe Integer)
stdTableSizeBytes = lens _stdTableSizeBytes (\ s a -> s{_stdTableSizeBytes = a})

-- | ARN of the table for which backup was created.
stdTableARN :: Lens' SourceTableDetails (Maybe Text)
stdTableARN = lens _stdTableARN (\ s a -> s{_stdTableARN = a})

-- | Number of items in the table. Please note this is an approximate value.
stdItemCount :: Lens' SourceTableDetails (Maybe Natural)
stdItemCount = lens _stdItemCount (\ s a -> s{_stdItemCount = a}) . mapping _Nat

-- | The name of the table for which the backup was created.
stdTableName :: Lens' SourceTableDetails Text
stdTableName = lens _stdTableName (\ s a -> s{_stdTableName = a})

-- | Unique identifier for the table for which the backup was created.
stdTableId :: Lens' SourceTableDetails Text
stdTableId = lens _stdTableId (\ s a -> s{_stdTableId = a})

-- | Schema of the table.
stdKeySchema :: Lens' SourceTableDetails (NonEmpty KeySchemaElement)
stdKeySchema = lens _stdKeySchema (\ s a -> s{_stdKeySchema = a}) . _List1

-- | Time when the source table was created.
stdTableCreationDateTime :: Lens' SourceTableDetails UTCTime
stdTableCreationDateTime = lens _stdTableCreationDateTime (\ s a -> s{_stdTableCreationDateTime = a}) . _Time

-- | Read IOPs and Write IOPS on the table when the backup was created.
stdProvisionedThroughput :: Lens' SourceTableDetails ProvisionedThroughput
stdProvisionedThroughput = lens _stdProvisionedThroughput (\ s a -> s{_stdProvisionedThroughput = a})

instance FromJSON SourceTableDetails where
        parseJSON
          = withObject "SourceTableDetails"
              (\ x ->
                 SourceTableDetails' <$>
                   (x .:? "TableSizeBytes") <*> (x .:? "TableArn") <*>
                     (x .:? "ItemCount")
                     <*> (x .: "TableName")
                     <*> (x .: "TableId")
                     <*> (x .: "KeySchema")
                     <*> (x .: "TableCreationDateTime")
                     <*> (x .: "ProvisionedThroughput"))

instance Hashable SourceTableDetails where

instance NFData SourceTableDetails where

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
--
--
--
-- /See:/ 'sourceTableFeatureDetails' smart constructor.
data SourceTableFeatureDetails = SourceTableFeatureDetails'
  { _stfdStreamDescription      :: !(Maybe StreamSpecification)
  , _stfdGlobalSecondaryIndexes :: !(Maybe [GlobalSecondaryIndexInfo])
  , _stfdLocalSecondaryIndexes  :: !(Maybe [LocalSecondaryIndexInfo])
  , _stfdSSEDescription         :: !(Maybe SSEDescription)
  , _stfdTimeToLiveDescription  :: !(Maybe TimeToLiveDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceTableFeatureDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfdStreamDescription' - Stream settings on the table when the backup was created.
--
-- * 'stfdGlobalSecondaryIndexes' - Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection and ProvisionedThroughput for the GSIs on the table at the time of backup.
--
-- * 'stfdLocalSecondaryIndexes' - Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup.
--
-- * 'stfdSSEDescription' - The description of the server-side encryption status on the table when the backup was created.
--
-- * 'stfdTimeToLiveDescription' - Time to Live settings on the table when the backup was created.
sourceTableFeatureDetails
    :: SourceTableFeatureDetails
sourceTableFeatureDetails =
  SourceTableFeatureDetails'
    { _stfdStreamDescription = Nothing
    , _stfdGlobalSecondaryIndexes = Nothing
    , _stfdLocalSecondaryIndexes = Nothing
    , _stfdSSEDescription = Nothing
    , _stfdTimeToLiveDescription = Nothing
    }


-- | Stream settings on the table when the backup was created.
stfdStreamDescription :: Lens' SourceTableFeatureDetails (Maybe StreamSpecification)
stfdStreamDescription = lens _stfdStreamDescription (\ s a -> s{_stfdStreamDescription = a})

-- | Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection and ProvisionedThroughput for the GSIs on the table at the time of backup.
stfdGlobalSecondaryIndexes :: Lens' SourceTableFeatureDetails [GlobalSecondaryIndexInfo]
stfdGlobalSecondaryIndexes = lens _stfdGlobalSecondaryIndexes (\ s a -> s{_stfdGlobalSecondaryIndexes = a}) . _Default . _Coerce

-- | Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup.
stfdLocalSecondaryIndexes :: Lens' SourceTableFeatureDetails [LocalSecondaryIndexInfo]
stfdLocalSecondaryIndexes = lens _stfdLocalSecondaryIndexes (\ s a -> s{_stfdLocalSecondaryIndexes = a}) . _Default . _Coerce

-- | The description of the server-side encryption status on the table when the backup was created.
stfdSSEDescription :: Lens' SourceTableFeatureDetails (Maybe SSEDescription)
stfdSSEDescription = lens _stfdSSEDescription (\ s a -> s{_stfdSSEDescription = a})

-- | Time to Live settings on the table when the backup was created.
stfdTimeToLiveDescription :: Lens' SourceTableFeatureDetails (Maybe TimeToLiveDescription)
stfdTimeToLiveDescription = lens _stfdTimeToLiveDescription (\ s a -> s{_stfdTimeToLiveDescription = a})

instance FromJSON SourceTableFeatureDetails where
        parseJSON
          = withObject "SourceTableFeatureDetails"
              (\ x ->
                 SourceTableFeatureDetails' <$>
                   (x .:? "StreamDescription") <*>
                     (x .:? "GlobalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "SSEDescription")
                     <*> (x .:? "TimeToLiveDescription"))

instance Hashable SourceTableFeatureDetails where

instance NFData SourceTableFeatureDetails where

-- | Represents the DynamoDB Streams configuration for a table in DynamoDB.
--
--
--
-- /See:/ 'streamSpecification' smart constructor.
data StreamSpecification = StreamSpecification'
  { _ssStreamViewType :: !(Maybe StreamViewType)
  , _ssStreamEnabled  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamViewType' - When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
-- * 'ssStreamEnabled' - Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
streamSpecification
    :: StreamSpecification
streamSpecification =
  StreamSpecification' {_ssStreamViewType = Nothing, _ssStreamEnabled = Nothing}


-- | When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
ssStreamViewType :: Lens' StreamSpecification (Maybe StreamViewType)
ssStreamViewType = lens _ssStreamViewType (\ s a -> s{_ssStreamViewType = a})

-- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
ssStreamEnabled :: Lens' StreamSpecification (Maybe Bool)
ssStreamEnabled = lens _ssStreamEnabled (\ s a -> s{_ssStreamEnabled = a})

instance FromJSON StreamSpecification where
        parseJSON
          = withObject "StreamSpecification"
              (\ x ->
                 StreamSpecification' <$>
                   (x .:? "StreamViewType") <*> (x .:? "StreamEnabled"))

instance Hashable StreamSpecification where

instance NFData StreamSpecification where

instance ToJSON StreamSpecification where
        toJSON StreamSpecification'{..}
          = object
              (catMaybes
                 [("StreamViewType" .=) <$> _ssStreamViewType,
                  ("StreamEnabled" .=) <$> _ssStreamEnabled])

-- | Represents the properties of a table.
--
--
--
-- /See:/ 'tableDescription' smart constructor.
data TableDescription = TableDescription'
  { _tdRestoreSummary         :: !(Maybe RestoreSummary)
  , _tdTableSizeBytes         :: !(Maybe Integer)
  , _tdAttributeDefinitions   :: !(Maybe [AttributeDefinition])
  , _tdLatestStreamARN        :: !(Maybe Text)
  , _tdProvisionedThroughput  :: !(Maybe ProvisionedThroughputDescription)
  , _tdTableStatus            :: !(Maybe TableStatus)
  , _tdTableARN               :: !(Maybe Text)
  , _tdKeySchema              :: !(Maybe (List1 KeySchemaElement))
  , _tdGlobalSecondaryIndexes :: !(Maybe [GlobalSecondaryIndexDescription])
  , _tdLatestStreamLabel      :: !(Maybe Text)
  , _tdLocalSecondaryIndexes  :: !(Maybe [LocalSecondaryIndexDescription])
  , _tdCreationDateTime       :: !(Maybe POSIX)
  , _tdSSEDescription         :: !(Maybe SSEDescription)
  , _tdTableId                :: !(Maybe Text)
  , _tdItemCount              :: !(Maybe Integer)
  , _tdTableName              :: !(Maybe Text)
  , _tdStreamSpecification    :: !(Maybe StreamSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdRestoreSummary' - Contains details for the restore.
--
-- * 'tdTableSizeBytes' - The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'tdAttributeDefinitions' - An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema. Each @AttributeDefinition@ object in this array is composed of:     * @AttributeName@ - The name of the attribute.     * @AttributeType@ - The data type for the attribute.
--
-- * 'tdLatestStreamARN' - The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
--
-- * 'tdProvisionedThroughput' - The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- * 'tdTableStatus' - The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.
--
-- * 'tdTableARN' - The Amazon Resource Name (ARN) that uniquely identifies the table.
--
-- * 'tdKeySchema' - The primary key structure for the table. Each @KeySchemaElement@ consists of:     * @AttributeName@ - The name of the attribute.     * @KeyType@ - The role of the attribute:     * @HASH@ - partition key     * @RANGE@ - sort key For more information about primary keys, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'tdGlobalSecondaryIndexes' - The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table; it is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)     * @IndexName@ - The name of the global secondary index.     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @IndexStatus@ - The current status of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.  If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- * 'tdLatestStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name.     * the @StreamLabel@ .
--
-- * 'tdLocalSecondaryIndexes' - Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:     * @IndexName@ - The name of the local secondary index.     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value. If the table is in the @DELETING@ state, no information about indexes will be returned.
--
-- * 'tdCreationDateTime' - The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- * 'tdSSEDescription' - The description of the server-side encryption status on the specified table.
--
-- * 'tdTableId' - Unique identifier for the table for which the backup was created.
--
-- * 'tdItemCount' - The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- * 'tdTableName' - The name of the table.
--
-- * 'tdStreamSpecification' - The current DynamoDB Streams configuration for the table.
tableDescription
    :: TableDescription
tableDescription =
  TableDescription'
    { _tdRestoreSummary = Nothing
    , _tdTableSizeBytes = Nothing
    , _tdAttributeDefinitions = Nothing
    , _tdLatestStreamARN = Nothing
    , _tdProvisionedThroughput = Nothing
    , _tdTableStatus = Nothing
    , _tdTableARN = Nothing
    , _tdKeySchema = Nothing
    , _tdGlobalSecondaryIndexes = Nothing
    , _tdLatestStreamLabel = Nothing
    , _tdLocalSecondaryIndexes = Nothing
    , _tdCreationDateTime = Nothing
    , _tdSSEDescription = Nothing
    , _tdTableId = Nothing
    , _tdItemCount = Nothing
    , _tdTableName = Nothing
    , _tdStreamSpecification = Nothing
    }


-- | Contains details for the restore.
tdRestoreSummary :: Lens' TableDescription (Maybe RestoreSummary)
tdRestoreSummary = lens _tdRestoreSummary (\ s a -> s{_tdRestoreSummary = a})

-- | The total size of the specified table, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
tdTableSizeBytes :: Lens' TableDescription (Maybe Integer)
tdTableSizeBytes = lens _tdTableSizeBytes (\ s a -> s{_tdTableSizeBytes = a})

-- | An array of @AttributeDefinition@ objects. Each of these objects describes one attribute in the table and index key schema. Each @AttributeDefinition@ object in this array is composed of:     * @AttributeName@ - The name of the attribute.     * @AttributeType@ - The data type for the attribute.
tdAttributeDefinitions :: Lens' TableDescription [AttributeDefinition]
tdAttributeDefinitions = lens _tdAttributeDefinitions (\ s a -> s{_tdAttributeDefinitions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) that uniquely identifies the latest stream for this table.
tdLatestStreamARN :: Lens' TableDescription (Maybe Text)
tdLatestStreamARN = lens _tdLatestStreamARN (\ s a -> s{_tdLatestStreamARN = a})

-- | The provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
tdProvisionedThroughput :: Lens' TableDescription (Maybe ProvisionedThroughputDescription)
tdProvisionedThroughput = lens _tdProvisionedThroughput (\ s a -> s{_tdProvisionedThroughput = a})

-- | The current state of the table:     * @CREATING@ - The table is being created.     * @UPDATING@ - The table is being updated.     * @DELETING@ - The table is being deleted.     * @ACTIVE@ - The table is ready for use.
tdTableStatus :: Lens' TableDescription (Maybe TableStatus)
tdTableStatus = lens _tdTableStatus (\ s a -> s{_tdTableStatus = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the table.
tdTableARN :: Lens' TableDescription (Maybe Text)
tdTableARN = lens _tdTableARN (\ s a -> s{_tdTableARN = a})

-- | The primary key structure for the table. Each @KeySchemaElement@ consists of:     * @AttributeName@ - The name of the attribute.     * @KeyType@ - The role of the attribute:     * @HASH@ - partition key     * @RANGE@ - sort key For more information about primary keys, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
tdKeySchema :: Lens' TableDescription (Maybe (NonEmpty KeySchemaElement))
tdKeySchema = lens _tdKeySchema (\ s a -> s{_tdKeySchema = a}) . mapping _List1

-- | The global secondary indexes, if any, on the table. Each index is scoped to a given partition key value. Each element is composed of:     * @Backfilling@ - If true, then the index is currently in the backfilling phase. Backfilling occurs only when a new global secondary index is added to the table; it is the process by which DynamoDB populates the new index with data from the table. (This attribute does not appear for indexes that were created during a @CreateTable@ operation.)     * @IndexName@ - The name of the global secondary index.     * @IndexSizeBytes@ - The total size of the global secondary index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @IndexStatus@ - The current status of the global secondary index:     * @CREATING@ - The index is being created.     * @UPDATING@ - The index is being updated.     * @DELETING@ - The index is being deleted.     * @ACTIVE@ - The index is ready for use.     * @ItemCount@ - The number of items in the global secondary index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.      * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @ProvisionedThroughput@ - The provisioned throughput settings for the global secondary index, consisting of read and write capacity units, along with data about increases and decreases.  If the table is in the @DELETING@ state, no information about indexes will be returned.
tdGlobalSecondaryIndexes :: Lens' TableDescription [GlobalSecondaryIndexDescription]
tdGlobalSecondaryIndexes = lens _tdGlobalSecondaryIndexes (\ s a -> s{_tdGlobalSecondaryIndexes = a}) . _Default . _Coerce

-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name.     * the @StreamLabel@ .
tdLatestStreamLabel :: Lens' TableDescription (Maybe Text)
tdLatestStreamLabel = lens _tdLatestStreamLabel (\ s a -> s{_tdLatestStreamLabel = a})

-- | Represents one or more local secondary indexes on the table. Each index is scoped to a given partition key value. Tables with one or more local secondary indexes are subject to an item collection size limit, where the amount of data within a given item collection cannot exceed 10 GB. Each element is composed of:     * @IndexName@ - The name of the local secondary index.     * @KeySchema@ - Specifies the complete index key schema. The attribute names in the key schema must be between 1 and 255 characters (inclusive). The key schema must begin with the same partition key as the table.     * @Projection@ - Specifies attributes that are copied (projected) from the table into the index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. Each attribute specification is composed of:     * @ProjectionType@ - One of the following:     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.     * @INCLUDE@ - Only the specified table attributes are projected into the index. The list of projected attributes are in @NonKeyAttributes@ .     * @ALL@ - All of the table attributes are projected into the index.     * @NonKeyAttributes@ - A list of one or more non-key attribute names that are projected into the secondary index. The total count of attributes provided in @NonKeyAttributes@ , summed across all of the secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.     * @IndexSizeBytes@ - Represents the total size of the index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.     * @ItemCount@ - Represents the number of items in the index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value. If the table is in the @DELETING@ state, no information about indexes will be returned.
tdLocalSecondaryIndexes :: Lens' TableDescription [LocalSecondaryIndexDescription]
tdLocalSecondaryIndexes = lens _tdLocalSecondaryIndexes (\ s a -> s{_tdLocalSecondaryIndexes = a}) . _Default . _Coerce

-- | The date and time when the table was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
tdCreationDateTime :: Lens' TableDescription (Maybe UTCTime)
tdCreationDateTime = lens _tdCreationDateTime (\ s a -> s{_tdCreationDateTime = a}) . mapping _Time

-- | The description of the server-side encryption status on the specified table.
tdSSEDescription :: Lens' TableDescription (Maybe SSEDescription)
tdSSEDescription = lens _tdSSEDescription (\ s a -> s{_tdSSEDescription = a})

-- | Unique identifier for the table for which the backup was created.
tdTableId :: Lens' TableDescription (Maybe Text)
tdTableId = lens _tdTableId (\ s a -> s{_tdTableId = a})

-- | The number of items in the specified table. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
tdItemCount :: Lens' TableDescription (Maybe Integer)
tdItemCount = lens _tdItemCount (\ s a -> s{_tdItemCount = a})

-- | The name of the table.
tdTableName :: Lens' TableDescription (Maybe Text)
tdTableName = lens _tdTableName (\ s a -> s{_tdTableName = a})

-- | The current DynamoDB Streams configuration for the table.
tdStreamSpecification :: Lens' TableDescription (Maybe StreamSpecification)
tdStreamSpecification = lens _tdStreamSpecification (\ s a -> s{_tdStreamSpecification = a})

instance FromJSON TableDescription where
        parseJSON
          = withObject "TableDescription"
              (\ x ->
                 TableDescription' <$>
                   (x .:? "RestoreSummary") <*> (x .:? "TableSizeBytes")
                     <*> (x .:? "AttributeDefinitions" .!= mempty)
                     <*> (x .:? "LatestStreamArn")
                     <*> (x .:? "ProvisionedThroughput")
                     <*> (x .:? "TableStatus")
                     <*> (x .:? "TableArn")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "GlobalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "LatestStreamLabel")
                     <*> (x .:? "LocalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "CreationDateTime")
                     <*> (x .:? "SSEDescription")
                     <*> (x .:? "TableId")
                     <*> (x .:? "ItemCount")
                     <*> (x .:? "TableName")
                     <*> (x .:? "StreamSpecification"))

instance Hashable TableDescription where

instance NFData TableDescription where

-- | Describes a tag. A tag is a key-value pair. You can add up to 50 tags to a single DynamoDB table.
--
--
-- AWS-assigned tag names and values are automatically assigned the aws: prefix, which the user cannot assign. AWS-assigned tag names do not count towards the tag limit of 50. User-assigned tag names have the prefix user: in the Cost Allocation Report. You cannot backdate the application of a tag.
--
-- For an overview on tagging DynamoDB resources, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> in the /Amazon DynamoDB Developer Guide/ .
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key of the tag.Tag keys are case sensitive. Each DynamoDB table can only have up to one tag with the same key. If you try to add an existing tag (same key), the existing tag value will be updated to the new value.
--
-- * 'tagValue' - The value of the tag. Tag values are case-sensitive and can be null.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The key of the tag.Tag keys are case sensitive. Each DynamoDB table can only have up to one tag with the same key. If you try to add an existing tag (same key), the existing tag value will be updated to the new value.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of the tag. Tag values are case-sensitive and can be null.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | The description of the Time to Live (TTL) status on the specified table.
--
--
--
-- /See:/ 'timeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { _ttldTimeToLiveStatus :: !(Maybe TimeToLiveStatus)
  , _ttldAttributeName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeToLiveDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttldTimeToLiveStatus' - The Time to Live status for the table.
--
-- * 'ttldAttributeName' - The name of the Time to Live attribute for items in the table.
timeToLiveDescription
    :: TimeToLiveDescription
timeToLiveDescription =
  TimeToLiveDescription'
    {_ttldTimeToLiveStatus = Nothing, _ttldAttributeName = Nothing}


-- | The Time to Live status for the table.
ttldTimeToLiveStatus :: Lens' TimeToLiveDescription (Maybe TimeToLiveStatus)
ttldTimeToLiveStatus = lens _ttldTimeToLiveStatus (\ s a -> s{_ttldTimeToLiveStatus = a})

-- | The name of the Time to Live attribute for items in the table.
ttldAttributeName :: Lens' TimeToLiveDescription (Maybe Text)
ttldAttributeName = lens _ttldAttributeName (\ s a -> s{_ttldAttributeName = a})

instance FromJSON TimeToLiveDescription where
        parseJSON
          = withObject "TimeToLiveDescription"
              (\ x ->
                 TimeToLiveDescription' <$>
                   (x .:? "TimeToLiveStatus") <*>
                     (x .:? "AttributeName"))

instance Hashable TimeToLiveDescription where

instance NFData TimeToLiveDescription where

-- | Represents the settings used to enable or disable Time to Live for the specified table.
--
--
--
-- /See:/ 'timeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { _ttlsEnabled       :: !Bool
  , _ttlsAttributeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeToLiveSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttlsEnabled' - Indicates whether Time To Live is to be enabled (true) or disabled (false) on the table.
--
-- * 'ttlsAttributeName' - The name of the Time to Live attribute used to store the expiration time for items in the table.
timeToLiveSpecification
    :: Bool -- ^ 'ttlsEnabled'
    -> Text -- ^ 'ttlsAttributeName'
    -> TimeToLiveSpecification
timeToLiveSpecification pEnabled_ pAttributeName_ =
  TimeToLiveSpecification'
    {_ttlsEnabled = pEnabled_, _ttlsAttributeName = pAttributeName_}


-- | Indicates whether Time To Live is to be enabled (true) or disabled (false) on the table.
ttlsEnabled :: Lens' TimeToLiveSpecification Bool
ttlsEnabled = lens _ttlsEnabled (\ s a -> s{_ttlsEnabled = a})

-- | The name of the Time to Live attribute used to store the expiration time for items in the table.
ttlsAttributeName :: Lens' TimeToLiveSpecification Text
ttlsAttributeName = lens _ttlsAttributeName (\ s a -> s{_ttlsAttributeName = a})

instance FromJSON TimeToLiveSpecification where
        parseJSON
          = withObject "TimeToLiveSpecification"
              (\ x ->
                 TimeToLiveSpecification' <$>
                   (x .: "Enabled") <*> (x .: "AttributeName"))

instance Hashable TimeToLiveSpecification where

instance NFData TimeToLiveSpecification where

instance ToJSON TimeToLiveSpecification where
        toJSON TimeToLiveSpecification'{..}
          = object
              (catMaybes
                 [Just ("Enabled" .= _ttlsEnabled),
                  Just ("AttributeName" .= _ttlsAttributeName)])

-- | Represents the new provisioned throughput settings to be applied to a global secondary index.
--
--
--
-- /See:/ 'updateGlobalSecondaryIndexAction' smart constructor.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
  { _ugsiaIndexName             :: !Text
  , _ugsiaProvisionedThroughput :: !ProvisionedThroughput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsiaIndexName' - The name of the global secondary index to be updated.
--
-- * 'ugsiaProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
updateGlobalSecondaryIndexAction
    :: Text -- ^ 'ugsiaIndexName'
    -> ProvisionedThroughput -- ^ 'ugsiaProvisionedThroughput'
    -> UpdateGlobalSecondaryIndexAction
updateGlobalSecondaryIndexAction pIndexName_ pProvisionedThroughput_ =
  UpdateGlobalSecondaryIndexAction'
    { _ugsiaIndexName = pIndexName_
    , _ugsiaProvisionedThroughput = pProvisionedThroughput_
    }


-- | The name of the global secondary index to be updated.
ugsiaIndexName :: Lens' UpdateGlobalSecondaryIndexAction Text
ugsiaIndexName = lens _ugsiaIndexName (\ s a -> s{_ugsiaIndexName = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
ugsiaProvisionedThroughput :: Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput = lens _ugsiaProvisionedThroughput (\ s a -> s{_ugsiaProvisionedThroughput = a})

instance Hashable UpdateGlobalSecondaryIndexAction
         where

instance NFData UpdateGlobalSecondaryIndexAction
         where

instance ToJSON UpdateGlobalSecondaryIndexAction
         where
        toJSON UpdateGlobalSecondaryIndexAction'{..}
          = object
              (catMaybes
                 [Just ("IndexName" .= _ugsiaIndexName),
                  Just
                    ("ProvisionedThroughput" .=
                       _ugsiaProvisionedThroughput)])

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@ . You can only request one of these operations, not both, in a single @WriteRequest@ . If you do need to perform both of these operations, you will need to provide two separate @WriteRequest@ objects.
--
--
--
-- /See:/ 'writeRequest' smart constructor.
data WriteRequest = WriteRequest'
  { _wrDeleteRequest :: !(Maybe DeleteRequest)
  , _wrPutRequest    :: !(Maybe PutRequest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrDeleteRequest' - A request to perform a @DeleteItem@ operation.
--
-- * 'wrPutRequest' - A request to perform a @PutItem@ operation.
writeRequest
    :: WriteRequest
writeRequest =
  WriteRequest' {_wrDeleteRequest = Nothing, _wrPutRequest = Nothing}


-- | A request to perform a @DeleteItem@ operation.
wrDeleteRequest :: Lens' WriteRequest (Maybe DeleteRequest)
wrDeleteRequest = lens _wrDeleteRequest (\ s a -> s{_wrDeleteRequest = a})

-- | A request to perform a @PutItem@ operation.
wrPutRequest :: Lens' WriteRequest (Maybe PutRequest)
wrPutRequest = lens _wrPutRequest (\ s a -> s{_wrPutRequest = a})

instance FromJSON WriteRequest where
        parseJSON
          = withObject "WriteRequest"
              (\ x ->
                 WriteRequest' <$>
                   (x .:? "DeleteRequest") <*> (x .:? "PutRequest"))

instance Hashable WriteRequest where

instance NFData WriteRequest where

instance ToJSON WriteRequest where
        toJSON WriteRequest'{..}
          = object
              (catMaybes
                 [("DeleteRequest" .=) <$> _wrDeleteRequest,
                  ("PutRequest" .=) <$> _wrPutRequest])
