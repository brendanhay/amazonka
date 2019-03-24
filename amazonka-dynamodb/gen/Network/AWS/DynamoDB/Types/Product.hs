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
-- * 'avL' - An attribute of type List. For example: @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
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


-- | An attribute of type List. For example: @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
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

-- | Represents the properties of the scaling policy.
--
--
--
-- /See:/ 'autoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { _aspdPolicyName :: !(Maybe Text)
  , _aspdTargetTrackingScalingPolicyConfiguration :: !(Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspdPolicyName' - The name of the scaling policy.
--
-- * 'aspdTargetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
autoScalingPolicyDescription
    :: AutoScalingPolicyDescription
autoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { _aspdPolicyName = Nothing
    , _aspdTargetTrackingScalingPolicyConfiguration = Nothing
    }


-- | The name of the scaling policy.
aspdPolicyName :: Lens' AutoScalingPolicyDescription (Maybe Text)
aspdPolicyName = lens _aspdPolicyName (\ s a -> s{_aspdPolicyName = a})

-- | Represents a target tracking scaling policy configuration.
aspdTargetTrackingScalingPolicyConfiguration :: Lens' AutoScalingPolicyDescription (Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
aspdTargetTrackingScalingPolicyConfiguration = lens _aspdTargetTrackingScalingPolicyConfiguration (\ s a -> s{_aspdTargetTrackingScalingPolicyConfiguration = a})

instance FromJSON AutoScalingPolicyDescription where
        parseJSON
          = withObject "AutoScalingPolicyDescription"
              (\ x ->
                 AutoScalingPolicyDescription' <$>
                   (x .:? "PolicyName") <*>
                     (x .:? "TargetTrackingScalingPolicyConfiguration"))

instance Hashable AutoScalingPolicyDescription where

instance NFData AutoScalingPolicyDescription where

-- | Represents the autoscaling policy to be modified.
--
--
--
-- /See:/ 'autoScalingPolicyUpdate' smart constructor.
data AutoScalingPolicyUpdate = AutoScalingPolicyUpdate'
  { _aspuPolicyName :: !(Maybe Text)
  , _aspuTargetTrackingScalingPolicyConfiguration :: !AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingPolicyUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspuPolicyName' - The name of the scaling policy.
--
-- * 'aspuTargetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
autoScalingPolicyUpdate
    :: AutoScalingTargetTrackingScalingPolicyConfigurationUpdate -- ^ 'aspuTargetTrackingScalingPolicyConfiguration'
    -> AutoScalingPolicyUpdate
autoScalingPolicyUpdate pTargetTrackingScalingPolicyConfiguration_ =
  AutoScalingPolicyUpdate'
    { _aspuPolicyName = Nothing
    , _aspuTargetTrackingScalingPolicyConfiguration =
        pTargetTrackingScalingPolicyConfiguration_
    }


-- | The name of the scaling policy.
aspuPolicyName :: Lens' AutoScalingPolicyUpdate (Maybe Text)
aspuPolicyName = lens _aspuPolicyName (\ s a -> s{_aspuPolicyName = a})

-- | Represents a target tracking scaling policy configuration.
aspuTargetTrackingScalingPolicyConfiguration :: Lens' AutoScalingPolicyUpdate AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
aspuTargetTrackingScalingPolicyConfiguration = lens _aspuTargetTrackingScalingPolicyConfiguration (\ s a -> s{_aspuTargetTrackingScalingPolicyConfiguration = a})

instance Hashable AutoScalingPolicyUpdate where

instance NFData AutoScalingPolicyUpdate where

instance ToJSON AutoScalingPolicyUpdate where
        toJSON AutoScalingPolicyUpdate'{..}
          = object
              (catMaybes
                 [("PolicyName" .=) <$> _aspuPolicyName,
                  Just
                    ("TargetTrackingScalingPolicyConfiguration" .=
                       _aspuTargetTrackingScalingPolicyConfiguration)])

-- | Represents the autoscaling settings for a global table or global secondary index.
--
--
--
-- /See:/ 'autoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { _assdAutoScalingDisabled :: !(Maybe Bool)
  , _assdMinimumUnits        :: !(Maybe Nat)
  , _assdMaximumUnits        :: !(Maybe Nat)
  , _assdScalingPolicies     :: !(Maybe [AutoScalingPolicyDescription])
  , _assdAutoScalingRoleARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assdAutoScalingDisabled' - Disabled autoscaling for this global table or global secondary index.
--
-- * 'assdMinimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- * 'assdMaximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- * 'assdScalingPolicies' - Information about the scaling policies.
--
-- * 'assdAutoScalingRoleARN' - Role ARN used for configuring autoScaling policy.
autoScalingSettingsDescription
    :: AutoScalingSettingsDescription
autoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { _assdAutoScalingDisabled = Nothing
    , _assdMinimumUnits = Nothing
    , _assdMaximumUnits = Nothing
    , _assdScalingPolicies = Nothing
    , _assdAutoScalingRoleARN = Nothing
    }


-- | Disabled autoscaling for this global table or global secondary index.
assdAutoScalingDisabled :: Lens' AutoScalingSettingsDescription (Maybe Bool)
assdAutoScalingDisabled = lens _assdAutoScalingDisabled (\ s a -> s{_assdAutoScalingDisabled = a})

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
assdMinimumUnits :: Lens' AutoScalingSettingsDescription (Maybe Natural)
assdMinimumUnits = lens _assdMinimumUnits (\ s a -> s{_assdMinimumUnits = a}) . mapping _Nat

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
assdMaximumUnits :: Lens' AutoScalingSettingsDescription (Maybe Natural)
assdMaximumUnits = lens _assdMaximumUnits (\ s a -> s{_assdMaximumUnits = a}) . mapping _Nat

-- | Information about the scaling policies.
assdScalingPolicies :: Lens' AutoScalingSettingsDescription [AutoScalingPolicyDescription]
assdScalingPolicies = lens _assdScalingPolicies (\ s a -> s{_assdScalingPolicies = a}) . _Default . _Coerce

-- | Role ARN used for configuring autoScaling policy.
assdAutoScalingRoleARN :: Lens' AutoScalingSettingsDescription (Maybe Text)
assdAutoScalingRoleARN = lens _assdAutoScalingRoleARN (\ s a -> s{_assdAutoScalingRoleARN = a})

instance FromJSON AutoScalingSettingsDescription
         where
        parseJSON
          = withObject "AutoScalingSettingsDescription"
              (\ x ->
                 AutoScalingSettingsDescription' <$>
                   (x .:? "AutoScalingDisabled") <*>
                     (x .:? "MinimumUnits")
                     <*> (x .:? "MaximumUnits")
                     <*> (x .:? "ScalingPolicies" .!= mempty)
                     <*> (x .:? "AutoScalingRoleArn"))

instance Hashable AutoScalingSettingsDescription
         where

instance NFData AutoScalingSettingsDescription where

-- | Represents the autoscaling settings to be modified for a global table or global secondary index.
--
--
--
-- /See:/ 'autoScalingSettingsUpdate' smart constructor.
data AutoScalingSettingsUpdate = AutoScalingSettingsUpdate'
  { _assuAutoScalingDisabled :: !(Maybe Bool)
  , _assuMinimumUnits        :: !(Maybe Nat)
  , _assuScalingPolicyUpdate :: !(Maybe AutoScalingPolicyUpdate)
  , _assuMaximumUnits        :: !(Maybe Nat)
  , _assuAutoScalingRoleARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assuAutoScalingDisabled' - Disabled autoscaling for this global table or global secondary index.
--
-- * 'assuMinimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- * 'assuScalingPolicyUpdate' - The scaling policy to apply for scaling target global table or global secondary index capacity units.
--
-- * 'assuMaximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- * 'assuAutoScalingRoleARN' - Role ARN used for configuring autoscaling policy.
autoScalingSettingsUpdate
    :: AutoScalingSettingsUpdate
autoScalingSettingsUpdate =
  AutoScalingSettingsUpdate'
    { _assuAutoScalingDisabled = Nothing
    , _assuMinimumUnits = Nothing
    , _assuScalingPolicyUpdate = Nothing
    , _assuMaximumUnits = Nothing
    , _assuAutoScalingRoleARN = Nothing
    }


-- | Disabled autoscaling for this global table or global secondary index.
assuAutoScalingDisabled :: Lens' AutoScalingSettingsUpdate (Maybe Bool)
assuAutoScalingDisabled = lens _assuAutoScalingDisabled (\ s a -> s{_assuAutoScalingDisabled = a})

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
assuMinimumUnits :: Lens' AutoScalingSettingsUpdate (Maybe Natural)
assuMinimumUnits = lens _assuMinimumUnits (\ s a -> s{_assuMinimumUnits = a}) . mapping _Nat

-- | The scaling policy to apply for scaling target global table or global secondary index capacity units.
assuScalingPolicyUpdate :: Lens' AutoScalingSettingsUpdate (Maybe AutoScalingPolicyUpdate)
assuScalingPolicyUpdate = lens _assuScalingPolicyUpdate (\ s a -> s{_assuScalingPolicyUpdate = a})

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
assuMaximumUnits :: Lens' AutoScalingSettingsUpdate (Maybe Natural)
assuMaximumUnits = lens _assuMaximumUnits (\ s a -> s{_assuMaximumUnits = a}) . mapping _Nat

-- | Role ARN used for configuring autoscaling policy.
assuAutoScalingRoleARN :: Lens' AutoScalingSettingsUpdate (Maybe Text)
assuAutoScalingRoleARN = lens _assuAutoScalingRoleARN (\ s a -> s{_assuAutoScalingRoleARN = a})

instance Hashable AutoScalingSettingsUpdate where

instance NFData AutoScalingSettingsUpdate where

instance ToJSON AutoScalingSettingsUpdate where
        toJSON AutoScalingSettingsUpdate'{..}
          = object
              (catMaybes
                 [("AutoScalingDisabled" .=) <$>
                    _assuAutoScalingDisabled,
                  ("MinimumUnits" .=) <$> _assuMinimumUnits,
                  ("ScalingPolicyUpdate" .=) <$>
                    _assuScalingPolicyUpdate,
                  ("MaximumUnits" .=) <$> _assuMaximumUnits,
                  ("AutoScalingRoleArn" .=) <$>
                    _assuAutoScalingRoleARN])

-- | Represents the properties of a target tracking scaling policy.
--
--
--
-- /See:/ 'autoScalingTargetTrackingScalingPolicyConfigurationDescription' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationDescription = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
  { _asttspcdScaleInCooldown  :: !(Maybe Int)
  , _asttspcdDisableScaleIn   :: !(Maybe Bool)
  , _asttspcdScaleOutCooldown :: !(Maybe Int)
  , _asttspcdTargetValue      :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asttspcdScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application autoscaling scales out your scalable target immediately.
--
-- * 'asttspcdDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- * 'asttspcdScaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- * 'asttspcdTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationDescription
    :: Double -- ^ 'asttspcdTargetValue'
    -> AutoScalingTargetTrackingScalingPolicyConfigurationDescription
autoScalingTargetTrackingScalingPolicyConfigurationDescription pTargetValue_ =
  AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
    { _asttspcdScaleInCooldown = Nothing
    , _asttspcdDisableScaleIn = Nothing
    , _asttspcdScaleOutCooldown = Nothing
    , _asttspcdTargetValue = pTargetValue_
    }


-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application autoscaling scales out your scalable target immediately.
asttspcdScaleInCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Int)
asttspcdScaleInCooldown = lens _asttspcdScaleInCooldown (\ s a -> s{_asttspcdScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
asttspcdDisableScaleIn :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Bool)
asttspcdDisableScaleIn = lens _asttspcdDisableScaleIn (\ s a -> s{_asttspcdDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
asttspcdScaleOutCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Int)
asttspcdScaleOutCooldown = lens _asttspcdScaleOutCooldown (\ s a -> s{_asttspcdScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
asttspcdTargetValue :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Double
asttspcdTargetValue = lens _asttspcdTargetValue (\ s a -> s{_asttspcdTargetValue = a})

instance FromJSON
           AutoScalingTargetTrackingScalingPolicyConfigurationDescription
         where
        parseJSON
          = withObject
              "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
              (\ x ->
                 AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
                   <$>
                   (x .:? "ScaleInCooldown") <*>
                     (x .:? "DisableScaleIn")
                     <*> (x .:? "ScaleOutCooldown")
                     <*> (x .: "TargetValue"))

instance Hashable
           AutoScalingTargetTrackingScalingPolicyConfigurationDescription
         where

instance NFData
           AutoScalingTargetTrackingScalingPolicyConfigurationDescription
         where

-- | Represents the settings of a target tracking scaling policy that will be modified.
--
--
--
-- /See:/ 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationUpdate = AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
  { _asttspcuScaleInCooldown  :: !(Maybe Int)
  , _asttspcuDisableScaleIn   :: !(Maybe Bool)
  , _asttspcuScaleOutCooldown :: !(Maybe Int)
  , _asttspcuTargetValue      :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asttspcuScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application autoscaling scales out your scalable target immediately.
--
-- * 'asttspcuDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- * 'asttspcuScaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- * 'asttspcuTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationUpdate
    :: Double -- ^ 'asttspcuTargetValue'
    -> AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
autoScalingTargetTrackingScalingPolicyConfigurationUpdate pTargetValue_ =
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
    { _asttspcuScaleInCooldown = Nothing
    , _asttspcuDisableScaleIn = Nothing
    , _asttspcuScaleOutCooldown = Nothing
    , _asttspcuTargetValue = pTargetValue_
    }


-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application autoscaling scales out your scalable target immediately.
asttspcuScaleInCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Int)
asttspcuScaleInCooldown = lens _asttspcuScaleInCooldown (\ s a -> s{_asttspcuScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
asttspcuDisableScaleIn :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Bool)
asttspcuDisableScaleIn = lens _asttspcuDisableScaleIn (\ s a -> s{_asttspcuDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
asttspcuScaleOutCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Int)
asttspcuScaleOutCooldown = lens _asttspcuScaleOutCooldown (\ s a -> s{_asttspcuScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
asttspcuTargetValue :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate Double
asttspcuTargetValue = lens _asttspcuTargetValue (\ s a -> s{_asttspcuTargetValue = a})

instance Hashable
           AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
         where

instance NFData
           AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
         where

instance ToJSON
           AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
         where
        toJSON
          AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'{..}
          = object
              (catMaybes
                 [("ScaleInCooldown" .=) <$> _asttspcuScaleInCooldown,
                  ("DisableScaleIn" .=) <$> _asttspcuDisableScaleIn,
                  ("ScaleOutCooldown" .=) <$>
                    _asttspcuScaleOutCooldown,
                  Just ("TargetValue" .= _asttspcuTargetValue)])

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
  { _bdBackupExpiryDateTime   :: !(Maybe POSIX)
  , _bdBackupSizeBytes        :: !(Maybe Nat)
  , _bdBackupARN              :: !Text
  , _bdBackupName             :: !Text
  , _bdBackupStatus           :: !BackupStatus
  , _bdBackupType             :: !BackupType
  , _bdBackupCreationDateTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBackupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
--
-- * 'bdBackupSizeBytes' - Size of the backup in bytes.
--
-- * 'bdBackupARN' - ARN associated with the backup.
--
-- * 'bdBackupName' - Name of the requested backup.
--
-- * 'bdBackupStatus' - Backup can be in one of the following states: CREATING, ACTIVE, DELETED.
--
-- * 'bdBackupType' - BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
-- * 'bdBackupCreationDateTime' - Time at which the backup was created. This is the request time of the backup.
backupDetails
    :: Text -- ^ 'bdBackupARN'
    -> Text -- ^ 'bdBackupName'
    -> BackupStatus -- ^ 'bdBackupStatus'
    -> BackupType -- ^ 'bdBackupType'
    -> UTCTime -- ^ 'bdBackupCreationDateTime'
    -> BackupDetails
backupDetails pBackupARN_ pBackupName_ pBackupStatus_ pBackupType_ pBackupCreationDateTime_ =
  BackupDetails'
    { _bdBackupExpiryDateTime = Nothing
    , _bdBackupSizeBytes = Nothing
    , _bdBackupARN = pBackupARN_
    , _bdBackupName = pBackupName_
    , _bdBackupStatus = pBackupStatus_
    , _bdBackupType = pBackupType_
    , _bdBackupCreationDateTime = _Time # pBackupCreationDateTime_
    }


-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
bdBackupExpiryDateTime :: Lens' BackupDetails (Maybe UTCTime)
bdBackupExpiryDateTime = lens _bdBackupExpiryDateTime (\ s a -> s{_bdBackupExpiryDateTime = a}) . mapping _Time

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

-- | BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
bdBackupType :: Lens' BackupDetails BackupType
bdBackupType = lens _bdBackupType (\ s a -> s{_bdBackupType = a})

-- | Time at which the backup was created. This is the request time of the backup.
bdBackupCreationDateTime :: Lens' BackupDetails UTCTime
bdBackupCreationDateTime = lens _bdBackupCreationDateTime (\ s a -> s{_bdBackupCreationDateTime = a}) . _Time

instance FromJSON BackupDetails where
        parseJSON
          = withObject "BackupDetails"
              (\ x ->
                 BackupDetails' <$>
                   (x .:? "BackupExpiryDateTime") <*>
                     (x .:? "BackupSizeBytes")
                     <*> (x .: "BackupArn")
                     <*> (x .: "BackupName")
                     <*> (x .: "BackupStatus")
                     <*> (x .: "BackupType")
                     <*> (x .: "BackupCreationDateTime"))

instance Hashable BackupDetails where

instance NFData BackupDetails where

-- | Contains details for the backup.
--
--
--
-- /See:/ 'backupSummary' smart constructor.
data BackupSummary = BackupSummary'
  { _bsBackupExpiryDateTime   :: !(Maybe POSIX)
  , _bsTableARN               :: !(Maybe Text)
  , _bsBackupName             :: !(Maybe Text)
  , _bsBackupStatus           :: !(Maybe BackupStatus)
  , _bsBackupSizeBytes        :: !(Maybe Nat)
  , _bsBackupARN              :: !(Maybe Text)
  , _bsTableId                :: !(Maybe Text)
  , _bsBackupCreationDateTime :: !(Maybe POSIX)
  , _bsBackupType             :: !(Maybe BackupType)
  , _bsTableName              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BackupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsBackupExpiryDateTime' - Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
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
-- * 'bsBackupType' - BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
--
-- * 'bsTableName' - Name of the table.
backupSummary
    :: BackupSummary
backupSummary =
  BackupSummary'
    { _bsBackupExpiryDateTime = Nothing
    , _bsTableARN = Nothing
    , _bsBackupName = Nothing
    , _bsBackupStatus = Nothing
    , _bsBackupSizeBytes = Nothing
    , _bsBackupARN = Nothing
    , _bsTableId = Nothing
    , _bsBackupCreationDateTime = Nothing
    , _bsBackupType = Nothing
    , _bsTableName = Nothing
    }


-- | Time at which the automatic on-demand backup created by DynamoDB will expire. This @SYSTEM@ on-demand backup expires automatically 35 days after its creation.
bsBackupExpiryDateTime :: Lens' BackupSummary (Maybe UTCTime)
bsBackupExpiryDateTime = lens _bsBackupExpiryDateTime (\ s a -> s{_bsBackupExpiryDateTime = a}) . mapping _Time

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

-- | BackupType:     * @USER@ - You create and manage these using the on-demand backup feature.     * @SYSTEM@ - If you delete a table with point-in-time recovery enabled, a @SYSTEM@ backup is automatically created and is retained for 35 days (at no additional cost). System backups allow you to restore the deleted table to the state it was in just before the point of deletion.      * @AWS_BACKUP@ - On-demand backup created by you from AWS Backup service.
bsBackupType :: Lens' BackupSummary (Maybe BackupType)
bsBackupType = lens _bsBackupType (\ s a -> s{_bsBackupType = a})

-- | Name of the table.
bsTableName :: Lens' BackupSummary (Maybe Text)
bsTableName = lens _bsTableName (\ s a -> s{_bsTableName = a})

instance FromJSON BackupSummary where
        parseJSON
          = withObject "BackupSummary"
              (\ x ->
                 BackupSummary' <$>
                   (x .:? "BackupExpiryDateTime") <*> (x .:? "TableArn")
                     <*> (x .:? "BackupName")
                     <*> (x .:? "BackupStatus")
                     <*> (x .:? "BackupSizeBytes")
                     <*> (x .:? "BackupArn")
                     <*> (x .:? "TableId")
                     <*> (x .:? "BackupCreationDateTime")
                     <*> (x .:? "BackupType")
                     <*> (x .:? "TableName"))

instance Hashable BackupSummary where

instance NFData BackupSummary where

-- | Contains the details for the read/write capacity mode.
--
--
--
-- /See:/ 'billingModeSummary' smart constructor.
data BillingModeSummary = BillingModeSummary'
  { _bmsLastUpdateToPayPerRequestDateTime :: !(Maybe POSIX)
  , _bmsBillingMode                       :: !(Maybe BillingMode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BillingModeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsLastUpdateToPayPerRequestDateTime' - Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
--
-- * 'bmsBillingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
billingModeSummary
    :: BillingModeSummary
billingModeSummary =
  BillingModeSummary'
    {_bmsLastUpdateToPayPerRequestDateTime = Nothing, _bmsBillingMode = Nothing}


-- | Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
bmsLastUpdateToPayPerRequestDateTime :: Lens' BillingModeSummary (Maybe UTCTime)
bmsLastUpdateToPayPerRequestDateTime = lens _bmsLastUpdateToPayPerRequestDateTime (\ s a -> s{_bmsLastUpdateToPayPerRequestDateTime = a}) . mapping _Time

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
bmsBillingMode :: Lens' BillingModeSummary (Maybe BillingMode)
bmsBillingMode = lens _bmsBillingMode (\ s a -> s{_bmsBillingMode = a})

instance FromJSON BillingModeSummary where
        parseJSON
          = withObject "BillingModeSummary"
              (\ x ->
                 BillingModeSummary' <$>
                   (x .:? "LastUpdateToPayPerRequestDateTime") <*>
                     (x .:? "BillingMode"))

instance Hashable BillingModeSummary where

instance NFData BillingModeSummary where

-- | Represents the amount of provisioned throughput capacity consumed on a table or an index.
--
--
--
-- /See:/ 'capacity' smart constructor.
data Capacity = Capacity'
  { _capReadCapacityUnits  :: !(Maybe Double)
  , _capCapacityUnits      :: !(Maybe Double)
  , _capWriteCapacityUnits :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Capacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capReadCapacityUnits' - The total number of read capacity units consumed on a table or an index.
--
-- * 'capCapacityUnits' - The total number of capacity units consumed on a table or an index.
--
-- * 'capWriteCapacityUnits' - The total number of write capacity units consumed on a table or an index.
capacity
    :: Capacity
capacity =
  Capacity'
    { _capReadCapacityUnits = Nothing
    , _capCapacityUnits = Nothing
    , _capWriteCapacityUnits = Nothing
    }


-- | The total number of read capacity units consumed on a table or an index.
capReadCapacityUnits :: Lens' Capacity (Maybe Double)
capReadCapacityUnits = lens _capReadCapacityUnits (\ s a -> s{_capReadCapacityUnits = a})

-- | The total number of capacity units consumed on a table or an index.
capCapacityUnits :: Lens' Capacity (Maybe Double)
capCapacityUnits = lens _capCapacityUnits (\ s a -> s{_capCapacityUnits = a})

-- | The total number of write capacity units consumed on a table or an index.
capWriteCapacityUnits :: Lens' Capacity (Maybe Double)
capWriteCapacityUnits = lens _capWriteCapacityUnits (\ s a -> s{_capWriteCapacityUnits = a})

instance FromJSON Capacity where
        parseJSON
          = withObject "Capacity"
              (\ x ->
                 Capacity' <$>
                   (x .:? "ReadCapacityUnits") <*>
                     (x .:? "CapacityUnits")
                     <*> (x .:? "WriteCapacityUnits"))

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

-- | Represents a request to perform a check that an item exists or to check the condition of specific attributes of the item..
--
--
--
-- /See:/ 'conditionCheck' smart constructor.
data ConditionCheck = ConditionCheck'
  { _ccExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _ccExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
  , _ccReturnValuesOnConditionCheckFailure :: !(Maybe ReturnValuesOnConditionCheckFailure)
  , _ccKey :: !(Map Text AttributeValue)
  , _ccTableName :: !Text
  , _ccConditionExpression :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConditionCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'ccExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'ccReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'ccKey' - The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
--
-- * 'ccTableName' - Name of the table for the check item request.
--
-- * 'ccConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
conditionCheck
    :: Text -- ^ 'ccTableName'
    -> Text -- ^ 'ccConditionExpression'
    -> ConditionCheck
conditionCheck pTableName_ pConditionExpression_ =
  ConditionCheck'
    { _ccExpressionAttributeNames = Nothing
    , _ccExpressionAttributeValues = Nothing
    , _ccReturnValuesOnConditionCheckFailure = Nothing
    , _ccKey = mempty
    , _ccTableName = pTableName_
    , _ccConditionExpression = pConditionExpression_
    }


-- | One or more substitution tokens for attribute names in an expression.
ccExpressionAttributeNames :: Lens' ConditionCheck (HashMap Text Text)
ccExpressionAttributeNames = lens _ccExpressionAttributeNames (\ s a -> s{_ccExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
ccExpressionAttributeValues :: Lens' ConditionCheck (HashMap Text AttributeValue)
ccExpressionAttributeValues = lens _ccExpressionAttributeValues (\ s a -> s{_ccExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @ConditionCheck@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
ccReturnValuesOnConditionCheckFailure :: Lens' ConditionCheck (Maybe ReturnValuesOnConditionCheckFailure)
ccReturnValuesOnConditionCheckFailure = lens _ccReturnValuesOnConditionCheckFailure (\ s a -> s{_ccReturnValuesOnConditionCheckFailure = a})

-- | The primary key of the item to be checked. Each element consists of an attribute name and a value for that attribute.
ccKey :: Lens' ConditionCheck (HashMap Text AttributeValue)
ccKey = lens _ccKey (\ s a -> s{_ccKey = a}) . _Map

-- | Name of the table for the check item request.
ccTableName :: Lens' ConditionCheck Text
ccTableName = lens _ccTableName (\ s a -> s{_ccTableName = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
ccConditionExpression :: Lens' ConditionCheck Text
ccConditionExpression = lens _ccConditionExpression (\ s a -> s{_ccConditionExpression = a})

instance Hashable ConditionCheck where

instance NFData ConditionCheck where

instance ToJSON ConditionCheck where
        toJSON ConditionCheck'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _ccExpressionAttributeNames,
                  ("ExpressionAttributeValues" .=) <$>
                    _ccExpressionAttributeValues,
                  ("ReturnValuesOnConditionCheckFailure" .=) <$>
                    _ccReturnValuesOnConditionCheckFailure,
                  Just ("Key" .= _ccKey),
                  Just ("TableName" .= _ccTableName),
                  Just
                    ("ConditionExpression" .= _ccConditionExpression)])

-- | The capacity units consumed by an operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the request asked for it. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
--
--
-- /See:/ 'consumedCapacity' smart constructor.
data ConsumedCapacity = ConsumedCapacity'
  { _cReadCapacityUnits      :: !(Maybe Double)
  , _cGlobalSecondaryIndexes :: !(Maybe (Map Text Capacity))
  , _cCapacityUnits          :: !(Maybe Double)
  , _cWriteCapacityUnits     :: !(Maybe Double)
  , _cLocalSecondaryIndexes  :: !(Maybe (Map Text Capacity))
  , _cTable                  :: !(Maybe Capacity)
  , _cTableName              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConsumedCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cReadCapacityUnits' - The total number of read capacity units consumed by the operation.
--
-- * 'cGlobalSecondaryIndexes' - The amount of throughput consumed on each global index affected by the operation.
--
-- * 'cCapacityUnits' - The total number of capacity units consumed by the operation.
--
-- * 'cWriteCapacityUnits' - The total number of write capacity units consumed by the operation.
--
-- * 'cLocalSecondaryIndexes' - The amount of throughput consumed on each local index affected by the operation.
--
-- * 'cTable' - The amount of throughput consumed on the table affected by the operation.
--
-- * 'cTableName' - The name of the table that was affected by the operation.
consumedCapacity
    :: ConsumedCapacity
consumedCapacity =
  ConsumedCapacity'
    { _cReadCapacityUnits = Nothing
    , _cGlobalSecondaryIndexes = Nothing
    , _cCapacityUnits = Nothing
    , _cWriteCapacityUnits = Nothing
    , _cLocalSecondaryIndexes = Nothing
    , _cTable = Nothing
    , _cTableName = Nothing
    }


-- | The total number of read capacity units consumed by the operation.
cReadCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cReadCapacityUnits = lens _cReadCapacityUnits (\ s a -> s{_cReadCapacityUnits = a})

-- | The amount of throughput consumed on each global index affected by the operation.
cGlobalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
cGlobalSecondaryIndexes = lens _cGlobalSecondaryIndexes (\ s a -> s{_cGlobalSecondaryIndexes = a}) . _Default . _Map

-- | The total number of capacity units consumed by the operation.
cCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cCapacityUnits = lens _cCapacityUnits (\ s a -> s{_cCapacityUnits = a})

-- | The total number of write capacity units consumed by the operation.
cWriteCapacityUnits :: Lens' ConsumedCapacity (Maybe Double)
cWriteCapacityUnits = lens _cWriteCapacityUnits (\ s a -> s{_cWriteCapacityUnits = a})

-- | The amount of throughput consumed on each local index affected by the operation.
cLocalSecondaryIndexes :: Lens' ConsumedCapacity (HashMap Text Capacity)
cLocalSecondaryIndexes = lens _cLocalSecondaryIndexes (\ s a -> s{_cLocalSecondaryIndexes = a}) . _Default . _Map

-- | The amount of throughput consumed on the table affected by the operation.
cTable :: Lens' ConsumedCapacity (Maybe Capacity)
cTable = lens _cTable (\ s a -> s{_cTable = a})

-- | The name of the table that was affected by the operation.
cTableName :: Lens' ConsumedCapacity (Maybe Text)
cTableName = lens _cTableName (\ s a -> s{_cTableName = a})

instance FromJSON ConsumedCapacity where
        parseJSON
          = withObject "ConsumedCapacity"
              (\ x ->
                 ConsumedCapacity' <$>
                   (x .:? "ReadCapacityUnits") <*>
                     (x .:? "GlobalSecondaryIndexes" .!= mempty)
                     <*> (x .:? "CapacityUnits")
                     <*> (x .:? "WriteCapacityUnits")
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
-- * 'cbdContinuousBackupsStatus' - @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
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

-- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
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
  { _cgsiaProvisionedThroughput :: !(Maybe ProvisionedThroughput)
  , _cgsiaIndexName             :: !Text
  , _cgsiaKeySchema             :: !(List1 KeySchemaElement)
  , _cgsiaProjection            :: !Projection
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsiaProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'cgsiaIndexName' - The name of the global secondary index to be created.
--
-- * 'cgsiaKeySchema' - The key schema for the global secondary index.
--
-- * 'cgsiaProjection' - Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
createGlobalSecondaryIndexAction
    :: Text -- ^ 'cgsiaIndexName'
    -> NonEmpty KeySchemaElement -- ^ 'cgsiaKeySchema'
    -> Projection -- ^ 'cgsiaProjection'
    -> CreateGlobalSecondaryIndexAction
createGlobalSecondaryIndexAction pIndexName_ pKeySchema_ pProjection_ =
  CreateGlobalSecondaryIndexAction'
    { _cgsiaProvisionedThroughput = Nothing
    , _cgsiaIndexName = pIndexName_
    , _cgsiaKeySchema = _List1 # pKeySchema_
    , _cgsiaProjection = pProjection_
    }


-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
cgsiaProvisionedThroughput :: Lens' CreateGlobalSecondaryIndexAction (Maybe ProvisionedThroughput)
cgsiaProvisionedThroughput = lens _cgsiaProvisionedThroughput (\ s a -> s{_cgsiaProvisionedThroughput = a})

-- | The name of the global secondary index to be created.
cgsiaIndexName :: Lens' CreateGlobalSecondaryIndexAction Text
cgsiaIndexName = lens _cgsiaIndexName (\ s a -> s{_cgsiaIndexName = a})

-- | The key schema for the global secondary index.
cgsiaKeySchema :: Lens' CreateGlobalSecondaryIndexAction (NonEmpty KeySchemaElement)
cgsiaKeySchema = lens _cgsiaKeySchema (\ s a -> s{_cgsiaKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
cgsiaProjection :: Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = lens _cgsiaProjection (\ s a -> s{_cgsiaProjection = a})

instance Hashable CreateGlobalSecondaryIndexAction
         where

instance NFData CreateGlobalSecondaryIndexAction
         where

instance ToJSON CreateGlobalSecondaryIndexAction
         where
        toJSON CreateGlobalSecondaryIndexAction'{..}
          = object
              (catMaybes
                 [("ProvisionedThroughput" .=) <$>
                    _cgsiaProvisionedThroughput,
                  Just ("IndexName" .= _cgsiaIndexName),
                  Just ("KeySchema" .= _cgsiaKeySchema),
                  Just ("Projection" .= _cgsiaProjection)])

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

-- | Represents a request to perform a @DeleteItem@ operation.
--
--
--
-- /See:/ 'delete'' smart constructor.
data Delete = Delete'
  { _dExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _dExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
  , _dReturnValuesOnConditionCheckFailure :: !(Maybe ReturnValuesOnConditionCheckFailure)
  , _dConditionExpression :: !(Maybe Text)
  , _dKey :: !(Map Text AttributeValue)
  , _dTableName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'dExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'dReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'dConditionExpression' - A condition that must be satisfied in order for a conditional delete to succeed.
--
-- * 'dKey' - The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
--
-- * 'dTableName' - Name of the table in which the item to be deleted resides.
delete'
    :: Text -- ^ 'dTableName'
    -> Delete
delete' pTableName_ =
  Delete'
    { _dExpressionAttributeNames = Nothing
    , _dExpressionAttributeValues = Nothing
    , _dReturnValuesOnConditionCheckFailure = Nothing
    , _dConditionExpression = Nothing
    , _dKey = mempty
    , _dTableName = pTableName_
    }


-- | One or more substitution tokens for attribute names in an expression.
dExpressionAttributeNames :: Lens' Delete (HashMap Text Text)
dExpressionAttributeNames = lens _dExpressionAttributeNames (\ s a -> s{_dExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
dExpressionAttributeValues :: Lens' Delete (HashMap Text AttributeValue)
dExpressionAttributeValues = lens _dExpressionAttributeValues (\ s a -> s{_dExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Delete@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
dReturnValuesOnConditionCheckFailure :: Lens' Delete (Maybe ReturnValuesOnConditionCheckFailure)
dReturnValuesOnConditionCheckFailure = lens _dReturnValuesOnConditionCheckFailure (\ s a -> s{_dReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional delete to succeed.
dConditionExpression :: Lens' Delete (Maybe Text)
dConditionExpression = lens _dConditionExpression (\ s a -> s{_dConditionExpression = a})

-- | The primary key of the item to be deleted. Each element consists of an attribute name and a value for that attribute.
dKey :: Lens' Delete (HashMap Text AttributeValue)
dKey = lens _dKey (\ s a -> s{_dKey = a}) . _Map

-- | Name of the table in which the item to be deleted resides.
dTableName :: Lens' Delete Text
dTableName = lens _dTableName (\ s a -> s{_dTableName = a})

instance Hashable Delete where

instance NFData Delete where

instance ToJSON Delete where
        toJSON Delete'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _dExpressionAttributeNames,
                  ("ExpressionAttributeValues" .=) <$>
                    _dExpressionAttributeValues,
                  ("ReturnValuesOnConditionCheckFailure" .=) <$>
                    _dReturnValuesOnConditionCheckFailure,
                  ("ConditionExpression" .=) <$> _dConditionExpression,
                  Just ("Key" .= _dKey),
                  Just ("TableName" .= _dTableName)])

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

-- | An endpoint information details.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAddress              :: !Text
  , _eCachePeriodInMinutes :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress' - IP address of the endpoint.
--
-- * 'eCachePeriodInMinutes' - Endpoint cache time to live (TTL) value.
endpoint
    :: Text -- ^ 'eAddress'
    -> Integer -- ^ 'eCachePeriodInMinutes'
    -> Endpoint
endpoint pAddress_ pCachePeriodInMinutes_ =
  Endpoint'
    {_eAddress = pAddress_, _eCachePeriodInMinutes = pCachePeriodInMinutes_}


-- | IP address of the endpoint.
eAddress :: Lens' Endpoint Text
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | Endpoint cache time to live (TTL) value.
eCachePeriodInMinutes :: Lens' Endpoint Integer
eCachePeriodInMinutes = lens _eCachePeriodInMinutes (\ s a -> s{_eCachePeriodInMinutes = a})

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x ->
                 Endpoint' <$>
                   (x .: "Address") <*> (x .: "CachePeriodInMinutes"))

instance Hashable Endpoint where

instance NFData Endpoint where

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
-- * 'eavExists' - Causes DynamoDB to evaluate the value before attempting a conditional operation:     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionCheckFailedException@ .     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionCheckFailedException@ . The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied. DynamoDB returns a @ValidationException@ if:     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
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

-- | Causes DynamoDB to evaluate the value before attempting a conditional operation:     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionCheckFailedException@ .     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionCheckFailedException@ . The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied. DynamoDB returns a @ValidationException@ if:     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
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

-- | Specifies an item and related attribute values to retrieve in a @TransactGetItem@ object.
--
--
--
-- /See:/ 'get'' smart constructor.
data Get = Get'
  { _getProjectionExpression     :: !(Maybe Text)
  , _getExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _getKey                      :: !(Map Text AttributeValue)
  , _getTableName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Get' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getProjectionExpression' - A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
--
-- * 'getExpressionAttributeNames' - One or more substitution tokens for attribute names in the ProjectionExpression parameter.
--
-- * 'getKey' - A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
--
-- * 'getTableName' - The name of the table from which to retrieve the specified item.
get'
    :: Text -- ^ 'getTableName'
    -> Get
get' pTableName_ =
  Get'
    { _getProjectionExpression = Nothing
    , _getExpressionAttributeNames = Nothing
    , _getKey = mempty
    , _getTableName = pTableName_
    }


-- | A string that identifies one or more attributes of the specified item to retrieve from the table. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes of the specified item are returned. If any of the requested attributes are not found, they do not appear in the result.
getProjectionExpression :: Lens' Get (Maybe Text)
getProjectionExpression = lens _getProjectionExpression (\ s a -> s{_getProjectionExpression = a})

-- | One or more substitution tokens for attribute names in the ProjectionExpression parameter.
getExpressionAttributeNames :: Lens' Get (HashMap Text Text)
getExpressionAttributeNames = lens _getExpressionAttributeNames (\ s a -> s{_getExpressionAttributeNames = a}) . _Default . _Map

-- | A map of attribute names to @AttributeValue@ objects that specifies the primary key of the item to retrieve.
getKey :: Lens' Get (HashMap Text AttributeValue)
getKey = lens _getKey (\ s a -> s{_getKey = a}) . _Map

-- | The name of the table from which to retrieve the specified item.
getTableName :: Lens' Get Text
getTableName = lens _getTableName (\ s a -> s{_getTableName = a})

instance Hashable Get where

instance NFData Get where

instance ToJSON Get where
        toJSON Get'{..}
          = object
              (catMaybes
                 [("ProjectionExpression" .=) <$>
                    _getProjectionExpression,
                  ("ExpressionAttributeNames" .=) <$>
                    _getExpressionAttributeNames,
                  Just ("Key" .= _getKey),
                  Just ("TableName" .= _getTableName)])

-- | Represents the properties of a global secondary index.
--
--
--
-- /See:/ 'globalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { _gsiProvisionedThroughput :: !(Maybe ProvisionedThroughput)
  , _gsiIndexName             :: !Text
  , _gsiKeySchema             :: !(List1 KeySchemaElement)
  , _gsiProjection            :: !Projection
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'gsiIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- * 'gsiKeySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
--
-- * 'gsiProjection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
globalSecondaryIndex
    :: Text -- ^ 'gsiIndexName'
    -> NonEmpty KeySchemaElement -- ^ 'gsiKeySchema'
    -> Projection -- ^ 'gsiProjection'
    -> GlobalSecondaryIndex
globalSecondaryIndex pIndexName_ pKeySchema_ pProjection_ =
  GlobalSecondaryIndex'
    { _gsiProvisionedThroughput = Nothing
    , _gsiIndexName = pIndexName_
    , _gsiKeySchema = _List1 # pKeySchema_
    , _gsiProjection = pProjection_
    }


-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Limits> in the /Amazon DynamoDB Developer Guide/ .
gsiProvisionedThroughput :: Lens' GlobalSecondaryIndex (Maybe ProvisionedThroughput)
gsiProvisionedThroughput = lens _gsiProvisionedThroughput (\ s a -> s{_gsiProvisionedThroughput = a})

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
gsiIndexName :: Lens' GlobalSecondaryIndex Text
gsiIndexName = lens _gsiIndexName (\ s a -> s{_gsiIndexName = a})

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:     * @HASH@ - partition key     * @RANGE@ - sort key
gsiKeySchema :: Lens' GlobalSecondaryIndex (NonEmpty KeySchemaElement)
gsiKeySchema = lens _gsiKeySchema (\ s a -> s{_gsiKeySchema = a}) . _List1

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
gsiProjection :: Lens' GlobalSecondaryIndex Projection
gsiProjection = lens _gsiProjection (\ s a -> s{_gsiProjection = a})

instance Hashable GlobalSecondaryIndex where

instance NFData GlobalSecondaryIndex where

instance ToJSON GlobalSecondaryIndex where
        toJSON GlobalSecondaryIndex'{..}
          = object
              (catMaybes
                 [("ProvisionedThroughput" .=) <$>
                    _gsiProvisionedThroughput,
                  Just ("IndexName" .= _gsiIndexName),
                  Just ("KeySchema" .= _gsiKeySchema),
                  Just ("Projection" .= _gsiProjection)])

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
  , _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate :: !(Maybe AutoScalingSettingsUpdate)
  , _gtgsisuIndexName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalTableGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgsisuProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
--
-- * 'gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate' - AutoScaling settings for managing a global secondary index's write capacity units.
--
-- * 'gtgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
globalTableGlobalSecondaryIndexSettingsUpdate
    :: Text -- ^ 'gtgsisuIndexName'
    -> GlobalTableGlobalSecondaryIndexSettingsUpdate
globalTableGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  GlobalTableGlobalSecondaryIndexSettingsUpdate'
    { _gtgsisuProvisionedWriteCapacityUnits = Nothing
    , _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = Nothing
    , _gtgsisuIndexName = pIndexName_
    }


-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException.@
gtgsisuProvisionedWriteCapacityUnits :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Maybe Natural)
gtgsisuProvisionedWriteCapacityUnits = lens _gtgsisuProvisionedWriteCapacityUnits (\ s a -> s{_gtgsisuProvisionedWriteCapacityUnits = a}) . mapping _Nat

-- | AutoScaling settings for managing a global secondary index's write capacity units.
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens' GlobalTableGlobalSecondaryIndexSettingsUpdate (Maybe AutoScalingSettingsUpdate)
gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = lens _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate (\ s a -> s{_gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate = a})

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
                  ("ProvisionedWriteCapacityAutoScalingSettingsUpdate"
                     .=)
                    <$>
                    _gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate,
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

-- | Details for the requested item.
--
--
--
-- /See:/ 'itemResponse' smart constructor.
newtype ItemResponse = ItemResponse'
  { _iItem :: Maybe (Map Text AttributeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iItem' - Map of attribute data consisting of the data type and attribute value.
itemResponse
    :: ItemResponse
itemResponse = ItemResponse' {_iItem = Nothing}


-- | Map of attribute data consisting of the data type and attribute value.
iItem :: Lens' ItemResponse (HashMap Text AttributeValue)
iItem = lens _iItem (\ s a -> s{_iItem = a}) . _Default . _Map

instance FromJSON ItemResponse where
        parseJSON
          = withObject "ItemResponse"
              (\ x -> ItemResponse' <$> (x .:? "Item" .!= mempty))

instance Hashable ItemResponse where

instance NFData ItemResponse where

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
-- * 'ptReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- * 'ptWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
provisionedThroughput
    :: Natural -- ^ 'ptReadCapacityUnits'
    -> Natural -- ^ 'ptWriteCapacityUnits'
    -> ProvisionedThroughput
provisionedThroughput pReadCapacityUnits_ pWriteCapacityUnits_ =
  ProvisionedThroughput'
    { _ptReadCapacityUnits = _Nat # pReadCapacityUnits_
    , _ptWriteCapacityUnits = _Nat # pWriteCapacityUnits_
    }


-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits = lens _ptReadCapacityUnits (\ s a -> s{_ptReadCapacityUnits = a}) . _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
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

-- | Represents a request to perform a @PutItem@ operation.
--
--
--
-- /See:/ 'put' smart constructor.
data Put = Put'
  { _pExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _pExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
  , _pReturnValuesOnConditionCheckFailure :: !(Maybe ReturnValuesOnConditionCheckFailure)
  , _pConditionExpression :: !(Maybe Text)
  , _pItem :: !(Map Text AttributeValue)
  , _pTableName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Put' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'pExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'pReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
--
-- * 'pConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
--
-- * 'pItem' - A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
--
-- * 'pTableName' - Name of the table in which to write the item.
put
    :: Text -- ^ 'pTableName'
    -> Put
put pTableName_ =
  Put'
    { _pExpressionAttributeNames = Nothing
    , _pExpressionAttributeValues = Nothing
    , _pReturnValuesOnConditionCheckFailure = Nothing
    , _pConditionExpression = Nothing
    , _pItem = mempty
    , _pTableName = pTableName_
    }


-- | One or more substitution tokens for attribute names in an expression.
pExpressionAttributeNames :: Lens' Put (HashMap Text Text)
pExpressionAttributeNames = lens _pExpressionAttributeNames (\ s a -> s{_pExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
pExpressionAttributeValues :: Lens' Put (HashMap Text AttributeValue)
pExpressionAttributeValues = lens _pExpressionAttributeValues (\ s a -> s{_pExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Put@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE and ALL_OLD.
pReturnValuesOnConditionCheckFailure :: Lens' Put (Maybe ReturnValuesOnConditionCheckFailure)
pReturnValuesOnConditionCheckFailure = lens _pReturnValuesOnConditionCheckFailure (\ s a -> s{_pReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
pConditionExpression :: Lens' Put (Maybe Text)
pConditionExpression = lens _pConditionExpression (\ s a -> s{_pConditionExpression = a})

-- | A map of attribute name to attribute values, representing the primary key of the item to be written by @PutItem@ . All of the table's primary key attributes must be specified, and their data types must match those of the table's key schema. If any attributes are present in the item that are part of an index key schema for the table, their types must match the index key schema.
pItem :: Lens' Put (HashMap Text AttributeValue)
pItem = lens _pItem (\ s a -> s{_pItem = a}) . _Map

-- | Name of the table in which to write the item.
pTableName :: Lens' Put Text
pTableName = lens _pTableName (\ s a -> s{_pTableName = a})

instance Hashable Put where

instance NFData Put where

instance ToJSON Put where
        toJSON Put'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _pExpressionAttributeNames,
                  ("ExpressionAttributeValues" .=) <$>
                    _pExpressionAttributeValues,
                  ("ReturnValuesOnConditionCheckFailure" .=) <$>
                    _pReturnValuesOnConditionCheckFailure,
                  ("ConditionExpression" .=) <$> _pConditionExpression,
                  Just ("Item" .= _pItem),
                  Just ("TableName" .= _pTableName)])

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
  { _rgsisdIndexStatus :: !(Maybe IndexStatus)
  , _rgsisdProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rgsisdProvisionedWriteCapacityUnits :: !(Maybe Nat)
  , _rgsisdProvisionedWriteCapacityAutoScalingSettings :: !(Maybe AutoScalingSettingsDescription)
  , _rgsisdProvisionedReadCapacityAutoScalingSettings :: !(Maybe AutoScalingSettingsDescription)
  , _rgsisdIndexName :: !Text
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
-- * 'rgsisdProvisionedWriteCapacityAutoScalingSettings' - AutoScaling settings for a global secondary index replica's write capacity units.
--
-- * 'rgsisdProvisionedReadCapacityAutoScalingSettings' - Autoscaling settings for a global secondary index replica's read capacity units.
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
    , _rgsisdProvisionedWriteCapacityAutoScalingSettings = Nothing
    , _rgsisdProvisionedReadCapacityAutoScalingSettings = Nothing
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

-- | AutoScaling settings for a global secondary index replica's write capacity units.
rgsisdProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe AutoScalingSettingsDescription)
rgsisdProvisionedWriteCapacityAutoScalingSettings = lens _rgsisdProvisionedWriteCapacityAutoScalingSettings (\ s a -> s{_rgsisdProvisionedWriteCapacityAutoScalingSettings = a})

-- | Autoscaling settings for a global secondary index replica's read capacity units.
rgsisdProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Maybe AutoScalingSettingsDescription)
rgsisdProvisionedReadCapacityAutoScalingSettings = lens _rgsisdProvisionedReadCapacityAutoScalingSettings (\ s a -> s{_rgsisdProvisionedReadCapacityAutoScalingSettings = a})

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
                     <*>
                     (x .:? "ProvisionedWriteCapacityAutoScalingSettings")
                     <*>
                     (x .:? "ProvisionedReadCapacityAutoScalingSettings")
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
  { _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate :: !(Maybe AutoScalingSettingsUpdate)
  , _rgsisuProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rgsisuIndexName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaGlobalSecondaryIndexSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate' - Autoscaling settings for managing a global secondary index replica's read capacity units.
--
-- * 'rgsisuProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'rgsisuIndexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
replicaGlobalSecondaryIndexSettingsUpdate
    :: Text -- ^ 'rgsisuIndexName'
    -> ReplicaGlobalSecondaryIndexSettingsUpdate
replicaGlobalSecondaryIndexSettingsUpdate pIndexName_ =
  ReplicaGlobalSecondaryIndexSettingsUpdate'
    { _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = Nothing
    , _rgsisuProvisionedReadCapacityUnits = Nothing
    , _rgsisuIndexName = pIndexName_
    }


-- | Autoscaling settings for managing a global secondary index replica's read capacity units.
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens' ReplicaGlobalSecondaryIndexSettingsUpdate (Maybe AutoScalingSettingsUpdate)
rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = lens _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate (\ s a -> s{_rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate = a})

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
                 [("ProvisionedReadCapacityAutoScalingSettingsUpdate"
                     .=)
                    <$>
                    _rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate,
                  ("ProvisionedReadCapacityUnits" .=) <$>
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
  , _rsdReplicaBillingModeSummary :: !(Maybe BillingModeSummary)
  , _rsdReplicaGlobalSecondaryIndexSettings :: !(Maybe [ReplicaGlobalSecondaryIndexSettingsDescription])
  , _rsdReplicaProvisionedWriteCapacityAutoScalingSettings :: !(Maybe AutoScalingSettingsDescription)
  , _rsdReplicaProvisionedReadCapacityAutoScalingSettings :: !(Maybe AutoScalingSettingsDescription)
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
-- * 'rsdReplicaBillingModeSummary' - The read/write capacity mode of the replica.
--
-- * 'rsdReplicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
--
-- * 'rsdReplicaProvisionedWriteCapacityAutoScalingSettings' - AutoScaling settings for a global table replica's write capacity units.
--
-- * 'rsdReplicaProvisionedReadCapacityAutoScalingSettings' - Autoscaling settings for a global table replica's read capacity units.
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
    , _rsdReplicaBillingModeSummary = Nothing
    , _rsdReplicaGlobalSecondaryIndexSettings = Nothing
    , _rsdReplicaProvisionedWriteCapacityAutoScalingSettings = Nothing
    , _rsdReplicaProvisionedReadCapacityAutoScalingSettings = Nothing
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

-- | The read/write capacity mode of the replica.
rsdReplicaBillingModeSummary :: Lens' ReplicaSettingsDescription (Maybe BillingModeSummary)
rsdReplicaBillingModeSummary = lens _rsdReplicaBillingModeSummary (\ s a -> s{_rsdReplicaBillingModeSummary = a})

-- | Replica global secondary index settings for the global table.
rsdReplicaGlobalSecondaryIndexSettings :: Lens' ReplicaSettingsDescription [ReplicaGlobalSecondaryIndexSettingsDescription]
rsdReplicaGlobalSecondaryIndexSettings = lens _rsdReplicaGlobalSecondaryIndexSettings (\ s a -> s{_rsdReplicaGlobalSecondaryIndexSettings = a}) . _Default . _Coerce

-- | AutoScaling settings for a global table replica's write capacity units.
rsdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens' ReplicaSettingsDescription (Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedWriteCapacityAutoScalingSettings = lens _rsdReplicaProvisionedWriteCapacityAutoScalingSettings (\ s a -> s{_rsdReplicaProvisionedWriteCapacityAutoScalingSettings = a})

-- | Autoscaling settings for a global table replica's read capacity units.
rsdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens' ReplicaSettingsDescription (Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedReadCapacityAutoScalingSettings = lens _rsdReplicaProvisionedReadCapacityAutoScalingSettings (\ s a -> s{_rsdReplicaProvisionedReadCapacityAutoScalingSettings = a})

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
                     <*> (x .:? "ReplicaBillingModeSummary")
                     <*>
                     (x .:? "ReplicaGlobalSecondaryIndexSettings" .!=
                        mempty)
                     <*>
                     (x .:?
                        "ReplicaProvisionedWriteCapacityAutoScalingSettings")
                     <*>
                     (x .:?
                        "ReplicaProvisionedReadCapacityAutoScalingSettings")
                     <*> (x .: "RegionName"))

instance Hashable ReplicaSettingsDescription where

instance NFData ReplicaSettingsDescription where

-- | Represents the settings for a global table in a region that will be modified.
--
--
--
-- /See:/ 'replicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate :: !(Maybe AutoScalingSettingsUpdate)
  , _rsuReplicaProvisionedReadCapacityUnits :: !(Maybe Nat)
  , _rsuReplicaGlobalSecondaryIndexSettingsUpdate :: !(Maybe (List1 ReplicaGlobalSecondaryIndexSettingsUpdate))
  , _rsuRegionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicaSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate' - Autoscaling settings for managing a global table replica's read capacity units.
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
    { _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = Nothing
    , _rsuReplicaProvisionedReadCapacityUnits = Nothing
    , _rsuReplicaGlobalSecondaryIndexSettingsUpdate = Nothing
    , _rsuRegionName = pRegionName_
    }


-- | Autoscaling settings for managing a global table replica's read capacity units.
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens' ReplicaSettingsUpdate (Maybe AutoScalingSettingsUpdate)
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = lens _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate (\ s a -> s{_rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = a})

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
                 [("ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate"
                     .=)
                    <$>
                    _rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
                  ("ReplicaProvisionedReadCapacityUnits" .=) <$>
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
data SSEDescription = SSEDescription'
  { _ssedStatus          :: !(Maybe SSEStatus)
  , _ssedSSEType         :: !(Maybe SSEType)
  , _ssedKMSMasterKeyARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssedStatus' - The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.     * @UPDATING@ - Server-side encryption is being updated.
--
-- * 'ssedSSEType' - Server-side encryption type:     * @AES256@ - Server-side encryption which uses the AES256 algorithm (not applicable).     * @KMS@ - Server-side encryption which uses AWS Key Management Service. Key is stored in your account and is managed by AWS KMS (KMS charges apply).
--
-- * 'ssedKMSMasterKeyARN' - The KMS master key ARN used for the KMS encryption.
sSEDescription
    :: SSEDescription
sSEDescription =
  SSEDescription'
    { _ssedStatus = Nothing
    , _ssedSSEType = Nothing
    , _ssedKMSMasterKeyARN = Nothing
    }


-- | The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.     * @UPDATING@ - Server-side encryption is being updated.
ssedStatus :: Lens' SSEDescription (Maybe SSEStatus)
ssedStatus = lens _ssedStatus (\ s a -> s{_ssedStatus = a})

-- | Server-side encryption type:     * @AES256@ - Server-side encryption which uses the AES256 algorithm (not applicable).     * @KMS@ - Server-side encryption which uses AWS Key Management Service. Key is stored in your account and is managed by AWS KMS (KMS charges apply).
ssedSSEType :: Lens' SSEDescription (Maybe SSEType)
ssedSSEType = lens _ssedSSEType (\ s a -> s{_ssedSSEType = a})

-- | The KMS master key ARN used for the KMS encryption.
ssedKMSMasterKeyARN :: Lens' SSEDescription (Maybe Text)
ssedKMSMasterKeyARN = lens _ssedKMSMasterKeyARN (\ s a -> s{_ssedKMSMasterKeyARN = a})

instance FromJSON SSEDescription where
        parseJSON
          = withObject "SSEDescription"
              (\ x ->
                 SSEDescription' <$>
                   (x .:? "Status") <*> (x .:? "SSEType") <*>
                     (x .:? "KMSMasterKeyArn"))

instance Hashable SSEDescription where

instance NFData SSEDescription where

-- | Represents the settings used to enable server-side encryption.
--
--
--
-- /See:/ 'sSESpecification' smart constructor.
data SSESpecification = SSESpecification'
  { _ssesEnabled        :: !(Maybe Bool)
  , _ssesKMSMasterKeyId :: !(Maybe Text)
  , _ssesSSEType        :: !(Maybe SSEType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSESpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssesEnabled' - Indicates whether server-side encryption is enabled (true) or disabled (false) on the table. If enabled (true), server-side encryption type is set to @KMS@ . If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
--
-- * 'ssesKMSMasterKeyId' - The KMS Master Key (CMK) which should be used for the KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS Master Key alias/aws/dynamodb.
--
-- * 'ssesSSEType' - Server-side encryption type:     * @AES256@ - Server-side encryption which uses the AES256 algorithm (not applicable).     * @KMS@ - Server-side encryption which uses AWS Key Management Service. Key is stored in your account and is managed by AWS KMS (KMS charges apply).
sSESpecification
    :: SSESpecification
sSESpecification =
  SSESpecification'
    { _ssesEnabled = Nothing
    , _ssesKMSMasterKeyId = Nothing
    , _ssesSSEType = Nothing
    }


-- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the table. If enabled (true), server-side encryption type is set to @KMS@ . If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
ssesEnabled :: Lens' SSESpecification (Maybe Bool)
ssesEnabled = lens _ssesEnabled (\ s a -> s{_ssesEnabled = a})

-- | The KMS Master Key (CMK) which should be used for the KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS Master Key alias/aws/dynamodb.
ssesKMSMasterKeyId :: Lens' SSESpecification (Maybe Text)
ssesKMSMasterKeyId = lens _ssesKMSMasterKeyId (\ s a -> s{_ssesKMSMasterKeyId = a})

-- | Server-side encryption type:     * @AES256@ - Server-side encryption which uses the AES256 algorithm (not applicable).     * @KMS@ - Server-side encryption which uses AWS Key Management Service. Key is stored in your account and is managed by AWS KMS (KMS charges apply).
ssesSSEType :: Lens' SSESpecification (Maybe SSEType)
ssesSSEType = lens _ssesSSEType (\ s a -> s{_ssesSSEType = a})

instance Hashable SSESpecification where

instance NFData SSESpecification where

instance ToJSON SSESpecification where
        toJSON SSESpecification'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _ssesEnabled,
                  ("KMSMasterKeyId" .=) <$> _ssesKMSMasterKeyId,
                  ("SSEType" .=) <$> _ssesSSEType])

-- | Contains the details of the table when the backup was created.
--
--
--
-- /See:/ 'sourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { _stdTableSizeBytes        :: !(Maybe Integer)
  , _stdTableARN              :: !(Maybe Text)
  , _stdBillingMode           :: !(Maybe BillingMode)
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
-- * 'stdBillingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
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
    , _stdBillingMode = Nothing
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

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
stdBillingMode :: Lens' SourceTableDetails (Maybe BillingMode)
stdBillingMode = lens _stdBillingMode (\ s a -> s{_stdBillingMode = a})

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
                     (x .:? "BillingMode")
                     <*> (x .:? "ItemCount")
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
  , _tdBillingModeSummary     :: !(Maybe BillingModeSummary)
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
-- * 'tdBillingModeSummary' - Contains the details for the read/write capacity mode.
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
    , _tdBillingModeSummary = Nothing
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

-- | Contains the details for the read/write capacity mode.
tdBillingModeSummary :: Lens' TableDescription (Maybe BillingModeSummary)
tdBillingModeSummary = lens _tdBillingModeSummary (\ s a -> s{_tdBillingModeSummary = a})

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
                     <*> (x .:? "BillingModeSummary")
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

-- | Specifies an item to be retrieved as part of the transaction.
--
--
--
-- /See:/ 'transactGetItem' smart constructor.
newtype TransactGetItem = TransactGetItem'
  { _tgiGet :: Get
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TransactGetItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgiGet' - Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
transactGetItem
    :: Get -- ^ 'tgiGet'
    -> TransactGetItem
transactGetItem pGet_ = TransactGetItem' {_tgiGet = pGet_}


-- | Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
tgiGet :: Lens' TransactGetItem Get
tgiGet = lens _tgiGet (\ s a -> s{_tgiGet = a})

instance Hashable TransactGetItem where

instance NFData TransactGetItem where

instance ToJSON TransactGetItem where
        toJSON TransactGetItem'{..}
          = object (catMaybes [Just ("Get" .= _tgiGet)])

-- | A list of requests that can perform update, put, delete, or check operations on multiple items in one or more tables atomically.
--
--
--
-- /See:/ 'transactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { _twiConditionCheck :: !(Maybe ConditionCheck)
  , _twiPut            :: !(Maybe Put)
  , _twiDelete         :: !(Maybe Delete)
  , _twiUpdate         :: !(Maybe Update)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TransactWriteItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'twiConditionCheck' - A request to perform a check item operation.
--
-- * 'twiPut' - A request to perform a @PutItem@ operation.
--
-- * 'twiDelete' - A request to perform a @DeleteItem@ operation.
--
-- * 'twiUpdate' - A request to perform an @UpdateItem@ operation.
transactWriteItem
    :: TransactWriteItem
transactWriteItem =
  TransactWriteItem'
    { _twiConditionCheck = Nothing
    , _twiPut = Nothing
    , _twiDelete = Nothing
    , _twiUpdate = Nothing
    }


-- | A request to perform a check item operation.
twiConditionCheck :: Lens' TransactWriteItem (Maybe ConditionCheck)
twiConditionCheck = lens _twiConditionCheck (\ s a -> s{_twiConditionCheck = a})

-- | A request to perform a @PutItem@ operation.
twiPut :: Lens' TransactWriteItem (Maybe Put)
twiPut = lens _twiPut (\ s a -> s{_twiPut = a})

-- | A request to perform a @DeleteItem@ operation.
twiDelete :: Lens' TransactWriteItem (Maybe Delete)
twiDelete = lens _twiDelete (\ s a -> s{_twiDelete = a})

-- | A request to perform an @UpdateItem@ operation.
twiUpdate :: Lens' TransactWriteItem (Maybe Update)
twiUpdate = lens _twiUpdate (\ s a -> s{_twiUpdate = a})

instance Hashable TransactWriteItem where

instance NFData TransactWriteItem where

instance ToJSON TransactWriteItem where
        toJSON TransactWriteItem'{..}
          = object
              (catMaybes
                 [("ConditionCheck" .=) <$> _twiConditionCheck,
                  ("Put" .=) <$> _twiPut, ("Delete" .=) <$> _twiDelete,
                  ("Update" .=) <$> _twiUpdate])

-- | Represents a request to perform an @UpdateItem@ operation.
--
--
--
-- /See:/ 'update' smart constructor.
data Update = Update'
  { _uExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _uExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
  , _uReturnValuesOnConditionCheckFailure :: !(Maybe ReturnValuesOnConditionCheckFailure)
  , _uConditionExpression :: !(Maybe Text)
  , _uKey :: !(Map Text AttributeValue)
  , _uUpdateExpression :: !Text
  , _uTableName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Update' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
--
-- * 'uExpressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- * 'uReturnValuesOnConditionCheckFailure' - Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
--
-- * 'uConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed.
--
-- * 'uKey' - The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
--
-- * 'uUpdateExpression' - An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
--
-- * 'uTableName' - Name of the table for the @UpdateItem@ request.
update
    :: Text -- ^ 'uUpdateExpression'
    -> Text -- ^ 'uTableName'
    -> Update
update pUpdateExpression_ pTableName_ =
  Update'
    { _uExpressionAttributeNames = Nothing
    , _uExpressionAttributeValues = Nothing
    , _uReturnValuesOnConditionCheckFailure = Nothing
    , _uConditionExpression = Nothing
    , _uKey = mempty
    , _uUpdateExpression = pUpdateExpression_
    , _uTableName = pTableName_
    }


-- | One or more substitution tokens for attribute names in an expression.
uExpressionAttributeNames :: Lens' Update (HashMap Text Text)
uExpressionAttributeNames = lens _uExpressionAttributeNames (\ s a -> s{_uExpressionAttributeNames = a}) . _Default . _Map

-- | One or more values that can be substituted in an expression.
uExpressionAttributeValues :: Lens' Update (HashMap Text AttributeValue)
uExpressionAttributeValues = lens _uExpressionAttributeValues (\ s a -> s{_uExpressionAttributeValues = a}) . _Default . _Map

-- | Use @ReturnValuesOnConditionCheckFailure@ to get the item attributes if the @Update@ condition fails. For @ReturnValuesOnConditionCheckFailure@ , the valid values are: NONE, ALL_OLD, UPDATED_OLD, ALL_NEW, UPDATED_NEW.
uReturnValuesOnConditionCheckFailure :: Lens' Update (Maybe ReturnValuesOnConditionCheckFailure)
uReturnValuesOnConditionCheckFailure = lens _uReturnValuesOnConditionCheckFailure (\ s a -> s{_uReturnValuesOnConditionCheckFailure = a})

-- | A condition that must be satisfied in order for a conditional update to succeed.
uConditionExpression :: Lens' Update (Maybe Text)
uConditionExpression = lens _uConditionExpression (\ s a -> s{_uConditionExpression = a})

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
uKey :: Lens' Update (HashMap Text AttributeValue)
uKey = lens _uKey (\ s a -> s{_uKey = a}) . _Map

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them.
uUpdateExpression :: Lens' Update Text
uUpdateExpression = lens _uUpdateExpression (\ s a -> s{_uUpdateExpression = a})

-- | Name of the table for the @UpdateItem@ request.
uTableName :: Lens' Update Text
uTableName = lens _uTableName (\ s a -> s{_uTableName = a})

instance Hashable Update where

instance NFData Update where

instance ToJSON Update where
        toJSON Update'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _uExpressionAttributeNames,
                  ("ExpressionAttributeValues" .=) <$>
                    _uExpressionAttributeValues,
                  ("ReturnValuesOnConditionCheckFailure" .=) <$>
                    _uReturnValuesOnConditionCheckFailure,
                  ("ConditionExpression" .=) <$> _uConditionExpression,
                  Just ("Key" .= _uKey),
                  Just ("UpdateExpression" .= _uUpdateExpression),
                  Just ("TableName" .= _uTableName)])

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
