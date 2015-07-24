{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SDB.Types.Sum

-- |
--
-- /See:/ 'attribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAlternateValueEncoding'
--
-- * 'aAlternateNameEncoding'
--
-- * 'aName'
--
-- * 'aValue'
data Attribute = Attribute'
    { _aAlternateValueEncoding :: !(Maybe Text)
    , _aAlternateNameEncoding  :: !(Maybe Text)
    , _aName                   :: !Text
    , _aValue                  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Attribute' smart constructor.
attribute :: Text -> Text -> Attribute
attribute pName_ pValue_ =
    Attribute'
    { _aAlternateValueEncoding = Nothing
    , _aAlternateNameEncoding = Nothing
    , _aName = pName_
    , _aValue = pValue_
    }

-- |
aAlternateValueEncoding :: Lens' Attribute (Maybe Text)
aAlternateValueEncoding = lens _aAlternateValueEncoding (\ s a -> s{_aAlternateValueEncoding = a});

-- |
aAlternateNameEncoding :: Lens' Attribute (Maybe Text)
aAlternateNameEncoding = lens _aAlternateNameEncoding (\ s a -> s{_aAlternateNameEncoding = a});

-- | The name of the attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\ s a -> s{_aName = a});

-- | The value of the attribute.
aValue :: Lens' Attribute Text
aValue = lens _aValue (\ s a -> s{_aValue = a});

instance FromXML Attribute where
        parseXML x
          = Attribute' <$>
              (x .@? "AlternateValueEncoding") <*>
                (x .@? "AlternateNameEncoding")
                <*> (x .@ "Name")
                <*> (x .@ "Value")

instance ToQuery Attribute where
        toQuery Attribute'{..}
          = mconcat
              ["AlternateValueEncoding" =:
                 _aAlternateValueEncoding,
               "AlternateNameEncoding" =: _aAlternateNameEncoding,
               "Name" =: _aName, "Value" =: _aValue]

-- | /See:/ 'deletableItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diAttributes'
--
-- * 'diName'
data DeletableItem = DeletableItem'
    { _diAttributes :: !(Maybe [Attribute])
    , _diName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletableItem' smart constructor.
deletableItem :: Text -> DeletableItem
deletableItem pName_ =
    DeletableItem'
    { _diAttributes = Nothing
    , _diName = pName_
    }

-- | FIXME: Undocumented member.
diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\ s a -> s{_diAttributes = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
diName :: Lens' DeletableItem Text
diName = lens _diName (\ s a -> s{_diName = a});

instance ToQuery DeletableItem where
        toQuery DeletableItem'{..}
          = mconcat
              [toQuery (toQueryList "Attribute" <$> _diAttributes),
               "ItemName" =: _diName]

-- |
--
-- /See:/ 'item' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iAlternateNameEncoding'
--
-- * 'iName'
--
-- * 'iAttributes'
data Item = Item'
    { _iAlternateNameEncoding :: !(Maybe Text)
    , _iName                  :: !Text
    , _iAttributes            :: ![Attribute]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Item' smart constructor.
item :: Text -> Item
item pName_ =
    Item'
    { _iAlternateNameEncoding = Nothing
    , _iName = pName_
    , _iAttributes = mempty
    }

-- |
iAlternateNameEncoding :: Lens' Item (Maybe Text)
iAlternateNameEncoding = lens _iAlternateNameEncoding (\ s a -> s{_iAlternateNameEncoding = a});

-- | The name of the item.
iName :: Lens' Item Text
iName = lens _iName (\ s a -> s{_iName = a});

-- | A list of attributes.
iAttributes :: Lens' Item [Attribute]
iAttributes = lens _iAttributes (\ s a -> s{_iAttributes = a}) . _Coerce;

instance FromXML Item where
        parseXML x
          = Item' <$>
              (x .@? "AlternateNameEncoding") <*> (x .@ "Name") <*>
                (parseXMLList "Attribute" x)

-- |
--
-- /See:/ 'replaceableAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raReplace'
--
-- * 'raName'
--
-- * 'raValue'
data ReplaceableAttribute = ReplaceableAttribute'
    { _raReplace :: !(Maybe Bool)
    , _raName    :: !Text
    , _raValue   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceableAttribute' smart constructor.
replaceableAttribute :: Text -> Text -> ReplaceableAttribute
replaceableAttribute pName_ pValue_ =
    ReplaceableAttribute'
    { _raReplace = Nothing
    , _raName = pName_
    , _raValue = pValue_
    }

-- | A flag specifying whether or not to replace the attribute\/value pair or
-- to add a new attribute\/value pair. The default setting is @false@.
raReplace :: Lens' ReplaceableAttribute (Maybe Bool)
raReplace = lens _raReplace (\ s a -> s{_raReplace = a});

-- | The name of the replaceable attribute.
raName :: Lens' ReplaceableAttribute Text
raName = lens _raName (\ s a -> s{_raName = a});

-- | The value of the replaceable attribute.
raValue :: Lens' ReplaceableAttribute Text
raValue = lens _raValue (\ s a -> s{_raValue = a});

instance ToQuery ReplaceableAttribute where
        toQuery ReplaceableAttribute'{..}
          = mconcat
              ["Replace" =: _raReplace, "Name" =: _raName,
               "Value" =: _raValue]

-- |
--
-- /See:/ 'replaceableItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riName'
--
-- * 'riAttributes'
data ReplaceableItem = ReplaceableItem'
    { _riName       :: !Text
    , _riAttributes :: ![ReplaceableAttribute]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceableItem' smart constructor.
replaceableItem :: Text -> ReplaceableItem
replaceableItem pName_ =
    ReplaceableItem'
    { _riName = pName_
    , _riAttributes = mempty
    }

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\ s a -> s{_riName = a});

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\ s a -> s{_riAttributes = a}) . _Coerce;

instance ToQuery ReplaceableItem where
        toQuery ReplaceableItem'{..}
          = mconcat
              ["ItemName" =: _riName,
               toQueryList "Attribute" _riAttributes]

-- | Specifies the conditions under which data should be updated. If an
-- update condition is specified for a request, the data will only be
-- updated if the condition is satisfied. For example, if an attribute with
-- a specific name and value exists, or if a specific attribute doesn\'t
-- exist.
--
-- /See:/ 'updateCondition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucExists'
--
-- * 'ucValue'
--
-- * 'ucName'
data UpdateCondition = UpdateCondition'
    { _ucExists :: !(Maybe Bool)
    , _ucValue  :: !(Maybe Text)
    , _ucName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateCondition' smart constructor.
updateCondition :: UpdateCondition
updateCondition =
    UpdateCondition'
    { _ucExists = Nothing
    , _ucValue = Nothing
    , _ucName = Nothing
    }

-- | A value specifying whether or not the specified attribute must exist
-- with the specified value in order for the update condition to be
-- satisfied. Specify @true@ if the attribute must exist for the update
-- condition to be satisfied. Specify @false@ if the attribute should not
-- exist in order for the update condition to be satisfied.
ucExists :: Lens' UpdateCondition (Maybe Bool)
ucExists = lens _ucExists (\ s a -> s{_ucExists = a});

-- | The value of an attribute. This value can only be specified when the
-- @Exists@ parameter is equal to @true@.
ucValue :: Lens' UpdateCondition (Maybe Text)
ucValue = lens _ucValue (\ s a -> s{_ucValue = a});

-- | The name of the attribute involved in the condition.
ucName :: Lens' UpdateCondition (Maybe Text)
ucName = lens _ucName (\ s a -> s{_ucName = a});

instance ToQuery UpdateCondition where
        toQuery UpdateCondition'{..}
          = mconcat
              ["Exists" =: _ucExists, "Value" =: _ucValue,
               "Name" =: _ucName]
