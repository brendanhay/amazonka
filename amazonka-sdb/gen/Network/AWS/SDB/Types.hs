{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.SDB.Types
    (
    -- * Service
      SDB
    -- ** Errors
    , RESTError

    -- * Attribute
    , Attribute
    , attribute
    , attAlternateValueEncoding
    , attAlternateNameEncoding
    , attName
    , attValue

    -- * DeletableItem
    , DeletableItem
    , deletableItem
    , diAttributes
    , diName

    -- * Item
    , Item
    , item
    , iteAlternateNameEncoding
    , iteName
    , iteAttributes

    -- * ReplaceableAttribute
    , ReplaceableAttribute
    , replaceableAttribute
    , raReplace
    , raName
    , raValue

    -- * ReplaceableItem
    , ReplaceableItem
    , replaceableItem
    , riName
    , riAttributes

    -- * UpdateCondition
    , UpdateCondition
    , updateCondition
    , ucExists
    , ucValue
    , ucName
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V2

-- | Version @2009-04-15@ of the Amazon SimpleDB SDK.
data SDB

instance AWSService SDB where
    type Sg SDB = V2
    type Er SDB = RESTError

    service = service'
      where
        service' :: Service SDB
        service' = Service
            { _svcAbbrev  = "SDB"
            , _svcPrefix  = "sdb"
            , _svcVersion = "2009-04-15"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry SDB
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'attribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attAlternateValueEncoding'
--
-- * 'attAlternateNameEncoding'
--
-- * 'attName'
--
-- * 'attValue'
data Attribute = Attribute'{_attAlternateValueEncoding :: Maybe Text, _attAlternateNameEncoding :: Maybe Text, _attName :: Text, _attValue :: Text} deriving (Eq, Read, Show)

-- | 'Attribute' smart constructor.
attribute :: Text -> Text -> Attribute
attribute pName pValue = Attribute'{_attAlternateValueEncoding = Nothing, _attAlternateNameEncoding = Nothing, _attName = pName, _attValue = pValue};

-- |
attAlternateValueEncoding :: Lens' Attribute (Maybe Text)
attAlternateValueEncoding = lens _attAlternateValueEncoding (\ s a -> s{_attAlternateValueEncoding = a});

-- |
attAlternateNameEncoding :: Lens' Attribute (Maybe Text)
attAlternateNameEncoding = lens _attAlternateNameEncoding (\ s a -> s{_attAlternateNameEncoding = a});

-- | The name of the attribute.
attName :: Lens' Attribute Text
attName = lens _attName (\ s a -> s{_attName = a});

-- | The value of the attribute.
attValue :: Lens' Attribute Text
attValue = lens _attValue (\ s a -> s{_attValue = a});

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
                 _attAlternateValueEncoding,
               "AlternateNameEncoding" =: _attAlternateNameEncoding,
               "Name" =: _attName, "Value" =: _attValue]

-- | /See:/ 'deletableItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diAttributes'
--
-- * 'diName'
data DeletableItem = DeletableItem'{_diAttributes :: Maybe [Attribute], _diName :: Text} deriving (Eq, Read, Show)

-- | 'DeletableItem' smart constructor.
deletableItem :: Text -> DeletableItem
deletableItem pName = DeletableItem'{_diAttributes = Nothing, _diName = pName};

-- | FIXME: Undocumented member.
diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\ s a -> s{_diAttributes = a}) . _Default;

-- | FIXME: Undocumented member.
diName :: Lens' DeletableItem Text
diName = lens _diName (\ s a -> s{_diName = a});

instance ToQuery DeletableItem where
        toQuery DeletableItem'{..}
          = mconcat
              [toQuery (toQueryList "Attribute" <$> _diAttributes),
               "ItemName" =: _diName]

-- | /See:/ 'item' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iteAlternateNameEncoding'
--
-- * 'iteName'
--
-- * 'iteAttributes'
data Item = Item'{_iteAlternateNameEncoding :: Maybe Text, _iteName :: Text, _iteAttributes :: [Attribute]} deriving (Eq, Read, Show)

-- | 'Item' smart constructor.
item :: Text -> Item
item pName = Item'{_iteAlternateNameEncoding = Nothing, _iteName = pName, _iteAttributes = mempty};

-- |
iteAlternateNameEncoding :: Lens' Item (Maybe Text)
iteAlternateNameEncoding = lens _iteAlternateNameEncoding (\ s a -> s{_iteAlternateNameEncoding = a});

-- | The name of the item.
iteName :: Lens' Item Text
iteName = lens _iteName (\ s a -> s{_iteName = a});

-- | A list of attributes.
iteAttributes :: Lens' Item [Attribute]
iteAttributes = lens _iteAttributes (\ s a -> s{_iteAttributes = a});

instance FromXML Item where
        parseXML x
          = Item' <$>
              (x .@? "AlternateNameEncoding") <*> (x .@ "Name") <*>
                (parseXMLList "Attribute" x)

-- | /See:/ 'replaceableAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raReplace'
--
-- * 'raName'
--
-- * 'raValue'
data ReplaceableAttribute = ReplaceableAttribute'{_raReplace :: Maybe Bool, _raName :: Text, _raValue :: Text} deriving (Eq, Read, Show)

-- | 'ReplaceableAttribute' smart constructor.
replaceableAttribute :: Text -> Text -> ReplaceableAttribute
replaceableAttribute pName pValue = ReplaceableAttribute'{_raReplace = Nothing, _raName = pName, _raValue = pValue};

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

-- | /See:/ 'replaceableItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riName'
--
-- * 'riAttributes'
data ReplaceableItem = ReplaceableItem'{_riName :: Text, _riAttributes :: [ReplaceableAttribute]} deriving (Eq, Read, Show)

-- | 'ReplaceableItem' smart constructor.
replaceableItem :: Text -> ReplaceableItem
replaceableItem pName = ReplaceableItem'{_riName = pName, _riAttributes = mempty};

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\ s a -> s{_riName = a});

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\ s a -> s{_riAttributes = a});

instance ToQuery ReplaceableItem where
        toQuery ReplaceableItem'{..}
          = mconcat
              ["ItemName" =: _riName,
               toQueryList "Attribute" _riAttributes]

-- | /See:/ 'updateCondition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucExists'
--
-- * 'ucValue'
--
-- * 'ucName'
data UpdateCondition = UpdateCondition'{_ucExists :: Maybe Bool, _ucValue :: Maybe Text, _ucName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UpdateCondition' smart constructor.
updateCondition :: UpdateCondition
updateCondition = UpdateCondition'{_ucExists = Nothing, _ucValue = Nothing, _ucName = Nothing};

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
