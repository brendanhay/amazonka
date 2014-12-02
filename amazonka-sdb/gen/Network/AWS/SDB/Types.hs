{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * Attribute
    , Attribute
    , attribute
    , aAlternateNameEncoding
    , aAlternateValueEncoding
    , aName
    , aValue

    -- * DeletableItem
    , DeletableItem
    , deletableItem
    , diAttributes
    , diName

    -- * ReplaceableItem
    , ReplaceableItem
    , replaceableItem
    , riAttributes
    , riName

    -- * UpdateCondition
    , UpdateCondition
    , updateCondition
    , ucExists
    , ucName
    , ucValue

    -- * ReplaceableAttribute
    , ReplaceableAttribute
    , replaceableAttribute
    , raName
    , raReplace
    , raValue

    -- * Item
    , Item
    , item
    , iAlternateNameEncoding
    , iAttributes
    , iName
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2009-04-15@ of the Amazon SimpleDB service.
data SDB

instance AWSService SDB where
    type Sg SDB = V2
    type Er SDB = RESTError

    service = service'
      where
        service' :: Service SDB
        service' = Service
              { _svcAbbrev       = "SDB"
              , _svcPrefix       = "sdb"
              , _svcVersion      = "2009-04-15"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcDelay        = Exp 0.05 2 5
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Status
              -> RESTError
              -> Bool
        retry (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://sdb.amazonaws.com/doc/2009-04-15/"
{-# INLINE ns #-}

data Attribute = Attribute
    { _aAlternateNameEncoding  :: Maybe Text
    , _aAlternateValueEncoding :: Maybe Text
    , _aName                   :: Text
    , _aValue                  :: Text
    } deriving (Eq, Ord, Show)

-- | 'Attribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAlternateNameEncoding' @::@ 'Maybe' 'Text'
--
-- * 'aAlternateValueEncoding' @::@ 'Maybe' 'Text'
--
-- * 'aName' @::@ 'Text'
--
-- * 'aValue' @::@ 'Text'
--
attribute :: Text -- ^ 'aName'
          -> Text -- ^ 'aValue'
          -> Attribute
attribute p1 p2 = Attribute
    { _aName                   = p1
    , _aValue                  = p2
    , _aAlternateNameEncoding  = Nothing
    , _aAlternateValueEncoding = Nothing
    }


aAlternateNameEncoding :: Lens' Attribute (Maybe Text)
aAlternateNameEncoding =
    lens _aAlternateNameEncoding (\s a -> s { _aAlternateNameEncoding = a })


aAlternateValueEncoding :: Lens' Attribute (Maybe Text)
aAlternateValueEncoding =
    lens _aAlternateValueEncoding (\s a -> s { _aAlternateValueEncoding = a })

-- | The name of the attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\s a -> s { _aName = a })

-- | The value of the attribute.
aValue :: Lens' Attribute Text
aValue = lens _aValue (\s a -> s { _aValue = a })

instance FromXML Attribute where
    parseXML x = Attribute
        <$> x .@? "AlternateNameEncoding"
        <*> x .@? "AlternateValueEncoding"
        <*> x .@  "Name"
        <*> x .@  "Value"

instance ToQuery Attribute where
    toQuery Attribute{..} = mconcat
        [ "AlternateNameEncoding"  =? _aAlternateNameEncoding
        , "AlternateValueEncoding" =? _aAlternateValueEncoding
        , "Name"                   =? _aName
        , "Value"                  =? _aValue
        ]

data DeletableItem = DeletableItem
    { _diAttributes :: List "member" Attribute
    , _diName       :: Text
    } deriving (Eq, Show)

-- | 'DeletableItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diAttributes' @::@ ['Attribute']
--
-- * 'diName' @::@ 'Text'
--
deletableItem :: Text -- ^ 'diName'
              -> DeletableItem
deletableItem p1 = DeletableItem
    { _diName       = p1
    , _diAttributes = mempty
    }

diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\s a -> s { _diAttributes = a }) . _List

diName :: Lens' DeletableItem Text
diName = lens _diName (\s a -> s { _diName = a })

instance FromXML DeletableItem where
    parseXML x = DeletableItem
        <$> parseXML x
        <*> x .@  "ItemName"

instance ToQuery DeletableItem where
    toQuery DeletableItem{..} = mconcat
        [ toQuery     _diAttributes
        , "ItemName"   =? _diName
        ]

data ReplaceableItem = ReplaceableItem
    { _riAttributes :: List "member" ReplaceableAttribute
    , _riName       :: Text
    } deriving (Eq, Show)

-- | 'ReplaceableItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riAttributes' @::@ ['ReplaceableAttribute']
--
-- * 'riName' @::@ 'Text'
--
replaceableItem :: Text -- ^ 'riName'
                -> ReplaceableItem
replaceableItem p1 = ReplaceableItem
    { _riName       = p1
    , _riAttributes = mempty
    }

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\s a -> s { _riAttributes = a }) . _List

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\s a -> s { _riName = a })

instance FromXML ReplaceableItem where
    parseXML x = ReplaceableItem
        <$> parseXML x
        <*> x .@  "ItemName"

instance ToQuery ReplaceableItem where
    toQuery ReplaceableItem{..} = mconcat
        [ toQuery     _riAttributes
        , "ItemName"   =? _riName
        ]

data UpdateCondition = UpdateCondition
    { _ucExists :: Maybe Bool
    , _ucName   :: Maybe Text
    , _ucValue  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateCondition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucExists' @::@ 'Maybe' 'Bool'
--
-- * 'ucName' @::@ 'Maybe' 'Text'
--
-- * 'ucValue' @::@ 'Maybe' 'Text'
--
updateCondition :: UpdateCondition
updateCondition = UpdateCondition
    { _ucName   = Nothing
    , _ucValue  = Nothing
    , _ucExists = Nothing
    }

-- | A value specifying whether or not the specified attribute must exist with the
-- specified value in order for the update condition to be satisfied. Specify 'true' if the attribute must exist for the update condition to be satisfied.
-- Specify 'false' if the attribute should not exist in order for the update
-- condition to be satisfied.
ucExists :: Lens' UpdateCondition (Maybe Bool)
ucExists = lens _ucExists (\s a -> s { _ucExists = a })

-- | The name of the attribute involved in the condition.
ucName :: Lens' UpdateCondition (Maybe Text)
ucName = lens _ucName (\s a -> s { _ucName = a })

-- | The value of an attribute. This value can only be specified when the 'Exists'
-- parameter is equal to 'true'.
ucValue :: Lens' UpdateCondition (Maybe Text)
ucValue = lens _ucValue (\s a -> s { _ucValue = a })

instance FromXML UpdateCondition where
    parseXML x = UpdateCondition
        <$> x .@? "Exists"
        <*> x .@? "Name"
        <*> x .@? "Value"

instance ToQuery UpdateCondition where
    toQuery UpdateCondition{..} = mconcat
        [ "Exists" =? _ucExists
        , "Name"   =? _ucName
        , "Value"  =? _ucValue
        ]

data ReplaceableAttribute = ReplaceableAttribute
    { _raName    :: Text
    , _raReplace :: Maybe Bool
    , _raValue   :: Text
    } deriving (Eq, Ord, Show)

-- | 'ReplaceableAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raName' @::@ 'Text'
--
-- * 'raReplace' @::@ 'Maybe' 'Bool'
--
-- * 'raValue' @::@ 'Text'
--
replaceableAttribute :: Text -- ^ 'raName'
                     -> Text -- ^ 'raValue'
                     -> ReplaceableAttribute
replaceableAttribute p1 p2 = ReplaceableAttribute
    { _raName    = p1
    , _raValue   = p2
    , _raReplace = Nothing
    }

-- | The name of the replaceable attribute.
raName :: Lens' ReplaceableAttribute Text
raName = lens _raName (\s a -> s { _raName = a })

-- | A flag specifying whether or not to replace the attribute/value pair or to
-- add a new attribute/value pair. The default setting is 'false'.
raReplace :: Lens' ReplaceableAttribute (Maybe Bool)
raReplace = lens _raReplace (\s a -> s { _raReplace = a })

-- | The value of the replaceable attribute.
raValue :: Lens' ReplaceableAttribute Text
raValue = lens _raValue (\s a -> s { _raValue = a })

instance FromXML ReplaceableAttribute where
    parseXML x = ReplaceableAttribute
        <$> x .@  "Name"
        <*> x .@? "Replace"
        <*> x .@  "Value"

instance ToQuery ReplaceableAttribute where
    toQuery ReplaceableAttribute{..} = mconcat
        [ "Name"    =? _raName
        , "Replace" =? _raReplace
        , "Value"   =? _raValue
        ]

data Item = Item
    { _iAlternateNameEncoding :: Maybe Text
    , _iAttributes            :: List "member" Attribute
    , _iName                  :: Text
    } deriving (Eq, Show)

-- | 'Item' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iAlternateNameEncoding' @::@ 'Maybe' 'Text'
--
-- * 'iAttributes' @::@ ['Attribute']
--
-- * 'iName' @::@ 'Text'
--
item :: Text -- ^ 'iName'
     -> Item
item p1 = Item
    { _iName                  = p1
    , _iAlternateNameEncoding = Nothing
    , _iAttributes            = mempty
    }


iAlternateNameEncoding :: Lens' Item (Maybe Text)
iAlternateNameEncoding =
    lens _iAlternateNameEncoding (\s a -> s { _iAlternateNameEncoding = a })

-- | A list of attributes.
iAttributes :: Lens' Item [Attribute]
iAttributes = lens _iAttributes (\s a -> s { _iAttributes = a }) . _List

-- | The name of the item.
iName :: Lens' Item Text
iName = lens _iName (\s a -> s { _iName = a })

instance FromXML Item where
    parseXML x = Item
        <$> x .@? "AlternateNameEncoding"
        <*> parseXML x
        <*> x .@  "Name"

instance ToQuery Item where
    toQuery Item{..} = mconcat
        [ "AlternateNameEncoding" =? _iAlternateNameEncoding
        , toQuery                _iAttributes
        , "Name"                  =? _iName
        ]
