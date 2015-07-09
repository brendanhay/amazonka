{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the attributes associated with the specified item.
-- Optionally, the attributes returned can be limited to one or more
-- attributes by specifying an attribute name parameter.
--
-- If the item does not exist on the replica that was accessed for this
-- operation, an empty set is returned. The system does not return an error
-- as it cannot guarantee the item does not exist on other replicas.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_GetAttributes.html>
module Network.AWS.SDB.GetAttributes
    (
    -- * Request
      GetAttributes
    -- ** Request constructor
    , getAttributes
    -- ** Request lenses
    , gaConsistentRead
    , gaAttributeNames
    , gaDomainName
    , gaItemName

    -- * Response
    , GetAttributesResponse
    -- ** Response constructor
    , getAttributesResponse
    -- ** Response lenses
    , garAttributes
    , garStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'getAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaConsistentRead'
--
-- * 'gaAttributeNames'
--
-- * 'gaDomainName'
--
-- * 'gaItemName'
data GetAttributes = GetAttributes'
    { _gaConsistentRead :: !(Maybe Bool)
    , _gaAttributeNames :: !(Maybe [Text])
    , _gaDomainName     :: !Text
    , _gaItemName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAttributes' smart constructor.
getAttributes :: Text -> Text -> GetAttributes
getAttributes pDomainName pItemName =
    GetAttributes'
    { _gaConsistentRead = Nothing
    , _gaAttributeNames = Nothing
    , _gaDomainName = pDomainName
    , _gaItemName = pItemName
    }

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
gaConsistentRead :: Lens' GetAttributes (Maybe Bool)
gaConsistentRead = lens _gaConsistentRead (\ s a -> s{_gaConsistentRead = a});

-- | The names of the attributes.
gaAttributeNames :: Lens' GetAttributes [Text]
gaAttributeNames = lens _gaAttributeNames (\ s a -> s{_gaAttributeNames = a}) . _Default;

-- | The name of the domain in which to perform the operation.
gaDomainName :: Lens' GetAttributes Text
gaDomainName = lens _gaDomainName (\ s a -> s{_gaDomainName = a});

-- | The name of the item.
gaItemName :: Lens' GetAttributes Text
gaItemName = lens _gaItemName (\ s a -> s{_gaItemName = a});

instance AWSRequest GetAttributes where
        type Sv GetAttributes = SDB
        type Rs GetAttributes = GetAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetAttributesResult"
              (\ s h x ->
                 GetAttributesResponse' <$>
                   (may (parseXMLList "Attribute") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetAttributes where
        toHeaders = const mempty

instance ToPath GetAttributes where
        toPath = const "/"

instance ToQuery GetAttributes where
        toQuery GetAttributes'{..}
          = mconcat
              ["Action" =: ("GetAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "ConsistentRead" =: _gaConsistentRead,
               toQuery
                 (toQueryList "AttributeName" <$> _gaAttributeNames),
               "DomainName" =: _gaDomainName,
               "ItemName" =: _gaItemName]

-- | /See:/ 'getAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garAttributes'
--
-- * 'garStatus'
data GetAttributesResponse = GetAttributesResponse'
    { _garAttributes :: !(Maybe [Attribute])
    , _garStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAttributesResponse' smart constructor.
getAttributesResponse :: Int -> GetAttributesResponse
getAttributesResponse pStatus =
    GetAttributesResponse'
    { _garAttributes = Nothing
    , _garStatus = pStatus
    }

-- | The list of attributes returned by the operation.
garAttributes :: Lens' GetAttributesResponse [Attribute]
garAttributes = lens _garAttributes (\ s a -> s{_garAttributes = a}) . _Default;

-- | FIXME: Undocumented member.
garStatus :: Lens' GetAttributesResponse Int
garStatus = lens _garStatus (\ s a -> s{_garStatus = a});
