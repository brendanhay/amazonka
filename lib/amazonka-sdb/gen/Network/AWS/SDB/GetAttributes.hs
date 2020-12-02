{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the attributes associated with the specified item. Optionally, the attributes returned can be limited to one or more attributes by specifying an attribute name parameter.
--
--
-- If the item does not exist on the replica that was accessed for this operation, an empty set is returned. The system does not return an error as it cannot guarantee the item does not exist on other replicas.
--
module Network.AWS.SDB.GetAttributes
    (
    -- * Creating a Request
      getAttributes
    , GetAttributes
    -- * Request Lenses
    , gaConsistentRead
    , gaAttributeNames
    , gaDomainName
    , gaItemName

    -- * Destructuring the Response
    , getAttributesResponse
    , GetAttributesResponse
    -- * Response Lenses
    , garsAttributes
    , garsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'getAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { _gaConsistentRead :: !(Maybe Bool)
  , _gaAttributeNames :: !(Maybe [Text])
  , _gaDomainName     :: !Text
  , _gaItemName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaConsistentRead' - @true@
--
-- * 'gaAttributeNames' - The names of the attributes.
--
-- * 'gaDomainName' - The name of the domain in which to perform the operation.
--
-- * 'gaItemName' - The name of the item.
getAttributes
    :: Text -- ^ 'gaDomainName'
    -> Text -- ^ 'gaItemName'
    -> GetAttributes
getAttributes pDomainName_ pItemName_ =
  GetAttributes'
    { _gaConsistentRead = Nothing
    , _gaAttributeNames = Nothing
    , _gaDomainName = pDomainName_
    , _gaItemName = pItemName_
    }


-- | @true@
gaConsistentRead :: Lens' GetAttributes (Maybe Bool)
gaConsistentRead = lens _gaConsistentRead (\ s a -> s{_gaConsistentRead = a})

-- | The names of the attributes.
gaAttributeNames :: Lens' GetAttributes [Text]
gaAttributeNames = lens _gaAttributeNames (\ s a -> s{_gaAttributeNames = a}) . _Default . _Coerce

-- | The name of the domain in which to perform the operation.
gaDomainName :: Lens' GetAttributes Text
gaDomainName = lens _gaDomainName (\ s a -> s{_gaDomainName = a})

-- | The name of the item.
gaItemName :: Lens' GetAttributes Text
gaItemName = lens _gaItemName (\ s a -> s{_gaItemName = a})

instance AWSRequest GetAttributes where
        type Rs GetAttributes = GetAttributesResponse
        request = postQuery sdb
        response
          = receiveXMLWrapper "GetAttributesResult"
              (\ s h x ->
                 GetAttributesResponse' <$>
                   (may (parseXMLList "Attribute") x) <*>
                     (pure (fromEnum s)))

instance Hashable GetAttributes where

instance NFData GetAttributes where

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
data GetAttributesResponse = GetAttributesResponse'
  { _garsAttributes     :: !(Maybe [Attribute])
  , _garsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsAttributes' - The list of attributes returned by the operation.
--
-- * 'garsResponseStatus' - -- | The response status code.
getAttributesResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetAttributesResponse
getAttributesResponse pResponseStatus_ =
  GetAttributesResponse'
    {_garsAttributes = Nothing, _garsResponseStatus = pResponseStatus_}


-- | The list of attributes returned by the operation.
garsAttributes :: Lens' GetAttributesResponse [Attribute]
garsAttributes = lens _garsAttributes (\ s a -> s{_garsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAttributesResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

instance NFData GetAttributesResponse where
