{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SDB.PutAttributes
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

-- | The PutAttributes operation creates or replaces attributes in an item.
-- The client may specify new attributes using a combination of the
-- @Attribute.X.Name@ and @Attribute.X.Value@ parameters. The client
-- specifies the first attribute by the parameters @Attribute.0.Name@ and
-- @Attribute.0.Value@, the second attribute by the parameters
-- @Attribute.1.Name@ and @Attribute.1.Value@, and so on.
--
-- Attributes are uniquely identified in an item by their name\/value
-- combination. For example, a single item can have the attributes
-- @{ \"first_name\", \"first_value\" }@ and
-- @{ \"first_name\", second_value\" }@. However, it cannot have two
-- attribute instances where both the @Attribute.X.Name@ and
-- @Attribute.X.Value@ are the same.
--
-- Optionally, the requestor can supply the @Replace@ parameter for each
-- individual attribute. Setting this value to @true@ causes the new
-- attribute value to replace the existing attribute value(s). For example,
-- if an item has the attributes @{ \'a\', \'1\' }@, @{ \'b\', \'2\'}@ and
-- @{ \'b\', \'3\' }@ and the requestor calls @PutAttributes@ using the
-- attributes @{ \'b\', \'4\' }@ with the @Replace@ parameter set to true,
-- the final attributes of the item are changed to @{ \'a\', \'1\' }@ and
-- @{ \'b\', \'4\' }@, which replaces the previous values of the \'b\'
-- attribute with the new value.
--
-- You cannot specify an empty string as an attribute name.
--
-- Because Amazon SimpleDB makes multiple copies of client data and uses an
-- eventual consistency update model, an immediate GetAttributes or Select
-- operation (read) immediately after a PutAttributes or DeleteAttributes
-- operation (write) might not return the updated data.
--
-- The following limitations are enforced for this operation:
--
-- -   256 total attribute name-value pairs per item
-- -   One billion attributes per domain
-- -   10 GB of total user data storage per domain
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_PutAttributes.html>
module Network.AWS.SDB.PutAttributes
    (
    -- * Request
      PutAttributes
    -- ** Request constructor
    , putAttributes
    -- ** Request lenses
    , paExpected
    , paDomainName
    , paItemName
    , paAttributes

    -- * Response
    , PutAttributesResponse
    -- ** Response constructor
    , putAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'putAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paExpected'
--
-- * 'paDomainName'
--
-- * 'paItemName'
--
-- * 'paAttributes'
data PutAttributes = PutAttributes'
    { _paExpected   :: Maybe UpdateCondition
    , _paDomainName :: Text
    , _paItemName   :: Text
    , _paAttributes :: [ReplaceableAttribute]
    } deriving (Eq,Read,Show)

-- | 'PutAttributes' smart constructor.
putAttributes :: Text -> Text -> PutAttributes
putAttributes pDomainName pItemName =
    PutAttributes'
    { _paExpected = Nothing
    , _paDomainName = pDomainName
    , _paItemName = pItemName
    , _paAttributes = mempty
    }

-- | The update condition which, if specified, determines whether the
-- specified attributes will be updated or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be updated.
paExpected :: Lens' PutAttributes (Maybe UpdateCondition)
paExpected = lens _paExpected (\ s a -> s{_paExpected = a});

-- | The name of the domain in which to perform the operation.
paDomainName :: Lens' PutAttributes Text
paDomainName = lens _paDomainName (\ s a -> s{_paDomainName = a});

-- | The name of the item.
paItemName :: Lens' PutAttributes Text
paItemName = lens _paItemName (\ s a -> s{_paItemName = a});

-- | The list of attributes.
paAttributes :: Lens' PutAttributes [ReplaceableAttribute]
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a});

instance AWSRequest PutAttributes where
        type Sv PutAttributes = SDB
        type Rs PutAttributes = PutAttributesResponse
        request = post
        response = receiveNull PutAttributesResponse'

instance ToHeaders PutAttributes where
        toHeaders = const mempty

instance ToPath PutAttributes where
        toPath = const "/"

instance ToQuery PutAttributes where
        toQuery PutAttributes'{..}
          = mconcat
              ["Action" =: ("PutAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "Expected" =: _paExpected,
               "DomainName" =: _paDomainName,
               "ItemName" =: _paItemName,
               toQueryList "Attribute" _paAttributes]

-- | /See:/ 'putAttributesResponse' smart constructor.
data PutAttributesResponse =
    PutAttributesResponse'
    deriving (Eq,Read,Show)

-- | 'PutAttributesResponse' smart constructor.
putAttributesResponse :: PutAttributesResponse
putAttributesResponse = PutAttributesResponse'
