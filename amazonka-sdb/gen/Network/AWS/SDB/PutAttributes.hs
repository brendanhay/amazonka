{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.PutAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The PutAttributes operation creates or replaces attributes in an item.
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
    , parqExpected
    , parqDomainName
    , parqItemName
    , parqAttributes

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
-- * 'parqExpected'
--
-- * 'parqDomainName'
--
-- * 'parqItemName'
--
-- * 'parqAttributes'
data PutAttributes = PutAttributes'
    { _parqExpected   :: !(Maybe UpdateCondition)
    , _parqDomainName :: !Text
    , _parqItemName   :: !Text
    , _parqAttributes :: ![ReplaceableAttribute]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutAttributes' smart constructor.
putAttributes :: Text -> Text -> PutAttributes
putAttributes pDomainName_ pItemName_ =
    PutAttributes'
    { _parqExpected = Nothing
    , _parqDomainName = pDomainName_
    , _parqItemName = pItemName_
    , _parqAttributes = mempty
    }

-- | The update condition which, if specified, determines whether the
-- specified attributes will be updated or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be updated.
parqExpected :: Lens' PutAttributes (Maybe UpdateCondition)
parqExpected = lens _parqExpected (\ s a -> s{_parqExpected = a});

-- | The name of the domain in which to perform the operation.
parqDomainName :: Lens' PutAttributes Text
parqDomainName = lens _parqDomainName (\ s a -> s{_parqDomainName = a});

-- | The name of the item.
parqItemName :: Lens' PutAttributes Text
parqItemName = lens _parqItemName (\ s a -> s{_parqItemName = a});

-- | The list of attributes.
parqAttributes :: Lens' PutAttributes [ReplaceableAttribute]
parqAttributes = lens _parqAttributes (\ s a -> s{_parqAttributes = a});

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
               "Expected" =: _parqExpected,
               "DomainName" =: _parqDomainName,
               "ItemName" =: _parqItemName,
               toQueryList "Attribute" _parqAttributes]

-- | /See:/ 'putAttributesResponse' smart constructor.
data PutAttributesResponse =
    PutAttributesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutAttributesResponse' smart constructor.
putAttributesResponse :: PutAttributesResponse
putAttributesResponse = PutAttributesResponse'
