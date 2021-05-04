{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.PutAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
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
module Network.AWS.SDB.PutAttributes
  ( -- * Creating a Request
    PutAttributes (..),
    newPutAttributes,

    -- * Request Lenses
    putAttributes_expected,
    putAttributes_domainName,
    putAttributes_itemName,
    putAttributes_attributes,

    -- * Destructuring the Response
    PutAttributesResponse (..),
    newPutAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { -- | The update condition which, if specified, determines whether the
    -- specified attributes will be updated or not. The update condition must
    -- be satisfied in order for this request to be processed and the
    -- attributes to be updated.
    expected :: Prelude.Maybe UpdateCondition,
    -- | The name of the domain in which to perform the operation.
    domainName :: Prelude.Text,
    -- | The name of the item.
    itemName :: Prelude.Text,
    -- | The list of attributes.
    attributes :: [ReplaceableAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expected', 'putAttributes_expected' - The update condition which, if specified, determines whether the
-- specified attributes will be updated or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be updated.
--
-- 'domainName', 'putAttributes_domainName' - The name of the domain in which to perform the operation.
--
-- 'itemName', 'putAttributes_itemName' - The name of the item.
--
-- 'attributes', 'putAttributes_attributes' - The list of attributes.
newPutAttributes ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'itemName'
  Prelude.Text ->
  PutAttributes
newPutAttributes pDomainName_ pItemName_ =
  PutAttributes'
    { expected = Prelude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_,
      attributes = Prelude.mempty
    }

-- | The update condition which, if specified, determines whether the
-- specified attributes will be updated or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be updated.
putAttributes_expected :: Lens.Lens' PutAttributes (Prelude.Maybe UpdateCondition)
putAttributes_expected = Lens.lens (\PutAttributes' {expected} -> expected) (\s@PutAttributes' {} a -> s {expected = a} :: PutAttributes)

-- | The name of the domain in which to perform the operation.
putAttributes_domainName :: Lens.Lens' PutAttributes Prelude.Text
putAttributes_domainName = Lens.lens (\PutAttributes' {domainName} -> domainName) (\s@PutAttributes' {} a -> s {domainName = a} :: PutAttributes)

-- | The name of the item.
putAttributes_itemName :: Lens.Lens' PutAttributes Prelude.Text
putAttributes_itemName = Lens.lens (\PutAttributes' {itemName} -> itemName) (\s@PutAttributes' {} a -> s {itemName = a} :: PutAttributes)

-- | The list of attributes.
putAttributes_attributes :: Lens.Lens' PutAttributes [ReplaceableAttribute]
putAttributes_attributes = Lens.lens (\PutAttributes' {attributes} -> attributes) (\s@PutAttributes' {} a -> s {attributes = a} :: PutAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutAttributes where
  type Rs PutAttributes = PutAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull PutAttributesResponse'

instance Prelude.Hashable PutAttributes

instance Prelude.NFData PutAttributes

instance Prelude.ToHeaders PutAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAttributes where
  toQuery PutAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2009-04-15" :: Prelude.ByteString),
        "Expected" Prelude.=: expected,
        "DomainName" Prelude.=: domainName,
        "ItemName" Prelude.=: itemName,
        Prelude.toQueryList "Attribute" attributes
      ]

-- | /See:/ 'newPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutAttributesResponse ::
  PutAttributesResponse
newPutAttributesResponse = PutAttributesResponse'

instance Prelude.NFData PutAttributesResponse
