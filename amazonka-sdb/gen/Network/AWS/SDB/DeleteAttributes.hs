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
-- Module      : Network.AWS.SDB.DeleteAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more attributes associated with an item. If all
-- attributes of the item are deleted, the item is deleted.
--
-- @DeleteAttributes@ is an idempotent operation; running it multiple times
-- on the same item or attribute does not result in an error response.
--
-- Because Amazon SimpleDB makes multiple copies of item data and uses an
-- eventual consistency update model, performing a GetAttributes or Select
-- operation (read) immediately after a @DeleteAttributes@ or PutAttributes
-- operation (write) might not return updated item data.
module Network.AWS.SDB.DeleteAttributes
  ( -- * Creating a Request
    DeleteAttributes (..),
    newDeleteAttributes,

    -- * Request Lenses
    deleteAttributes_expected,
    deleteAttributes_attributes,
    deleteAttributes_domainName,
    deleteAttributes_itemName,

    -- * Destructuring the Response
    DeleteAttributesResponse (..),
    newDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { -- | The update condition which, if specified, determines whether the
    -- specified attributes will be deleted or not. The update condition must
    -- be satisfied in order for this request to be processed and the
    -- attributes to be deleted.
    expected :: Prelude.Maybe UpdateCondition,
    -- | A list of Attributes. Similar to columns on a spreadsheet, attributes
    -- represent categories of data that can be assigned to items.
    attributes :: Prelude.Maybe [Attribute],
    -- | The name of the domain in which to perform the operation.
    domainName :: Prelude.Text,
    -- | The name of the item. Similar to rows on a spreadsheet, items represent
    -- individual objects that contain one or more value-attribute pairs.
    itemName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expected', 'deleteAttributes_expected' - The update condition which, if specified, determines whether the
-- specified attributes will be deleted or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be deleted.
--
-- 'attributes', 'deleteAttributes_attributes' - A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
--
-- 'domainName', 'deleteAttributes_domainName' - The name of the domain in which to perform the operation.
--
-- 'itemName', 'deleteAttributes_itemName' - The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
newDeleteAttributes ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'itemName'
  Prelude.Text ->
  DeleteAttributes
newDeleteAttributes pDomainName_ pItemName_ =
  DeleteAttributes'
    { expected = Prelude.Nothing,
      attributes = Prelude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | The update condition which, if specified, determines whether the
-- specified attributes will be deleted or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be deleted.
deleteAttributes_expected :: Lens.Lens' DeleteAttributes (Prelude.Maybe UpdateCondition)
deleteAttributes_expected = Lens.lens (\DeleteAttributes' {expected} -> expected) (\s@DeleteAttributes' {} a -> s {expected = a} :: DeleteAttributes)

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
deleteAttributes_attributes :: Lens.Lens' DeleteAttributes (Prelude.Maybe [Attribute])
deleteAttributes_attributes = Lens.lens (\DeleteAttributes' {attributes} -> attributes) (\s@DeleteAttributes' {} a -> s {attributes = a} :: DeleteAttributes) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the domain in which to perform the operation.
deleteAttributes_domainName :: Lens.Lens' DeleteAttributes Prelude.Text
deleteAttributes_domainName = Lens.lens (\DeleteAttributes' {domainName} -> domainName) (\s@DeleteAttributes' {} a -> s {domainName = a} :: DeleteAttributes)

-- | The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
deleteAttributes_itemName :: Lens.Lens' DeleteAttributes Prelude.Text
deleteAttributes_itemName = Lens.lens (\DeleteAttributes' {itemName} -> itemName) (\s@DeleteAttributes' {} a -> s {itemName = a} :: DeleteAttributes)

instance Prelude.AWSRequest DeleteAttributes where
  type Rs DeleteAttributes = DeleteAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteAttributesResponse'

instance Prelude.Hashable DeleteAttributes

instance Prelude.NFData DeleteAttributes

instance Prelude.ToHeaders DeleteAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAttributes where
  toQuery DeleteAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2009-04-15" :: Prelude.ByteString),
        "Expected" Prelude.=: expected,
        Prelude.toQuery
          ( Prelude.toQueryList "Attribute"
              Prelude.<$> attributes
          ),
        "DomainName" Prelude.=: domainName,
        "ItemName" Prelude.=: itemName
      ]

-- | /See:/ 'newDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAttributesResponse ::
  DeleteAttributesResponse
newDeleteAttributesResponse =
  DeleteAttributesResponse'

instance Prelude.NFData DeleteAttributesResponse
