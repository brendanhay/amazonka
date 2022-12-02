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
-- Module      : Amazonka.Route53.ChangeCidrCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates, changes, or deletes CIDR blocks within a collection. Contains
-- authoritative IP information mapping blocks to one or multiple
-- locations.
--
-- A change request can update multiple locations in a collection at a
-- time, which is helpful if you want to move one or more CIDR blocks from
-- one location to another in one transaction, without downtime.
--
-- __Limits__
--
-- The max number of CIDR blocks included in the request is 1000. As a
-- result, big updates require multiple API calls.
--
-- __PUT and DELETE_IF_EXISTS__
--
-- Use @ChangeCidrCollection@ to perform the following actions:
--
-- -   @PUT@: Create a CIDR block within the specified collection.
--
-- -   @ DELETE_IF_EXISTS@: Delete an existing CIDR block from the
--     collection.
module Amazonka.Route53.ChangeCidrCollection
  ( -- * Creating a Request
    ChangeCidrCollection (..),
    newChangeCidrCollection,

    -- * Request Lenses
    changeCidrCollection_collectionVersion,
    changeCidrCollection_id,
    changeCidrCollection_changes,

    -- * Destructuring the Response
    ChangeCidrCollectionResponse (..),
    newChangeCidrCollectionResponse,

    -- * Response Lenses
    changeCidrCollectionResponse_httpStatus,
    changeCidrCollectionResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newChangeCidrCollection' smart constructor.
data ChangeCidrCollection = ChangeCidrCollection'
  { -- | A sequential counter that Amazon Route 53 sets to 1 when you create a
    -- collection and increments it by 1 each time you update the collection.
    --
    -- We recommend that you use @ListCidrCollection@ to get the current value
    -- of @CollectionVersion@ for the collection that you want to update, and
    -- then include that value with the change request. This prevents Route 53
    -- from overwriting an intervening update:
    --
    -- -   If the value in the request matches the value of @CollectionVersion@
    --     in the collection, Route 53 updates the collection.
    --
    -- -   If the value of @CollectionVersion@ in the collection is greater
    --     than the value in the request, the collection was changed after you
    --     got the version number. Route 53 does not update the collection, and
    --     it returns a @CidrCollectionVersionMismatch@ error.
    collectionVersion :: Prelude.Maybe Prelude.Natural,
    -- | The UUID of the CIDR collection to update.
    id :: Prelude.Text,
    -- | Information about changes to a CIDR collection.
    changes :: Prelude.NonEmpty CidrCollectionChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeCidrCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionVersion', 'changeCidrCollection_collectionVersion' - A sequential counter that Amazon Route 53 sets to 1 when you create a
-- collection and increments it by 1 each time you update the collection.
--
-- We recommend that you use @ListCidrCollection@ to get the current value
-- of @CollectionVersion@ for the collection that you want to update, and
-- then include that value with the change request. This prevents Route 53
-- from overwriting an intervening update:
--
-- -   If the value in the request matches the value of @CollectionVersion@
--     in the collection, Route 53 updates the collection.
--
-- -   If the value of @CollectionVersion@ in the collection is greater
--     than the value in the request, the collection was changed after you
--     got the version number. Route 53 does not update the collection, and
--     it returns a @CidrCollectionVersionMismatch@ error.
--
-- 'id', 'changeCidrCollection_id' - The UUID of the CIDR collection to update.
--
-- 'changes', 'changeCidrCollection_changes' - Information about changes to a CIDR collection.
newChangeCidrCollection ::
  -- | 'id'
  Prelude.Text ->
  -- | 'changes'
  Prelude.NonEmpty CidrCollectionChange ->
  ChangeCidrCollection
newChangeCidrCollection pId_ pChanges_ =
  ChangeCidrCollection'
    { collectionVersion =
        Prelude.Nothing,
      id = pId_,
      changes = Lens.coerced Lens.# pChanges_
    }

-- | A sequential counter that Amazon Route 53 sets to 1 when you create a
-- collection and increments it by 1 each time you update the collection.
--
-- We recommend that you use @ListCidrCollection@ to get the current value
-- of @CollectionVersion@ for the collection that you want to update, and
-- then include that value with the change request. This prevents Route 53
-- from overwriting an intervening update:
--
-- -   If the value in the request matches the value of @CollectionVersion@
--     in the collection, Route 53 updates the collection.
--
-- -   If the value of @CollectionVersion@ in the collection is greater
--     than the value in the request, the collection was changed after you
--     got the version number. Route 53 does not update the collection, and
--     it returns a @CidrCollectionVersionMismatch@ error.
changeCidrCollection_collectionVersion :: Lens.Lens' ChangeCidrCollection (Prelude.Maybe Prelude.Natural)
changeCidrCollection_collectionVersion = Lens.lens (\ChangeCidrCollection' {collectionVersion} -> collectionVersion) (\s@ChangeCidrCollection' {} a -> s {collectionVersion = a} :: ChangeCidrCollection)

-- | The UUID of the CIDR collection to update.
changeCidrCollection_id :: Lens.Lens' ChangeCidrCollection Prelude.Text
changeCidrCollection_id = Lens.lens (\ChangeCidrCollection' {id} -> id) (\s@ChangeCidrCollection' {} a -> s {id = a} :: ChangeCidrCollection)

-- | Information about changes to a CIDR collection.
changeCidrCollection_changes :: Lens.Lens' ChangeCidrCollection (Prelude.NonEmpty CidrCollectionChange)
changeCidrCollection_changes = Lens.lens (\ChangeCidrCollection' {changes} -> changes) (\s@ChangeCidrCollection' {} a -> s {changes = a} :: ChangeCidrCollection) Prelude.. Lens.coerced

instance Core.AWSRequest ChangeCidrCollection where
  type
    AWSResponse ChangeCidrCollection =
      ChangeCidrCollectionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ChangeCidrCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Id")
      )

instance Prelude.Hashable ChangeCidrCollection where
  hashWithSalt _salt ChangeCidrCollection' {..} =
    _salt `Prelude.hashWithSalt` collectionVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` changes

instance Prelude.NFData ChangeCidrCollection where
  rnf ChangeCidrCollection' {..} =
    Prelude.rnf collectionVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf changes

instance Data.ToElement ChangeCidrCollection where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeCidrCollectionRequest"

instance Data.ToHeaders ChangeCidrCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ChangeCidrCollection where
  toPath ChangeCidrCollection' {..} =
    Prelude.mconcat
      ["/2013-04-01/cidrcollection/", Data.toBS id]

instance Data.ToQuery ChangeCidrCollection where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML ChangeCidrCollection where
  toXML ChangeCidrCollection' {..} =
    Prelude.mconcat
      [ "CollectionVersion" Data.@= collectionVersion,
        "Changes" Data.@= Data.toXMLList "member" changes
      ]

-- | /See:/ 'newChangeCidrCollectionResponse' smart constructor.
data ChangeCidrCollectionResponse = ChangeCidrCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID that is returned by @ChangeCidrCollection@. You can use it as
    -- input to @GetChange@ to see if a CIDR collection change has propagated
    -- or not.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeCidrCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'changeCidrCollectionResponse_httpStatus' - The response's http status code.
--
-- 'id', 'changeCidrCollectionResponse_id' - The ID that is returned by @ChangeCidrCollection@. You can use it as
-- input to @GetChange@ to see if a CIDR collection change has propagated
-- or not.
newChangeCidrCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  ChangeCidrCollectionResponse
newChangeCidrCollectionResponse pHttpStatus_ pId_ =
  ChangeCidrCollectionResponse'
    { httpStatus =
        pHttpStatus_,
      id = pId_
    }

-- | The response's http status code.
changeCidrCollectionResponse_httpStatus :: Lens.Lens' ChangeCidrCollectionResponse Prelude.Int
changeCidrCollectionResponse_httpStatus = Lens.lens (\ChangeCidrCollectionResponse' {httpStatus} -> httpStatus) (\s@ChangeCidrCollectionResponse' {} a -> s {httpStatus = a} :: ChangeCidrCollectionResponse)

-- | The ID that is returned by @ChangeCidrCollection@. You can use it as
-- input to @GetChange@ to see if a CIDR collection change has propagated
-- or not.
changeCidrCollectionResponse_id :: Lens.Lens' ChangeCidrCollectionResponse Prelude.Text
changeCidrCollectionResponse_id = Lens.lens (\ChangeCidrCollectionResponse' {id} -> id) (\s@ChangeCidrCollectionResponse' {} a -> s {id = a} :: ChangeCidrCollectionResponse)

instance Prelude.NFData ChangeCidrCollectionResponse where
  rnf ChangeCidrCollectionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq` Prelude.rnf id
