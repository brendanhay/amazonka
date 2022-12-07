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
-- Module      : Amazonka.ResourceExplorer2.CreateIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on Amazon Web Services Resource Explorer in the Amazon Web
-- Services Region in which you called this operation by creating an index.
-- Resource Explorer begins discovering the resources in this Region and
-- stores the details about the resources in the index so that they can be
-- queried by using the Search operation. You can create only one index in
-- a Region.
--
-- This operation creates only a /local/ index. To promote the local index
-- in one Amazon Web Services Region into the aggregator index for the
-- Amazon Web Services account, use the UpdateIndexType operation. For more
-- information, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search by creating an aggregator index>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- For more details about what happens when you turn on Resource Explorer
-- in an Amazon Web Services Region, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-service-activate.html Turn on Resource Explorer to index your resources in an Amazon Web Services Region>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- If this is the first Amazon Web Services Region in which you\'ve created
-- an index for Resource Explorer, then this operation also
-- <https://docs.aws.amazon.com/arexug/mainline/security_iam_service-linked-roles.html creates a service-linked role>
-- in your Amazon Web Services account that allows Resource Explorer to
-- enumerate your resources to populate the index.
--
-- -   __Action__: @resource-explorer-2:CreateIndex@
--
--     __Resource__: The ARN of the index (as it will exist after the
--     operation completes) in the Amazon Web Services Region and account
--     in which you\'re trying to create the index. Use the wildcard
--     character (@*@) at the end of the string to match the eventual UUID.
--     For example, the following @Resource@ element restricts the role or
--     user to creating an index in only the @us-east-2@ Region of the
--     specified account.
--
--     @\"Resource\": \"arn:aws:resource-explorer-2:us-west-2:\<account-id>:index\/*\"@
--
--     Alternatively, you can use @\"Resource\": \"*\"@ to allow the role
--     or user to create an index in any Region.
--
-- -   __Action__: @iam:CreateServiceLinkedRole@
--
--     __Resource__: No specific resource (*).
--
--     This permission is required only the first time you create an index
--     to turn on Resource Explorer in the account. Resource Explorer uses
--     this to create the
--     <https://docs.aws.amazon.com/resource-explorer/latest/userguide/security_iam_service-linked-roles.html service-linked role needed to index the resources in your account>.
--     Resource Explorer uses the same service-linked role for all
--     additional indexes you create afterwards.
module Amazonka.ResourceExplorer2.CreateIndex
  ( -- * Creating a Request
    CreateIndex (..),
    newCreateIndex,

    -- * Request Lenses
    createIndex_tags,
    createIndex_clientToken,

    -- * Destructuring the Response
    CreateIndexResponse (..),
    newCreateIndexResponse,

    -- * Response Lenses
    createIndexResponse_arn,
    createIndexResponse_state,
    createIndexResponse_createdAt,
    createIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { -- | The specified tags are attached only to the index created in this Amazon
    -- Web Services Region. The tags aren\'t attached to any of the resources
    -- listed in the index.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This value helps ensure idempotency. Resource Explorer uses this value
    -- to prevent the accidental creation of duplicate versions. We recommend
    -- that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
    -- to ensure the uniqueness of your views.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createIndex_tags' - The specified tags are attached only to the index created in this Amazon
-- Web Services Region. The tags aren\'t attached to any of the resources
-- listed in the index.
--
-- 'clientToken', 'createIndex_clientToken' - This value helps ensure idempotency. Resource Explorer uses this value
-- to prevent the accidental creation of duplicate versions. We recommend
-- that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
-- to ensure the uniqueness of your views.
newCreateIndex ::
  CreateIndex
newCreateIndex =
  CreateIndex'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | The specified tags are attached only to the index created in this Amazon
-- Web Services Region. The tags aren\'t attached to any of the resources
-- listed in the index.
createIndex_tags :: Lens.Lens' CreateIndex (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIndex_tags = Lens.lens (\CreateIndex' {tags} -> tags) (\s@CreateIndex' {} a -> s {tags = a} :: CreateIndex) Prelude.. Lens.mapping Lens.coerced

-- | This value helps ensure idempotency. Resource Explorer uses this value
-- to prevent the accidental creation of duplicate versions. We recommend
-- that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
-- to ensure the uniqueness of your views.
createIndex_clientToken :: Lens.Lens' CreateIndex (Prelude.Maybe Prelude.Text)
createIndex_clientToken = Lens.lens (\CreateIndex' {clientToken} -> clientToken) (\s@CreateIndex' {} a -> s {clientToken = a} :: CreateIndex)

instance Core.AWSRequest CreateIndex where
  type AWSResponse CreateIndex = CreateIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIndexResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIndex where
  hashWithSalt _salt CreateIndex' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateIndex where
  rnf CreateIndex' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIndex where
  toJSON CreateIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken
          ]
      )

instance Data.ToPath CreateIndex where
  toPath = Prelude.const "/CreateIndex"

instance Data.ToQuery CreateIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { -- | The ARN of the new local index for the Region. You can reference this
    -- ARN in IAM permission policies to authorize the following operations:
    -- DeleteIndex | GetIndex | UpdateIndexType | CreateView
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the current state of the index. You can check for changes to
    -- the state for asynchronous operations by calling the GetIndex operation.
    --
    -- The state can remain in the @CREATING@ or @UPDATING@ state for several
    -- hours as Resource Explorer discovers the information about your
    -- resources and populates the index.
    state :: Prelude.Maybe IndexState,
    -- | The date and timestamp when the index was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createIndexResponse_arn' - The ARN of the new local index for the Region. You can reference this
-- ARN in IAM permission policies to authorize the following operations:
-- DeleteIndex | GetIndex | UpdateIndexType | CreateView
--
-- 'state', 'createIndexResponse_state' - Indicates the current state of the index. You can check for changes to
-- the state for asynchronous operations by calling the GetIndex operation.
--
-- The state can remain in the @CREATING@ or @UPDATING@ state for several
-- hours as Resource Explorer discovers the information about your
-- resources and populates the index.
--
-- 'createdAt', 'createIndexResponse_createdAt' - The date and timestamp when the index was created.
--
-- 'httpStatus', 'createIndexResponse_httpStatus' - The response's http status code.
newCreateIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIndexResponse
newCreateIndexResponse pHttpStatus_ =
  CreateIndexResponse'
    { arn = Prelude.Nothing,
      state = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new local index for the Region. You can reference this
-- ARN in IAM permission policies to authorize the following operations:
-- DeleteIndex | GetIndex | UpdateIndexType | CreateView
createIndexResponse_arn :: Lens.Lens' CreateIndexResponse (Prelude.Maybe Prelude.Text)
createIndexResponse_arn = Lens.lens (\CreateIndexResponse' {arn} -> arn) (\s@CreateIndexResponse' {} a -> s {arn = a} :: CreateIndexResponse)

-- | Indicates the current state of the index. You can check for changes to
-- the state for asynchronous operations by calling the GetIndex operation.
--
-- The state can remain in the @CREATING@ or @UPDATING@ state for several
-- hours as Resource Explorer discovers the information about your
-- resources and populates the index.
createIndexResponse_state :: Lens.Lens' CreateIndexResponse (Prelude.Maybe IndexState)
createIndexResponse_state = Lens.lens (\CreateIndexResponse' {state} -> state) (\s@CreateIndexResponse' {} a -> s {state = a} :: CreateIndexResponse)

-- | The date and timestamp when the index was created.
createIndexResponse_createdAt :: Lens.Lens' CreateIndexResponse (Prelude.Maybe Prelude.UTCTime)
createIndexResponse_createdAt = Lens.lens (\CreateIndexResponse' {createdAt} -> createdAt) (\s@CreateIndexResponse' {} a -> s {createdAt = a} :: CreateIndexResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createIndexResponse_httpStatus :: Lens.Lens' CreateIndexResponse Prelude.Int
createIndexResponse_httpStatus = Lens.lens (\CreateIndexResponse' {httpStatus} -> httpStatus) (\s@CreateIndexResponse' {} a -> s {httpStatus = a} :: CreateIndexResponse)

instance Prelude.NFData CreateIndexResponse where
  rnf CreateIndexResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
