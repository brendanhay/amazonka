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
-- Module      : Amazonka.MarketplaceCatalog.StartChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to request changes for your entities. Within a single
-- @ChangeSet@, you can\'t start the same change type against the same
-- entity multiple times. Additionally, when a @ChangeSet@ is running, all
-- the entities targeted by the different changes are locked until the
-- change set has completed (either succeeded, cancelled, or failed). If
-- you try to start a change set containing a change against an entity that
-- is already locked, you will receive a @ResourceInUseException@ error.
--
-- For example, you can\'t start the @ChangeSet@ described in the
-- <https://docs.aws.amazon.com/marketplace-catalog/latest/api-reference/API_StartChangeSet.html#API_StartChangeSet_Examples example>
-- later in this topic because it contains two changes to run the same
-- change type (@AddRevisions@) against the same entity (@entity-id\@1@).
--
-- For more information about working with change sets, see
-- <https://docs.aws.amazon.com/marketplace-catalog/latest/api-reference/welcome.html#working-with-change-sets Working with change sets>.
module Amazonka.MarketplaceCatalog.StartChangeSet
  ( -- * Creating a Request
    StartChangeSet (..),
    newStartChangeSet,

    -- * Request Lenses
    startChangeSet_changeSetName,
    startChangeSet_changeSetTags,
    startChangeSet_clientRequestToken,
    startChangeSet_catalog,
    startChangeSet_changeSet,

    -- * Destructuring the Response
    StartChangeSetResponse (..),
    newStartChangeSetResponse,

    -- * Response Lenses
    startChangeSetResponse_changeSetArn,
    startChangeSetResponse_changeSetId,
    startChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartChangeSet' smart constructor.
data StartChangeSet = StartChangeSet'
  { -- | Optional case sensitive string of up to 100 ASCII characters. The change
    -- set name can be used to filter the list of change sets.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of objects specifying each key name and value for the
    -- @ChangeSetTags@ property.
    changeSetTags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A unique token to identify the request to ensure idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The catalog related to the request. Fixed value: @AWSMarketplace@
    catalog :: Prelude.Text,
    -- | Array of @change@ object.
    changeSet :: Prelude.NonEmpty Change
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetName', 'startChangeSet_changeSetName' - Optional case sensitive string of up to 100 ASCII characters. The change
-- set name can be used to filter the list of change sets.
--
-- 'changeSetTags', 'startChangeSet_changeSetTags' - A list of objects specifying each key name and value for the
-- @ChangeSetTags@ property.
--
-- 'clientRequestToken', 'startChangeSet_clientRequestToken' - A unique token to identify the request to ensure idempotency.
--
-- 'catalog', 'startChangeSet_catalog' - The catalog related to the request. Fixed value: @AWSMarketplace@
--
-- 'changeSet', 'startChangeSet_changeSet' - Array of @change@ object.
newStartChangeSet ::
  -- | 'catalog'
  Prelude.Text ->
  -- | 'changeSet'
  Prelude.NonEmpty Change ->
  StartChangeSet
newStartChangeSet pCatalog_ pChangeSet_ =
  StartChangeSet'
    { changeSetName = Prelude.Nothing,
      changeSetTags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      catalog = pCatalog_,
      changeSet = Lens.coerced Lens.# pChangeSet_
    }

-- | Optional case sensitive string of up to 100 ASCII characters. The change
-- set name can be used to filter the list of change sets.
startChangeSet_changeSetName :: Lens.Lens' StartChangeSet (Prelude.Maybe Prelude.Text)
startChangeSet_changeSetName = Lens.lens (\StartChangeSet' {changeSetName} -> changeSetName) (\s@StartChangeSet' {} a -> s {changeSetName = a} :: StartChangeSet)

-- | A list of objects specifying each key name and value for the
-- @ChangeSetTags@ property.
startChangeSet_changeSetTags :: Lens.Lens' StartChangeSet (Prelude.Maybe (Prelude.NonEmpty Tag))
startChangeSet_changeSetTags = Lens.lens (\StartChangeSet' {changeSetTags} -> changeSetTags) (\s@StartChangeSet' {} a -> s {changeSetTags = a} :: StartChangeSet) Prelude.. Lens.mapping Lens.coerced

-- | A unique token to identify the request to ensure idempotency.
startChangeSet_clientRequestToken :: Lens.Lens' StartChangeSet (Prelude.Maybe Prelude.Text)
startChangeSet_clientRequestToken = Lens.lens (\StartChangeSet' {clientRequestToken} -> clientRequestToken) (\s@StartChangeSet' {} a -> s {clientRequestToken = a} :: StartChangeSet)

-- | The catalog related to the request. Fixed value: @AWSMarketplace@
startChangeSet_catalog :: Lens.Lens' StartChangeSet Prelude.Text
startChangeSet_catalog = Lens.lens (\StartChangeSet' {catalog} -> catalog) (\s@StartChangeSet' {} a -> s {catalog = a} :: StartChangeSet)

-- | Array of @change@ object.
startChangeSet_changeSet :: Lens.Lens' StartChangeSet (Prelude.NonEmpty Change)
startChangeSet_changeSet = Lens.lens (\StartChangeSet' {changeSet} -> changeSet) (\s@StartChangeSet' {} a -> s {changeSet = a} :: StartChangeSet) Prelude.. Lens.coerced

instance Core.AWSRequest StartChangeSet where
  type
    AWSResponse StartChangeSet =
      StartChangeSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChangeSetResponse'
            Prelude.<$> (x Data..?> "ChangeSetArn")
            Prelude.<*> (x Data..?> "ChangeSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChangeSet where
  hashWithSalt _salt StartChangeSet' {..} =
    _salt `Prelude.hashWithSalt` changeSetName
      `Prelude.hashWithSalt` changeSetTags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` changeSet

instance Prelude.NFData StartChangeSet where
  rnf StartChangeSet' {..} =
    Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf changeSetTags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf changeSet

instance Data.ToHeaders StartChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartChangeSet where
  toJSON StartChangeSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChangeSetName" Data..=) Prelude.<$> changeSetName,
            ("ChangeSetTags" Data..=) Prelude.<$> changeSetTags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Catalog" Data..= catalog),
            Prelude.Just ("ChangeSet" Data..= changeSet)
          ]
      )

instance Data.ToPath StartChangeSet where
  toPath = Prelude.const "/StartChangeSet"

instance Data.ToQuery StartChangeSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartChangeSetResponse' smart constructor.
data StartChangeSetResponse = StartChangeSetResponse'
  { -- | The ARN associated to the unique identifier generated for the request.
    changeSetArn :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier generated for the request.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetArn', 'startChangeSetResponse_changeSetArn' - The ARN associated to the unique identifier generated for the request.
--
-- 'changeSetId', 'startChangeSetResponse_changeSetId' - Unique identifier generated for the request.
--
-- 'httpStatus', 'startChangeSetResponse_httpStatus' - The response's http status code.
newStartChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartChangeSetResponse
newStartChangeSetResponse pHttpStatus_ =
  StartChangeSetResponse'
    { changeSetArn =
        Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN associated to the unique identifier generated for the request.
startChangeSetResponse_changeSetArn :: Lens.Lens' StartChangeSetResponse (Prelude.Maybe Prelude.Text)
startChangeSetResponse_changeSetArn = Lens.lens (\StartChangeSetResponse' {changeSetArn} -> changeSetArn) (\s@StartChangeSetResponse' {} a -> s {changeSetArn = a} :: StartChangeSetResponse)

-- | Unique identifier generated for the request.
startChangeSetResponse_changeSetId :: Lens.Lens' StartChangeSetResponse (Prelude.Maybe Prelude.Text)
startChangeSetResponse_changeSetId = Lens.lens (\StartChangeSetResponse' {changeSetId} -> changeSetId) (\s@StartChangeSetResponse' {} a -> s {changeSetId = a} :: StartChangeSetResponse)

-- | The response's http status code.
startChangeSetResponse_httpStatus :: Lens.Lens' StartChangeSetResponse Prelude.Int
startChangeSetResponse_httpStatus = Lens.lens (\StartChangeSetResponse' {httpStatus} -> httpStatus) (\s@StartChangeSetResponse' {} a -> s {httpStatus = a} :: StartChangeSetResponse)

instance Prelude.NFData StartChangeSetResponse where
  rnf StartChangeSetResponse' {..} =
    Prelude.rnf changeSetArn
      `Prelude.seq` Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf httpStatus
