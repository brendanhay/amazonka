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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to request changes for your entities. Within a
-- single ChangeSet, you cannot start the same change type against the same
-- entity multiple times. Additionally, when a ChangeSet is running, all
-- the entities targeted by the different changes are locked until the
-- ChangeSet has completed (either succeeded, cancelled, or failed). If you
-- try to start a ChangeSet containing a change against an entity that is
-- already locked, you will receive a @ResourceInUseException@.
--
-- For example, you cannot start the ChangeSet described in the
-- <https://docs.aws.amazon.com/marketplace-catalog/latest/api-reference/API_StartChangeSet.html#API_StartChangeSet_Examples example>
-- later in this topic, because it contains two changes to execute the same
-- change type (@AddRevisions@) against the same entity (@entity-id\@1)@.
--
-- For more information about working with change sets, see
-- <https://docs.aws.amazon.com/marketplace-catalog/latest/api-reference/welcome.html#working-with-change-sets Working with change sets>.
module Amazonka.MarketplaceCatalog.StartChangeSet
  ( -- * Creating a Request
    StartChangeSet (..),
    newStartChangeSet,

    -- * Request Lenses
    startChangeSet_changeSetName,
    startChangeSet_clientRequestToken,
    startChangeSet_catalog,
    startChangeSet_changeSet,

    -- * Destructuring the Response
    StartChangeSetResponse (..),
    newStartChangeSetResponse,

    -- * Response Lenses
    startChangeSetResponse_changeSetId,
    startChangeSetResponse_changeSetArn,
    startChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartChangeSet' smart constructor.
data StartChangeSet = StartChangeSet'
  { -- | Optional case sensitive string of up to 100 ASCII characters. The change
    -- set name can be used to filter the list of change sets.
    changeSetName :: Prelude.Maybe Prelude.Text,
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
      clientRequestToken = Prelude.Nothing,
      catalog = pCatalog_,
      changeSet = Lens.coerced Lens.# pChangeSet_
    }

-- | Optional case sensitive string of up to 100 ASCII characters. The change
-- set name can be used to filter the list of change sets.
startChangeSet_changeSetName :: Lens.Lens' StartChangeSet (Prelude.Maybe Prelude.Text)
startChangeSet_changeSetName = Lens.lens (\StartChangeSet' {changeSetName} -> changeSetName) (\s@StartChangeSet' {} a -> s {changeSetName = a} :: StartChangeSet)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChangeSetResponse'
            Prelude.<$> (x Core..?> "ChangeSetId")
            Prelude.<*> (x Core..?> "ChangeSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChangeSet where
  hashWithSalt salt' StartChangeSet' {..} =
    salt' `Prelude.hashWithSalt` changeSet
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` changeSetName

instance Prelude.NFData StartChangeSet where
  rnf StartChangeSet' {..} =
    Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf changeSet
      `Prelude.seq` Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Core.ToHeaders StartChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartChangeSet where
  toJSON StartChangeSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ChangeSetName" Core..=) Prelude.<$> changeSetName,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Catalog" Core..= catalog),
            Prelude.Just ("ChangeSet" Core..= changeSet)
          ]
      )

instance Core.ToPath StartChangeSet where
  toPath = Prelude.const "/StartChangeSet"

instance Core.ToQuery StartChangeSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartChangeSetResponse' smart constructor.
data StartChangeSetResponse = StartChangeSetResponse'
  { -- | Unique identifier generated for the request.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The ARN associated to the unique identifier generated for the request.
    changeSetArn :: Prelude.Maybe Prelude.Text,
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
-- 'changeSetId', 'startChangeSetResponse_changeSetId' - Unique identifier generated for the request.
--
-- 'changeSetArn', 'startChangeSetResponse_changeSetArn' - The ARN associated to the unique identifier generated for the request.
--
-- 'httpStatus', 'startChangeSetResponse_httpStatus' - The response's http status code.
newStartChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartChangeSetResponse
newStartChangeSetResponse pHttpStatus_ =
  StartChangeSetResponse'
    { changeSetId =
        Prelude.Nothing,
      changeSetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier generated for the request.
startChangeSetResponse_changeSetId :: Lens.Lens' StartChangeSetResponse (Prelude.Maybe Prelude.Text)
startChangeSetResponse_changeSetId = Lens.lens (\StartChangeSetResponse' {changeSetId} -> changeSetId) (\s@StartChangeSetResponse' {} a -> s {changeSetId = a} :: StartChangeSetResponse)

-- | The ARN associated to the unique identifier generated for the request.
startChangeSetResponse_changeSetArn :: Lens.Lens' StartChangeSetResponse (Prelude.Maybe Prelude.Text)
startChangeSetResponse_changeSetArn = Lens.lens (\StartChangeSetResponse' {changeSetArn} -> changeSetArn) (\s@StartChangeSetResponse' {} a -> s {changeSetArn = a} :: StartChangeSetResponse)

-- | The response's http status code.
startChangeSetResponse_httpStatus :: Lens.Lens' StartChangeSetResponse Prelude.Int
startChangeSetResponse_httpStatus = Lens.lens (\StartChangeSetResponse' {httpStatus} -> httpStatus) (\s@StartChangeSetResponse' {} a -> s {httpStatus = a} :: StartChangeSetResponse)

instance Prelude.NFData StartChangeSetResponse where
  rnf StartChangeSetResponse' {..} =
    Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeSetArn
