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
-- Module      : Amazonka.Kendra.DescribePrincipalMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the processing of @PUT@ and @DELETE@ actions for mapping users
-- to their groups. This includes information on the status of actions
-- currently processing or yet to be processed, when actions were last
-- updated, when actions were received by Amazon Kendra, the latest action
-- that should process and apply after other actions, and useful error
-- messages if an action could not be processed.
--
-- @DescribePrincipalMapping@ is currently not supported in the Amazon Web
-- Services GovCloud (US-West) region.
module Amazonka.Kendra.DescribePrincipalMapping
  ( -- * Creating a Request
    DescribePrincipalMapping (..),
    newDescribePrincipalMapping,

    -- * Request Lenses
    describePrincipalMapping_dataSourceId,
    describePrincipalMapping_indexId,
    describePrincipalMapping_groupId,

    -- * Destructuring the Response
    DescribePrincipalMappingResponse (..),
    newDescribePrincipalMappingResponse,

    -- * Response Lenses
    describePrincipalMappingResponse_dataSourceId,
    describePrincipalMappingResponse_groupId,
    describePrincipalMappingResponse_groupOrderingIdSummaries,
    describePrincipalMappingResponse_indexId,
    describePrincipalMappingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePrincipalMapping' smart constructor.
data DescribePrincipalMapping = DescribePrincipalMapping'
  { -- | The identifier of the data source to check the processing of @PUT@ and
    -- @DELETE@ actions for mapping users to their groups.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index required to check the processing of @PUT@
    -- and @DELETE@ actions for mapping users to their groups.
    indexId :: Prelude.Text,
    -- | The identifier of the group required to check the processing of @PUT@
    -- and @DELETE@ actions for mapping users to their groups.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePrincipalMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'describePrincipalMapping_dataSourceId' - The identifier of the data source to check the processing of @PUT@ and
-- @DELETE@ actions for mapping users to their groups.
--
-- 'indexId', 'describePrincipalMapping_indexId' - The identifier of the index required to check the processing of @PUT@
-- and @DELETE@ actions for mapping users to their groups.
--
-- 'groupId', 'describePrincipalMapping_groupId' - The identifier of the group required to check the processing of @PUT@
-- and @DELETE@ actions for mapping users to their groups.
newDescribePrincipalMapping ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  DescribePrincipalMapping
newDescribePrincipalMapping pIndexId_ pGroupId_ =
  DescribePrincipalMapping'
    { dataSourceId =
        Prelude.Nothing,
      indexId = pIndexId_,
      groupId = pGroupId_
    }

-- | The identifier of the data source to check the processing of @PUT@ and
-- @DELETE@ actions for mapping users to their groups.
describePrincipalMapping_dataSourceId :: Lens.Lens' DescribePrincipalMapping (Prelude.Maybe Prelude.Text)
describePrincipalMapping_dataSourceId = Lens.lens (\DescribePrincipalMapping' {dataSourceId} -> dataSourceId) (\s@DescribePrincipalMapping' {} a -> s {dataSourceId = a} :: DescribePrincipalMapping)

-- | The identifier of the index required to check the processing of @PUT@
-- and @DELETE@ actions for mapping users to their groups.
describePrincipalMapping_indexId :: Lens.Lens' DescribePrincipalMapping Prelude.Text
describePrincipalMapping_indexId = Lens.lens (\DescribePrincipalMapping' {indexId} -> indexId) (\s@DescribePrincipalMapping' {} a -> s {indexId = a} :: DescribePrincipalMapping)

-- | The identifier of the group required to check the processing of @PUT@
-- and @DELETE@ actions for mapping users to their groups.
describePrincipalMapping_groupId :: Lens.Lens' DescribePrincipalMapping Prelude.Text
describePrincipalMapping_groupId = Lens.lens (\DescribePrincipalMapping' {groupId} -> groupId) (\s@DescribePrincipalMapping' {} a -> s {groupId = a} :: DescribePrincipalMapping)

instance Core.AWSRequest DescribePrincipalMapping where
  type
    AWSResponse DescribePrincipalMapping =
      DescribePrincipalMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePrincipalMappingResponse'
            Prelude.<$> (x Data..?> "DataSourceId")
            Prelude.<*> (x Data..?> "GroupId")
            Prelude.<*> ( x
                            Data..?> "GroupOrderingIdSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "IndexId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePrincipalMapping where
  hashWithSalt _salt DescribePrincipalMapping' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData DescribePrincipalMapping where
  rnf DescribePrincipalMapping' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders DescribePrincipalMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribePrincipalMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePrincipalMapping where
  toJSON DescribePrincipalMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Data..=) Prelude.<$> dataSourceId,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

instance Data.ToPath DescribePrincipalMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePrincipalMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePrincipalMappingResponse' smart constructor.
data DescribePrincipalMappingResponse = DescribePrincipalMappingResponse'
  { -- | Shows the identifier of the data source to see information on the
    -- processing of @PUT@ and @DELETE@ actions for mapping users to their
    -- groups.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | Shows the identifier of the group to see information on the processing
    -- of @PUT@ and @DELETE@ actions for mapping users to their groups.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | Shows the following information on the processing of @PUT@ and @DELETE@
    -- actions for mapping users to their groups:
    --
    -- -   Status – the status can be either @PROCESSING@, @SUCCEEDED@,
    --     @DELETING@, @DELETED@, or @FAILED@.
    --
    -- -   Last updated – the last date-time an action was updated.
    --
    -- -   Received – the last date-time an action was received or submitted.
    --
    -- -   Ordering ID – the latest action that should process and apply after
    --     other actions.
    --
    -- -   Failure reason – the reason an action could not be processed.
    groupOrderingIdSummaries :: Prelude.Maybe [GroupOrderingIdSummary],
    -- | Shows the identifier of the index to see information on the processing
    -- of @PUT@ and @DELETE@ actions for mapping users to their groups.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePrincipalMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'describePrincipalMappingResponse_dataSourceId' - Shows the identifier of the data source to see information on the
-- processing of @PUT@ and @DELETE@ actions for mapping users to their
-- groups.
--
-- 'groupId', 'describePrincipalMappingResponse_groupId' - Shows the identifier of the group to see information on the processing
-- of @PUT@ and @DELETE@ actions for mapping users to their groups.
--
-- 'groupOrderingIdSummaries', 'describePrincipalMappingResponse_groupOrderingIdSummaries' - Shows the following information on the processing of @PUT@ and @DELETE@
-- actions for mapping users to their groups:
--
-- -   Status – the status can be either @PROCESSING@, @SUCCEEDED@,
--     @DELETING@, @DELETED@, or @FAILED@.
--
-- -   Last updated – the last date-time an action was updated.
--
-- -   Received – the last date-time an action was received or submitted.
--
-- -   Ordering ID – the latest action that should process and apply after
--     other actions.
--
-- -   Failure reason – the reason an action could not be processed.
--
-- 'indexId', 'describePrincipalMappingResponse_indexId' - Shows the identifier of the index to see information on the processing
-- of @PUT@ and @DELETE@ actions for mapping users to their groups.
--
-- 'httpStatus', 'describePrincipalMappingResponse_httpStatus' - The response's http status code.
newDescribePrincipalMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePrincipalMappingResponse
newDescribePrincipalMappingResponse pHttpStatus_ =
  DescribePrincipalMappingResponse'
    { dataSourceId =
        Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupOrderingIdSummaries =
        Prelude.Nothing,
      indexId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Shows the identifier of the data source to see information on the
-- processing of @PUT@ and @DELETE@ actions for mapping users to their
-- groups.
describePrincipalMappingResponse_dataSourceId :: Lens.Lens' DescribePrincipalMappingResponse (Prelude.Maybe Prelude.Text)
describePrincipalMappingResponse_dataSourceId = Lens.lens (\DescribePrincipalMappingResponse' {dataSourceId} -> dataSourceId) (\s@DescribePrincipalMappingResponse' {} a -> s {dataSourceId = a} :: DescribePrincipalMappingResponse)

-- | Shows the identifier of the group to see information on the processing
-- of @PUT@ and @DELETE@ actions for mapping users to their groups.
describePrincipalMappingResponse_groupId :: Lens.Lens' DescribePrincipalMappingResponse (Prelude.Maybe Prelude.Text)
describePrincipalMappingResponse_groupId = Lens.lens (\DescribePrincipalMappingResponse' {groupId} -> groupId) (\s@DescribePrincipalMappingResponse' {} a -> s {groupId = a} :: DescribePrincipalMappingResponse)

-- | Shows the following information on the processing of @PUT@ and @DELETE@
-- actions for mapping users to their groups:
--
-- -   Status – the status can be either @PROCESSING@, @SUCCEEDED@,
--     @DELETING@, @DELETED@, or @FAILED@.
--
-- -   Last updated – the last date-time an action was updated.
--
-- -   Received – the last date-time an action was received or submitted.
--
-- -   Ordering ID – the latest action that should process and apply after
--     other actions.
--
-- -   Failure reason – the reason an action could not be processed.
describePrincipalMappingResponse_groupOrderingIdSummaries :: Lens.Lens' DescribePrincipalMappingResponse (Prelude.Maybe [GroupOrderingIdSummary])
describePrincipalMappingResponse_groupOrderingIdSummaries = Lens.lens (\DescribePrincipalMappingResponse' {groupOrderingIdSummaries} -> groupOrderingIdSummaries) (\s@DescribePrincipalMappingResponse' {} a -> s {groupOrderingIdSummaries = a} :: DescribePrincipalMappingResponse) Prelude.. Lens.mapping Lens.coerced

-- | Shows the identifier of the index to see information on the processing
-- of @PUT@ and @DELETE@ actions for mapping users to their groups.
describePrincipalMappingResponse_indexId :: Lens.Lens' DescribePrincipalMappingResponse (Prelude.Maybe Prelude.Text)
describePrincipalMappingResponse_indexId = Lens.lens (\DescribePrincipalMappingResponse' {indexId} -> indexId) (\s@DescribePrincipalMappingResponse' {} a -> s {indexId = a} :: DescribePrincipalMappingResponse)

-- | The response's http status code.
describePrincipalMappingResponse_httpStatus :: Lens.Lens' DescribePrincipalMappingResponse Prelude.Int
describePrincipalMappingResponse_httpStatus = Lens.lens (\DescribePrincipalMappingResponse' {httpStatus} -> httpStatus) (\s@DescribePrincipalMappingResponse' {} a -> s {httpStatus = a} :: DescribePrincipalMappingResponse)

instance
  Prelude.NFData
    DescribePrincipalMappingResponse
  where
  rnf DescribePrincipalMappingResponse' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupOrderingIdSummaries
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf httpStatus
