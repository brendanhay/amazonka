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
-- Module      : Amazonka.Kendra.DeletePrincipalMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group so that all users and sub groups that belong to the
-- group can no longer access documents only available to that group.
--
-- For example, after deleting the group \"Summer Interns\", all interns
-- who belonged to that group no longer see intern-only documents in their
-- search results.
--
-- If you want to delete or replace users or sub groups of a group, you
-- need to use the @PutPrincipalMapping@ operation. For example, if a user
-- in the group \"Engineering\" leaves the engineering team and another
-- user takes their place, you provide an updated list of users or sub
-- groups that belong to the \"Engineering\" group when calling
-- @PutPrincipalMapping@. You can update your internal list of users or sub
-- groups and input this list when calling @PutPrincipalMapping@.
--
-- @DeletePrincipalMapping@ is currently not supported in the Amazon Web
-- Services GovCloud (US-West) region.
module Amazonka.Kendra.DeletePrincipalMapping
  ( -- * Creating a Request
    DeletePrincipalMapping (..),
    newDeletePrincipalMapping,

    -- * Request Lenses
    deletePrincipalMapping_dataSourceId,
    deletePrincipalMapping_orderingId,
    deletePrincipalMapping_indexId,
    deletePrincipalMapping_groupId,

    -- * Destructuring the Response
    DeletePrincipalMappingResponse (..),
    newDeletePrincipalMappingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePrincipalMapping' smart constructor.
data DeletePrincipalMapping = DeletePrincipalMapping'
  { -- | The identifier of the data source you want to delete a group from.
    --
    -- A group can be tied to multiple data sources. You can delete a group
    -- from accessing documents in a certain data source. For example, the
    -- groups \"Research\", \"Engineering\", and \"Sales and Marketing\" are
    -- all tied to the company\'s documents stored in the data sources
    -- Confluence and Salesforce. You want to delete \"Research\" and
    -- \"Engineering\" groups from Salesforce, so that these groups cannot
    -- access customer-related documents stored in Salesforce. Only \"Sales and
    -- Marketing\" should access documents in the Salesforce data source.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp identifier you specify to ensure Amazon Kendra does not
    -- override the latest @DELETE@ action with previous actions. The highest
    -- number ID, which is the ordering ID, is the latest action you want to
    -- process and apply on top of other actions with lower number IDs. This
    -- prevents previous actions with lower number IDs from possibly overriding
    -- the latest action.
    --
    -- The ordering ID can be the UNIX time of the last update you made to a
    -- group members list. You would then provide this list when calling
    -- @PutPrincipalMapping@. This ensures your @DELETE@ action for that
    -- updated group with the latest members list doesn\'t get overwritten by
    -- earlier @DELETE@ actions for the same group which are yet to be
    -- processed.
    --
    -- The default ordering ID is the current UNIX time in milliseconds that
    -- the action was received by Amazon Kendra.
    orderingId :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index you want to delete a group from.
    indexId :: Prelude.Text,
    -- | The identifier of the group you want to delete.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePrincipalMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'deletePrincipalMapping_dataSourceId' - The identifier of the data source you want to delete a group from.
--
-- A group can be tied to multiple data sources. You can delete a group
-- from accessing documents in a certain data source. For example, the
-- groups \"Research\", \"Engineering\", and \"Sales and Marketing\" are
-- all tied to the company\'s documents stored in the data sources
-- Confluence and Salesforce. You want to delete \"Research\" and
-- \"Engineering\" groups from Salesforce, so that these groups cannot
-- access customer-related documents stored in Salesforce. Only \"Sales and
-- Marketing\" should access documents in the Salesforce data source.
--
-- 'orderingId', 'deletePrincipalMapping_orderingId' - The timestamp identifier you specify to ensure Amazon Kendra does not
-- override the latest @DELETE@ action with previous actions. The highest
-- number ID, which is the ordering ID, is the latest action you want to
-- process and apply on top of other actions with lower number IDs. This
-- prevents previous actions with lower number IDs from possibly overriding
-- the latest action.
--
-- The ordering ID can be the UNIX time of the last update you made to a
-- group members list. You would then provide this list when calling
-- @PutPrincipalMapping@. This ensures your @DELETE@ action for that
-- updated group with the latest members list doesn\'t get overwritten by
-- earlier @DELETE@ actions for the same group which are yet to be
-- processed.
--
-- The default ordering ID is the current UNIX time in milliseconds that
-- the action was received by Amazon Kendra.
--
-- 'indexId', 'deletePrincipalMapping_indexId' - The identifier of the index you want to delete a group from.
--
-- 'groupId', 'deletePrincipalMapping_groupId' - The identifier of the group you want to delete.
newDeletePrincipalMapping ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  DeletePrincipalMapping
newDeletePrincipalMapping pIndexId_ pGroupId_ =
  DeletePrincipalMapping'
    { dataSourceId =
        Prelude.Nothing,
      orderingId = Prelude.Nothing,
      indexId = pIndexId_,
      groupId = pGroupId_
    }

-- | The identifier of the data source you want to delete a group from.
--
-- A group can be tied to multiple data sources. You can delete a group
-- from accessing documents in a certain data source. For example, the
-- groups \"Research\", \"Engineering\", and \"Sales and Marketing\" are
-- all tied to the company\'s documents stored in the data sources
-- Confluence and Salesforce. You want to delete \"Research\" and
-- \"Engineering\" groups from Salesforce, so that these groups cannot
-- access customer-related documents stored in Salesforce. Only \"Sales and
-- Marketing\" should access documents in the Salesforce data source.
deletePrincipalMapping_dataSourceId :: Lens.Lens' DeletePrincipalMapping (Prelude.Maybe Prelude.Text)
deletePrincipalMapping_dataSourceId = Lens.lens (\DeletePrincipalMapping' {dataSourceId} -> dataSourceId) (\s@DeletePrincipalMapping' {} a -> s {dataSourceId = a} :: DeletePrincipalMapping)

-- | The timestamp identifier you specify to ensure Amazon Kendra does not
-- override the latest @DELETE@ action with previous actions. The highest
-- number ID, which is the ordering ID, is the latest action you want to
-- process and apply on top of other actions with lower number IDs. This
-- prevents previous actions with lower number IDs from possibly overriding
-- the latest action.
--
-- The ordering ID can be the UNIX time of the last update you made to a
-- group members list. You would then provide this list when calling
-- @PutPrincipalMapping@. This ensures your @DELETE@ action for that
-- updated group with the latest members list doesn\'t get overwritten by
-- earlier @DELETE@ actions for the same group which are yet to be
-- processed.
--
-- The default ordering ID is the current UNIX time in milliseconds that
-- the action was received by Amazon Kendra.
deletePrincipalMapping_orderingId :: Lens.Lens' DeletePrincipalMapping (Prelude.Maybe Prelude.Natural)
deletePrincipalMapping_orderingId = Lens.lens (\DeletePrincipalMapping' {orderingId} -> orderingId) (\s@DeletePrincipalMapping' {} a -> s {orderingId = a} :: DeletePrincipalMapping)

-- | The identifier of the index you want to delete a group from.
deletePrincipalMapping_indexId :: Lens.Lens' DeletePrincipalMapping Prelude.Text
deletePrincipalMapping_indexId = Lens.lens (\DeletePrincipalMapping' {indexId} -> indexId) (\s@DeletePrincipalMapping' {} a -> s {indexId = a} :: DeletePrincipalMapping)

-- | The identifier of the group you want to delete.
deletePrincipalMapping_groupId :: Lens.Lens' DeletePrincipalMapping Prelude.Text
deletePrincipalMapping_groupId = Lens.lens (\DeletePrincipalMapping' {groupId} -> groupId) (\s@DeletePrincipalMapping' {} a -> s {groupId = a} :: DeletePrincipalMapping)

instance Core.AWSRequest DeletePrincipalMapping where
  type
    AWSResponse DeletePrincipalMapping =
      DeletePrincipalMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeletePrincipalMappingResponse'

instance Prelude.Hashable DeletePrincipalMapping where
  hashWithSalt _salt DeletePrincipalMapping' {..} =
    _salt `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` orderingId
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData DeletePrincipalMapping where
  rnf DeletePrincipalMapping' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf orderingId
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders DeletePrincipalMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeletePrincipalMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePrincipalMapping where
  toJSON DeletePrincipalMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Data..=) Prelude.<$> dataSourceId,
            ("OrderingId" Data..=) Prelude.<$> orderingId,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

instance Data.ToPath DeletePrincipalMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePrincipalMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePrincipalMappingResponse' smart constructor.
data DeletePrincipalMappingResponse = DeletePrincipalMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePrincipalMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePrincipalMappingResponse ::
  DeletePrincipalMappingResponse
newDeletePrincipalMappingResponse =
  DeletePrincipalMappingResponse'

instance
  Prelude.NFData
    DeletePrincipalMappingResponse
  where
  rnf _ = ()
