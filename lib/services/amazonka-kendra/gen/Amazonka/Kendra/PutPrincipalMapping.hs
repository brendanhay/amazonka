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
-- Module      : Amazonka.Kendra.PutPrincipalMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps users to their groups so that you only need to provide the user ID
-- when you issue the query.
--
-- You can also map sub groups to groups. For example, the group \"Company
-- Intellectual Property Teams\" includes sub groups \"Research\" and
-- \"Engineering\". These sub groups include their own list of users or
-- people who work in these teams. Only users who work in research and
-- engineering, and therefore belong in the intellectual property group,
-- can see top-secret company documents in their search results.
--
-- This is useful for user context filtering, where search results are
-- filtered based on the user or their group access to documents. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html Filtering on user context>.
--
-- If more than five @PUT@ actions for a group are currently processing, a
-- validation exception is thrown.
module Amazonka.Kendra.PutPrincipalMapping
  ( -- * Creating a Request
    PutPrincipalMapping (..),
    newPutPrincipalMapping,

    -- * Request Lenses
    putPrincipalMapping_dataSourceId,
    putPrincipalMapping_orderingId,
    putPrincipalMapping_roleArn,
    putPrincipalMapping_indexId,
    putPrincipalMapping_groupId,
    putPrincipalMapping_groupMembers,

    -- * Destructuring the Response
    PutPrincipalMappingResponse (..),
    newPutPrincipalMappingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPrincipalMapping' smart constructor.
data PutPrincipalMapping = PutPrincipalMapping'
  { -- | The identifier of the data source you want to map users to their groups.
    --
    -- This is useful if a group is tied to multiple data sources, but you only
    -- want the group to access documents of a certain data source. For
    -- example, the groups \"Research\", \"Engineering\", and \"Sales and
    -- Marketing\" are all tied to the company\'s documents stored in the data
    -- sources Confluence and Salesforce. However, \"Sales and Marketing\" team
    -- only needs access to customer-related documents stored in Salesforce.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp identifier you specify to ensure Amazon Kendra does not
    -- override the latest @PUT@ action with previous actions. The highest
    -- number ID, which is the ordering ID, is the latest action you want to
    -- process and apply on top of other actions with lower number IDs. This
    -- prevents previous actions with lower number IDs from possibly overriding
    -- the latest action.
    --
    -- The ordering ID can be the Unix time of the last update you made to a
    -- group members list. You would then provide this list when calling
    -- @PutPrincipalMapping@. This ensures your @PUT@ action for that updated
    -- group with the latest members list doesn\'t get overwritten by earlier
    -- @PUT@ actions for the same group which are yet to be processed.
    --
    -- The default ordering ID is the current Unix time in milliseconds that
    -- the action was received by Amazon Kendra.
    orderingId :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of a role that has access to the S3 file
    -- that contains your list of users or sub groups that belong to a group.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html#iam-roles-ds IAM roles for Amazon Kendra>.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index you want to map users to their groups.
    indexId :: Prelude.Text,
    -- | The identifier of the group you want to map its users to.
    groupId :: Prelude.Text,
    -- | The list that contains your users or sub groups that belong the same
    -- group.
    --
    -- For example, the group \"Company\" includes the user \"CEO\" and the sub
    -- groups \"Research\", \"Engineering\", and \"Sales and Marketing\".
    --
    -- If you have more than 1000 users and\/or sub groups for a single group,
    -- you need to provide the path to the S3 file that lists your users and
    -- sub groups for a group. Your sub groups can contain more than 1000
    -- users, but the list of sub groups that belong to a group (and\/or users)
    -- must be no more than 1000.
    groupMembers :: GroupMembers
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPrincipalMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'putPrincipalMapping_dataSourceId' - The identifier of the data source you want to map users to their groups.
--
-- This is useful if a group is tied to multiple data sources, but you only
-- want the group to access documents of a certain data source. For
-- example, the groups \"Research\", \"Engineering\", and \"Sales and
-- Marketing\" are all tied to the company\'s documents stored in the data
-- sources Confluence and Salesforce. However, \"Sales and Marketing\" team
-- only needs access to customer-related documents stored in Salesforce.
--
-- 'orderingId', 'putPrincipalMapping_orderingId' - The timestamp identifier you specify to ensure Amazon Kendra does not
-- override the latest @PUT@ action with previous actions. The highest
-- number ID, which is the ordering ID, is the latest action you want to
-- process and apply on top of other actions with lower number IDs. This
-- prevents previous actions with lower number IDs from possibly overriding
-- the latest action.
--
-- The ordering ID can be the Unix time of the last update you made to a
-- group members list. You would then provide this list when calling
-- @PutPrincipalMapping@. This ensures your @PUT@ action for that updated
-- group with the latest members list doesn\'t get overwritten by earlier
-- @PUT@ actions for the same group which are yet to be processed.
--
-- The default ordering ID is the current Unix time in milliseconds that
-- the action was received by Amazon Kendra.
--
-- 'roleArn', 'putPrincipalMapping_roleArn' - The Amazon Resource Name (ARN) of a role that has access to the S3 file
-- that contains your list of users or sub groups that belong to a group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html#iam-roles-ds IAM roles for Amazon Kendra>.
--
-- 'indexId', 'putPrincipalMapping_indexId' - The identifier of the index you want to map users to their groups.
--
-- 'groupId', 'putPrincipalMapping_groupId' - The identifier of the group you want to map its users to.
--
-- 'groupMembers', 'putPrincipalMapping_groupMembers' - The list that contains your users or sub groups that belong the same
-- group.
--
-- For example, the group \"Company\" includes the user \"CEO\" and the sub
-- groups \"Research\", \"Engineering\", and \"Sales and Marketing\".
--
-- If you have more than 1000 users and\/or sub groups for a single group,
-- you need to provide the path to the S3 file that lists your users and
-- sub groups for a group. Your sub groups can contain more than 1000
-- users, but the list of sub groups that belong to a group (and\/or users)
-- must be no more than 1000.
newPutPrincipalMapping ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'groupMembers'
  GroupMembers ->
  PutPrincipalMapping
newPutPrincipalMapping
  pIndexId_
  pGroupId_
  pGroupMembers_ =
    PutPrincipalMapping'
      { dataSourceId =
          Prelude.Nothing,
        orderingId = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        indexId = pIndexId_,
        groupId = pGroupId_,
        groupMembers = pGroupMembers_
      }

-- | The identifier of the data source you want to map users to their groups.
--
-- This is useful if a group is tied to multiple data sources, but you only
-- want the group to access documents of a certain data source. For
-- example, the groups \"Research\", \"Engineering\", and \"Sales and
-- Marketing\" are all tied to the company\'s documents stored in the data
-- sources Confluence and Salesforce. However, \"Sales and Marketing\" team
-- only needs access to customer-related documents stored in Salesforce.
putPrincipalMapping_dataSourceId :: Lens.Lens' PutPrincipalMapping (Prelude.Maybe Prelude.Text)
putPrincipalMapping_dataSourceId = Lens.lens (\PutPrincipalMapping' {dataSourceId} -> dataSourceId) (\s@PutPrincipalMapping' {} a -> s {dataSourceId = a} :: PutPrincipalMapping)

-- | The timestamp identifier you specify to ensure Amazon Kendra does not
-- override the latest @PUT@ action with previous actions. The highest
-- number ID, which is the ordering ID, is the latest action you want to
-- process and apply on top of other actions with lower number IDs. This
-- prevents previous actions with lower number IDs from possibly overriding
-- the latest action.
--
-- The ordering ID can be the Unix time of the last update you made to a
-- group members list. You would then provide this list when calling
-- @PutPrincipalMapping@. This ensures your @PUT@ action for that updated
-- group with the latest members list doesn\'t get overwritten by earlier
-- @PUT@ actions for the same group which are yet to be processed.
--
-- The default ordering ID is the current Unix time in milliseconds that
-- the action was received by Amazon Kendra.
putPrincipalMapping_orderingId :: Lens.Lens' PutPrincipalMapping (Prelude.Maybe Prelude.Natural)
putPrincipalMapping_orderingId = Lens.lens (\PutPrincipalMapping' {orderingId} -> orderingId) (\s@PutPrincipalMapping' {} a -> s {orderingId = a} :: PutPrincipalMapping)

-- | The Amazon Resource Name (ARN) of a role that has access to the S3 file
-- that contains your list of users or sub groups that belong to a group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html#iam-roles-ds IAM roles for Amazon Kendra>.
putPrincipalMapping_roleArn :: Lens.Lens' PutPrincipalMapping (Prelude.Maybe Prelude.Text)
putPrincipalMapping_roleArn = Lens.lens (\PutPrincipalMapping' {roleArn} -> roleArn) (\s@PutPrincipalMapping' {} a -> s {roleArn = a} :: PutPrincipalMapping)

-- | The identifier of the index you want to map users to their groups.
putPrincipalMapping_indexId :: Lens.Lens' PutPrincipalMapping Prelude.Text
putPrincipalMapping_indexId = Lens.lens (\PutPrincipalMapping' {indexId} -> indexId) (\s@PutPrincipalMapping' {} a -> s {indexId = a} :: PutPrincipalMapping)

-- | The identifier of the group you want to map its users to.
putPrincipalMapping_groupId :: Lens.Lens' PutPrincipalMapping Prelude.Text
putPrincipalMapping_groupId = Lens.lens (\PutPrincipalMapping' {groupId} -> groupId) (\s@PutPrincipalMapping' {} a -> s {groupId = a} :: PutPrincipalMapping)

-- | The list that contains your users or sub groups that belong the same
-- group.
--
-- For example, the group \"Company\" includes the user \"CEO\" and the sub
-- groups \"Research\", \"Engineering\", and \"Sales and Marketing\".
--
-- If you have more than 1000 users and\/or sub groups for a single group,
-- you need to provide the path to the S3 file that lists your users and
-- sub groups for a group. Your sub groups can contain more than 1000
-- users, but the list of sub groups that belong to a group (and\/or users)
-- must be no more than 1000.
putPrincipalMapping_groupMembers :: Lens.Lens' PutPrincipalMapping GroupMembers
putPrincipalMapping_groupMembers = Lens.lens (\PutPrincipalMapping' {groupMembers} -> groupMembers) (\s@PutPrincipalMapping' {} a -> s {groupMembers = a} :: PutPrincipalMapping)

instance Core.AWSRequest PutPrincipalMapping where
  type
    AWSResponse PutPrincipalMapping =
      PutPrincipalMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutPrincipalMappingResponse'

instance Prelude.Hashable PutPrincipalMapping where
  hashWithSalt _salt PutPrincipalMapping' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` orderingId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupMembers

instance Prelude.NFData PutPrincipalMapping where
  rnf PutPrincipalMapping' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf orderingId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupMembers

instance Data.ToHeaders PutPrincipalMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.PutPrincipalMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPrincipalMapping where
  toJSON PutPrincipalMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Data..=) Prelude.<$> dataSourceId,
            ("OrderingId" Data..=) Prelude.<$> orderingId,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("GroupMembers" Data..= groupMembers)
          ]
      )

instance Data.ToPath PutPrincipalMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery PutPrincipalMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPrincipalMappingResponse' smart constructor.
data PutPrincipalMappingResponse = PutPrincipalMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPrincipalMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutPrincipalMappingResponse ::
  PutPrincipalMappingResponse
newPutPrincipalMappingResponse =
  PutPrincipalMappingResponse'

instance Prelude.NFData PutPrincipalMappingResponse where
  rnf _ = ()
