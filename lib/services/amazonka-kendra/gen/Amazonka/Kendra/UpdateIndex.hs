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
-- Module      : Amazonka.Kendra.UpdateIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kendra index.
module Amazonka.Kendra.UpdateIndex
  ( -- * Creating a Request
    UpdateIndex (..),
    newUpdateIndex,

    -- * Request Lenses
    updateIndex_name,
    updateIndex_roleArn,
    updateIndex_capacityUnits,
    updateIndex_userGroupResolutionConfiguration,
    updateIndex_documentMetadataConfigurationUpdates,
    updateIndex_description,
    updateIndex_userTokenConfigurations,
    updateIndex_userContextPolicy,
    updateIndex_id,

    -- * Destructuring the Response
    UpdateIndexResponse (..),
    newUpdateIndexResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIndex' smart constructor.
data UpdateIndex = UpdateIndex'
  { -- | The name of the index you want to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | An Identity and Access Management (IAM) role that gives Amazon Kendra
    -- permission to access Amazon CloudWatch logs and metrics.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Sets the number of additional document storage and query capacity units
    -- that should be used by the index. You can change the capacity of the
    -- index up to 5 times per day, or make 5 API calls.
    --
    -- If you are using extra storage units, you can\'t reduce the storage
    -- capacity below what is required to meet the storage needs for your
    -- index.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | Enables fetching access levels of groups and users from an IAM Identity
    -- Center (successor to Single Sign-On) identity source. To configure this,
    -- see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | The document metadata configuration you want to update for the index.
    -- Document metadata are fields or attributes associated with your
    -- documents. For example, the company department name associated with each
    -- document.
    documentMetadataConfigurationUpdates :: Prelude.Maybe [DocumentMetadataConfiguration],
    -- | A new description for the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user token configuration.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | The user context policy.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | The identifier of the index you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateIndex_name' - The name of the index you want to update.
--
-- 'roleArn', 'updateIndex_roleArn' - An Identity and Access Management (IAM) role that gives Amazon Kendra
-- permission to access Amazon CloudWatch logs and metrics.
--
-- 'capacityUnits', 'updateIndex_capacityUnits' - Sets the number of additional document storage and query capacity units
-- that should be used by the index. You can change the capacity of the
-- index up to 5 times per day, or make 5 API calls.
--
-- If you are using extra storage units, you can\'t reduce the storage
-- capacity below what is required to meet the storage needs for your
-- index.
--
-- 'userGroupResolutionConfiguration', 'updateIndex_userGroupResolutionConfiguration' - Enables fetching access levels of groups and users from an IAM Identity
-- Center (successor to Single Sign-On) identity source. To configure this,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
--
-- 'documentMetadataConfigurationUpdates', 'updateIndex_documentMetadataConfigurationUpdates' - The document metadata configuration you want to update for the index.
-- Document metadata are fields or attributes associated with your
-- documents. For example, the company department name associated with each
-- document.
--
-- 'description', 'updateIndex_description' - A new description for the index.
--
-- 'userTokenConfigurations', 'updateIndex_userTokenConfigurations' - The user token configuration.
--
-- 'userContextPolicy', 'updateIndex_userContextPolicy' - The user context policy.
--
-- 'id', 'updateIndex_id' - The identifier of the index you want to update.
newUpdateIndex ::
  -- | 'id'
  Prelude.Text ->
  UpdateIndex
newUpdateIndex pId_ =
  UpdateIndex'
    { name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      documentMetadataConfigurationUpdates =
        Prelude.Nothing,
      description = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      id = pId_
    }

-- | The name of the index you want to update.
updateIndex_name :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_name = Lens.lens (\UpdateIndex' {name} -> name) (\s@UpdateIndex' {} a -> s {name = a} :: UpdateIndex)

-- | An Identity and Access Management (IAM) role that gives Amazon Kendra
-- permission to access Amazon CloudWatch logs and metrics.
updateIndex_roleArn :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_roleArn = Lens.lens (\UpdateIndex' {roleArn} -> roleArn) (\s@UpdateIndex' {} a -> s {roleArn = a} :: UpdateIndex)

-- | Sets the number of additional document storage and query capacity units
-- that should be used by the index. You can change the capacity of the
-- index up to 5 times per day, or make 5 API calls.
--
-- If you are using extra storage units, you can\'t reduce the storage
-- capacity below what is required to meet the storage needs for your
-- index.
updateIndex_capacityUnits :: Lens.Lens' UpdateIndex (Prelude.Maybe CapacityUnitsConfiguration)
updateIndex_capacityUnits = Lens.lens (\UpdateIndex' {capacityUnits} -> capacityUnits) (\s@UpdateIndex' {} a -> s {capacityUnits = a} :: UpdateIndex)

-- | Enables fetching access levels of groups and users from an IAM Identity
-- Center (successor to Single Sign-On) identity source. To configure this,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
updateIndex_userGroupResolutionConfiguration :: Lens.Lens' UpdateIndex (Prelude.Maybe UserGroupResolutionConfiguration)
updateIndex_userGroupResolutionConfiguration = Lens.lens (\UpdateIndex' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@UpdateIndex' {} a -> s {userGroupResolutionConfiguration = a} :: UpdateIndex)

-- | The document metadata configuration you want to update for the index.
-- Document metadata are fields or attributes associated with your
-- documents. For example, the company department name associated with each
-- document.
updateIndex_documentMetadataConfigurationUpdates :: Lens.Lens' UpdateIndex (Prelude.Maybe [DocumentMetadataConfiguration])
updateIndex_documentMetadataConfigurationUpdates = Lens.lens (\UpdateIndex' {documentMetadataConfigurationUpdates} -> documentMetadataConfigurationUpdates) (\s@UpdateIndex' {} a -> s {documentMetadataConfigurationUpdates = a} :: UpdateIndex) Prelude.. Lens.mapping Lens.coerced

-- | A new description for the index.
updateIndex_description :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_description = Lens.lens (\UpdateIndex' {description} -> description) (\s@UpdateIndex' {} a -> s {description = a} :: UpdateIndex)

-- | The user token configuration.
updateIndex_userTokenConfigurations :: Lens.Lens' UpdateIndex (Prelude.Maybe [UserTokenConfiguration])
updateIndex_userTokenConfigurations = Lens.lens (\UpdateIndex' {userTokenConfigurations} -> userTokenConfigurations) (\s@UpdateIndex' {} a -> s {userTokenConfigurations = a} :: UpdateIndex) Prelude.. Lens.mapping Lens.coerced

-- | The user context policy.
updateIndex_userContextPolicy :: Lens.Lens' UpdateIndex (Prelude.Maybe UserContextPolicy)
updateIndex_userContextPolicy = Lens.lens (\UpdateIndex' {userContextPolicy} -> userContextPolicy) (\s@UpdateIndex' {} a -> s {userContextPolicy = a} :: UpdateIndex)

-- | The identifier of the index you want to update.
updateIndex_id :: Lens.Lens' UpdateIndex Prelude.Text
updateIndex_id = Lens.lens (\UpdateIndex' {id} -> id) (\s@UpdateIndex' {} a -> s {id = a} :: UpdateIndex)

instance Core.AWSRequest UpdateIndex where
  type AWSResponse UpdateIndex = UpdateIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull UpdateIndexResponse'

instance Prelude.Hashable UpdateIndex where
  hashWithSalt _salt UpdateIndex' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` capacityUnits
      `Prelude.hashWithSalt` userGroupResolutionConfiguration
      `Prelude.hashWithSalt` documentMetadataConfigurationUpdates
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` userTokenConfigurations
      `Prelude.hashWithSalt` userContextPolicy
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateIndex where
  rnf UpdateIndex' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf userGroupResolutionConfiguration
      `Prelude.seq` Prelude.rnf documentMetadataConfigurationUpdates
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf userTokenConfigurations
      `Prelude.seq` Prelude.rnf userContextPolicy
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders UpdateIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.UpdateIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIndex where
  toJSON UpdateIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("CapacityUnits" Core..=) Prelude.<$> capacityUnits,
            ("UserGroupResolutionConfiguration" Core..=)
              Prelude.<$> userGroupResolutionConfiguration,
            ("DocumentMetadataConfigurationUpdates" Core..=)
              Prelude.<$> documentMetadataConfigurationUpdates,
            ("Description" Core..=) Prelude.<$> description,
            ("UserTokenConfigurations" Core..=)
              Prelude.<$> userTokenConfigurations,
            ("UserContextPolicy" Core..=)
              Prelude.<$> userContextPolicy,
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateIndex where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIndexResponse' smart constructor.
data UpdateIndexResponse = UpdateIndexResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateIndexResponse ::
  UpdateIndexResponse
newUpdateIndexResponse = UpdateIndexResponse'

instance Prelude.NFData UpdateIndexResponse where
  rnf _ = ()
