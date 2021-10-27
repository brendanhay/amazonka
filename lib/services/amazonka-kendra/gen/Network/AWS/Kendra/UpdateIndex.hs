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
-- Module      : Network.AWS.Kendra.UpdateIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kendra index.
module Network.AWS.Kendra.UpdateIndex
  ( -- * Creating a Request
    UpdateIndex (..),
    newUpdateIndex,

    -- * Request Lenses
    updateIndex_documentMetadataConfigurationUpdates,
    updateIndex_capacityUnits,
    updateIndex_name,
    updateIndex_userGroupResolutionConfiguration,
    updateIndex_description,
    updateIndex_userContextPolicy,
    updateIndex_userTokenConfigurations,
    updateIndex_roleArn,
    updateIndex_id,

    -- * Destructuring the Response
    UpdateIndexResponse (..),
    newUpdateIndexResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIndex' smart constructor.
data UpdateIndex = UpdateIndex'
  { -- | The document metadata to update.
    documentMetadataConfigurationUpdates :: Prelude.Maybe [DocumentMetadataConfiguration],
    -- | Sets the number of additional storage and query capacity units that
    -- should be used by the index. You can change the capacity of the index up
    -- to 5 times per day.
    --
    -- If you are using extra storage units, you can\'t reduce the storage
    -- capacity below that required to meet the storage needs for your index.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | The name of the index to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | Enables fetching access levels of groups and users from an AWS Single
    -- Sign-On identity source. To configure this, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | A new description for the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user context policy.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | The user token configuration.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | A new IAM role that gives Amazon Kendra permission to access your Amazon
    -- CloudWatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index to update.
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
-- 'documentMetadataConfigurationUpdates', 'updateIndex_documentMetadataConfigurationUpdates' - The document metadata to update.
--
-- 'capacityUnits', 'updateIndex_capacityUnits' - Sets the number of additional storage and query capacity units that
-- should be used by the index. You can change the capacity of the index up
-- to 5 times per day.
--
-- If you are using extra storage units, you can\'t reduce the storage
-- capacity below that required to meet the storage needs for your index.
--
-- 'name', 'updateIndex_name' - The name of the index to update.
--
-- 'userGroupResolutionConfiguration', 'updateIndex_userGroupResolutionConfiguration' - Enables fetching access levels of groups and users from an AWS Single
-- Sign-On identity source. To configure this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
--
-- 'description', 'updateIndex_description' - A new description for the index.
--
-- 'userContextPolicy', 'updateIndex_userContextPolicy' - The user context policy.
--
-- 'userTokenConfigurations', 'updateIndex_userTokenConfigurations' - The user token configuration.
--
-- 'roleArn', 'updateIndex_roleArn' - A new IAM role that gives Amazon Kendra permission to access your Amazon
-- CloudWatch logs.
--
-- 'id', 'updateIndex_id' - The identifier of the index to update.
newUpdateIndex ::
  -- | 'id'
  Prelude.Text ->
  UpdateIndex
newUpdateIndex pId_ =
  UpdateIndex'
    { documentMetadataConfigurationUpdates =
        Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      name = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      id = pId_
    }

-- | The document metadata to update.
updateIndex_documentMetadataConfigurationUpdates :: Lens.Lens' UpdateIndex (Prelude.Maybe [DocumentMetadataConfiguration])
updateIndex_documentMetadataConfigurationUpdates = Lens.lens (\UpdateIndex' {documentMetadataConfigurationUpdates} -> documentMetadataConfigurationUpdates) (\s@UpdateIndex' {} a -> s {documentMetadataConfigurationUpdates = a} :: UpdateIndex) Prelude.. Lens.mapping Lens.coerced

-- | Sets the number of additional storage and query capacity units that
-- should be used by the index. You can change the capacity of the index up
-- to 5 times per day.
--
-- If you are using extra storage units, you can\'t reduce the storage
-- capacity below that required to meet the storage needs for your index.
updateIndex_capacityUnits :: Lens.Lens' UpdateIndex (Prelude.Maybe CapacityUnitsConfiguration)
updateIndex_capacityUnits = Lens.lens (\UpdateIndex' {capacityUnits} -> capacityUnits) (\s@UpdateIndex' {} a -> s {capacityUnits = a} :: UpdateIndex)

-- | The name of the index to update.
updateIndex_name :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_name = Lens.lens (\UpdateIndex' {name} -> name) (\s@UpdateIndex' {} a -> s {name = a} :: UpdateIndex)

-- | Enables fetching access levels of groups and users from an AWS Single
-- Sign-On identity source. To configure this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
updateIndex_userGroupResolutionConfiguration :: Lens.Lens' UpdateIndex (Prelude.Maybe UserGroupResolutionConfiguration)
updateIndex_userGroupResolutionConfiguration = Lens.lens (\UpdateIndex' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@UpdateIndex' {} a -> s {userGroupResolutionConfiguration = a} :: UpdateIndex)

-- | A new description for the index.
updateIndex_description :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_description = Lens.lens (\UpdateIndex' {description} -> description) (\s@UpdateIndex' {} a -> s {description = a} :: UpdateIndex)

-- | The user context policy.
updateIndex_userContextPolicy :: Lens.Lens' UpdateIndex (Prelude.Maybe UserContextPolicy)
updateIndex_userContextPolicy = Lens.lens (\UpdateIndex' {userContextPolicy} -> userContextPolicy) (\s@UpdateIndex' {} a -> s {userContextPolicy = a} :: UpdateIndex)

-- | The user token configuration.
updateIndex_userTokenConfigurations :: Lens.Lens' UpdateIndex (Prelude.Maybe [UserTokenConfiguration])
updateIndex_userTokenConfigurations = Lens.lens (\UpdateIndex' {userTokenConfigurations} -> userTokenConfigurations) (\s@UpdateIndex' {} a -> s {userTokenConfigurations = a} :: UpdateIndex) Prelude.. Lens.mapping Lens.coerced

-- | A new IAM role that gives Amazon Kendra permission to access your Amazon
-- CloudWatch logs.
updateIndex_roleArn :: Lens.Lens' UpdateIndex (Prelude.Maybe Prelude.Text)
updateIndex_roleArn = Lens.lens (\UpdateIndex' {roleArn} -> roleArn) (\s@UpdateIndex' {} a -> s {roleArn = a} :: UpdateIndex)

-- | The identifier of the index to update.
updateIndex_id :: Lens.Lens' UpdateIndex Prelude.Text
updateIndex_id = Lens.lens (\UpdateIndex' {id} -> id) (\s@UpdateIndex' {} a -> s {id = a} :: UpdateIndex)

instance Core.AWSRequest UpdateIndex where
  type AWSResponse UpdateIndex = UpdateIndexResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateIndexResponse'

instance Prelude.Hashable UpdateIndex

instance Prelude.NFData UpdateIndex

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
          [ ("DocumentMetadataConfigurationUpdates" Core..=)
              Prelude.<$> documentMetadataConfigurationUpdates,
            ("CapacityUnits" Core..=) Prelude.<$> capacityUnits,
            ("Name" Core..=) Prelude.<$> name,
            ("UserGroupResolutionConfiguration" Core..=)
              Prelude.<$> userGroupResolutionConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("UserContextPolicy" Core..=)
              Prelude.<$> userContextPolicy,
            ("UserTokenConfigurations" Core..=)
              Prelude.<$> userTokenConfigurations,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
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

instance Prelude.NFData UpdateIndexResponse
