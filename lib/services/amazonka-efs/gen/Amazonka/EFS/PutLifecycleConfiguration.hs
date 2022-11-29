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
-- Module      : Amazonka.EFS.PutLifecycleConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this action to manage EFS lifecycle management and intelligent
-- tiering. A @LifecycleConfiguration@ consists of one or more
-- @LifecyclePolicy@ objects that define the following:
--
-- -   __EFS Lifecycle management__ - When Amazon EFS automatically
--     transitions files in a file system into the lower-cost Infrequent
--     Access (IA) storage class.
--
--     To enable EFS Lifecycle management, set the value of
--     @TransitionToIA@ to one of the available options.
--
-- -   __EFS Intelligent tiering__ - When Amazon EFS automatically
--     transitions files from IA back into the file system\'s primary
--     storage class (Standard or One Zone Standard.
--
--     To enable EFS Intelligent Tiering, set the value of
--     @TransitionToPrimaryStorageClass@ to @AFTER_1_ACCESS@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/lifecycle-management-efs.html EFS Lifecycle Management>.
--
-- Each Amazon EFS file system supports one lifecycle configuration, which
-- applies to all files in the file system. If a @LifecycleConfiguration@
-- object already exists for the specified file system, a
-- @PutLifecycleConfiguration@ call modifies the existing configuration. A
-- @PutLifecycleConfiguration@ call with an empty @LifecyclePolicies@ array
-- in the request body deletes any existing @LifecycleConfiguration@ and
-- turns off lifecycle management and intelligent tiering for the file
-- system.
--
-- In the request, specify the following:
--
-- -   The ID for the file system for which you are enabling, disabling, or
--     modifying lifecycle management and intelligent tiering.
--
-- -   A @LifecyclePolicies@ array of @LifecyclePolicy@ objects that define
--     when files are moved into IA storage, and when they are moved back
--     to Standard storage.
--
--     Amazon EFS requires that each @LifecyclePolicy@ object have only
--     have a single transition, so the @LifecyclePolicies@ array needs to
--     be structured with separate @LifecyclePolicy@ objects. See the
--     example requests in the following section for more information.
--
-- This operation requires permissions for the
-- @elasticfilesystem:PutLifecycleConfiguration@ operation.
--
-- To apply a @LifecycleConfiguration@ object to an encrypted file system,
-- you need the same Key Management Service permissions as when you created
-- the encrypted file system.
module Amazonka.EFS.PutLifecycleConfiguration
  ( -- * Creating a Request
    PutLifecycleConfiguration (..),
    newPutLifecycleConfiguration,

    -- * Request Lenses
    putLifecycleConfiguration_fileSystemId,
    putLifecycleConfiguration_lifecyclePolicies,

    -- * Destructuring the Response
    LifecycleConfigurationDescription (..),
    newLifecycleConfigurationDescription,

    -- * Response Lenses
    lifecycleConfigurationDescription_lifecyclePolicies,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLifecycleConfiguration' smart constructor.
data PutLifecycleConfiguration = PutLifecycleConfiguration'
  { -- | The ID of the file system for which you are creating the
    -- @LifecycleConfiguration@ object (String).
    fileSystemId :: Prelude.Text,
    -- | An array of @LifecyclePolicy@ objects that define the file system\'s
    -- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object
    -- informs EFS lifecycle management and EFS Intelligent-Tiering of the
    -- following:
    --
    -- -   When to move files in the file system from primary storage to the IA
    --     storage class.
    --
    -- -   When to move files that are in IA storage to primary storage.
    --
    -- When using the @put-lifecycle-configuration@ CLI command or the
    -- @PutLifecycleConfiguration@ API action, Amazon EFS requires that each
    -- @LifecyclePolicy@ object have only a single transition. This means that
    -- in a request body, @LifecyclePolicies@ must be structured as an array of
    -- @LifecyclePolicy@ objects, one object for each transition,
    -- @TransitionToIA@, @TransitionToPrimaryStorageClass@. See the example
    -- requests in the following section for more information.
    lifecyclePolicies :: [LifecyclePolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'putLifecycleConfiguration_fileSystemId' - The ID of the file system for which you are creating the
-- @LifecycleConfiguration@ object (String).
--
-- 'lifecyclePolicies', 'putLifecycleConfiguration_lifecyclePolicies' - An array of @LifecyclePolicy@ objects that define the file system\'s
-- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object
-- informs EFS lifecycle management and EFS Intelligent-Tiering of the
-- following:
--
-- -   When to move files in the file system from primary storage to the IA
--     storage class.
--
-- -   When to move files that are in IA storage to primary storage.
--
-- When using the @put-lifecycle-configuration@ CLI command or the
-- @PutLifecycleConfiguration@ API action, Amazon EFS requires that each
-- @LifecyclePolicy@ object have only a single transition. This means that
-- in a request body, @LifecyclePolicies@ must be structured as an array of
-- @LifecyclePolicy@ objects, one object for each transition,
-- @TransitionToIA@, @TransitionToPrimaryStorageClass@. See the example
-- requests in the following section for more information.
newPutLifecycleConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  PutLifecycleConfiguration
newPutLifecycleConfiguration pFileSystemId_ =
  PutLifecycleConfiguration'
    { fileSystemId =
        pFileSystemId_,
      lifecyclePolicies = Prelude.mempty
    }

-- | The ID of the file system for which you are creating the
-- @LifecycleConfiguration@ object (String).
putLifecycleConfiguration_fileSystemId :: Lens.Lens' PutLifecycleConfiguration Prelude.Text
putLifecycleConfiguration_fileSystemId = Lens.lens (\PutLifecycleConfiguration' {fileSystemId} -> fileSystemId) (\s@PutLifecycleConfiguration' {} a -> s {fileSystemId = a} :: PutLifecycleConfiguration)

-- | An array of @LifecyclePolicy@ objects that define the file system\'s
-- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object
-- informs EFS lifecycle management and EFS Intelligent-Tiering of the
-- following:
--
-- -   When to move files in the file system from primary storage to the IA
--     storage class.
--
-- -   When to move files that are in IA storage to primary storage.
--
-- When using the @put-lifecycle-configuration@ CLI command or the
-- @PutLifecycleConfiguration@ API action, Amazon EFS requires that each
-- @LifecyclePolicy@ object have only a single transition. This means that
-- in a request body, @LifecyclePolicies@ must be structured as an array of
-- @LifecyclePolicy@ objects, one object for each transition,
-- @TransitionToIA@, @TransitionToPrimaryStorageClass@. See the example
-- requests in the following section for more information.
putLifecycleConfiguration_lifecyclePolicies :: Lens.Lens' PutLifecycleConfiguration [LifecyclePolicy]
putLifecycleConfiguration_lifecyclePolicies = Lens.lens (\PutLifecycleConfiguration' {lifecyclePolicies} -> lifecyclePolicies) (\s@PutLifecycleConfiguration' {} a -> s {lifecyclePolicies = a} :: PutLifecycleConfiguration) Prelude.. Lens.coerced

instance Core.AWSRequest PutLifecycleConfiguration where
  type
    AWSResponse PutLifecycleConfiguration =
      LifecycleConfigurationDescription
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable PutLifecycleConfiguration where
  hashWithSalt _salt PutLifecycleConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` lifecyclePolicies

instance Prelude.NFData PutLifecycleConfiguration where
  rnf PutLifecycleConfiguration' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf lifecyclePolicies

instance Core.ToHeaders PutLifecycleConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutLifecycleConfiguration where
  toJSON PutLifecycleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LifecyclePolicies" Core..= lifecyclePolicies)
          ]
      )

instance Core.ToPath PutLifecycleConfiguration where
  toPath PutLifecycleConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS fileSystemId,
        "/lifecycle-configuration"
      ]

instance Core.ToQuery PutLifecycleConfiguration where
  toQuery = Prelude.const Prelude.mempty
