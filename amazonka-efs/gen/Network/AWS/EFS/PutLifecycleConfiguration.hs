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
-- Module      : Network.AWS.EFS.PutLifecycleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables lifecycle management by creating a new @LifecycleConfiguration@
-- object. A @LifecycleConfiguration@ object defines when files in an
-- Amazon EFS file system are automatically transitioned to the lower-cost
-- EFS Infrequent Access (IA) storage class. To enable EFS Intelligent
-- Tiering, set the value of @TransitionToPrimaryStorageClass@ to
-- @AFTER_1_ACCESS@. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/lifecycle-management-efs.html EFS Lifecycle Management>.
--
-- A @LifecycleConfiguration@ applies to all files in a file system.
--
-- Each Amazon EFS file system supports one lifecycle configuration, which
-- applies to all files in the file system. If a @LifecycleConfiguration@
-- object already exists for the specified file system, a
-- @PutLifecycleConfiguration@ call modifies the existing configuration. A
-- @PutLifecycleConfiguration@ call with an empty @LifecyclePolicies@ array
-- in the request body deletes any existing @LifecycleConfiguration@ and
-- disables lifecycle management.
--
-- In the request, specify the following:
--
-- -   The ID for the file system for which you are enabling, disabling, or
--     modifying lifecycle management.
--
-- -   A @LifecyclePolicies@ array of @LifecyclePolicy@ objects that define
--     when files are moved to the IA storage class. The array can contain
--     only one @LifecyclePolicy@ item.
--
-- This operation requires permissions for the
-- @elasticfilesystem:PutLifecycleConfiguration@ operation.
--
-- To apply a @LifecycleConfiguration@ object to an encrypted file system,
-- you need the same Key Management Service permissions as when you created
-- the encrypted file system.
module Network.AWS.EFS.PutLifecycleConfiguration
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

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLifecycleConfiguration' smart constructor.
data PutLifecycleConfiguration = PutLifecycleConfiguration'
  { -- | The ID of the file system for which you are creating the
    -- @LifecycleConfiguration@ object (String).
    fileSystemId :: Prelude.Text,
    -- | An array of @LifecyclePolicy@ objects that define the file system\'s
    -- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells
    -- lifecycle management when to transition files from the Standard storage
    -- class to the Infrequent Access storage class.
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
-- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells
-- lifecycle management when to transition files from the Standard storage
-- class to the Infrequent Access storage class.
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
-- @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells
-- lifecycle management when to transition files from the Standard storage
-- class to the Infrequent Access storage class.
putLifecycleConfiguration_lifecyclePolicies :: Lens.Lens' PutLifecycleConfiguration [LifecyclePolicy]
putLifecycleConfiguration_lifecyclePolicies = Lens.lens (\PutLifecycleConfiguration' {lifecyclePolicies} -> lifecyclePolicies) (\s@PutLifecycleConfiguration' {} a -> s {lifecyclePolicies = a} :: PutLifecycleConfiguration) Prelude.. Lens._Coerce

instance Core.AWSRequest PutLifecycleConfiguration where
  type
    AWSResponse PutLifecycleConfiguration =
      LifecycleConfigurationDescription
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable PutLifecycleConfiguration

instance Prelude.NFData PutLifecycleConfiguration

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
