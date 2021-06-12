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
-- Module      : Network.AWS.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides identifying details of the resource being migrated so that it
-- can be associated in the Application Discovery Service repository. This
-- association occurs asynchronously after @PutResourceAttributes@ returns.
--
-- -   Keep in mind that subsequent calls to PutResourceAttributes will
--     override previously stored attributes. For example, if it is first
--     called with a MAC address, but later, it is desired to /add/ an IP
--     address, it will then be required to call it with /both/ the IP and
--     MAC addresses to prevent overriding the MAC address.
--
-- -   Note the instructions regarding the special use case of the
--     <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#migrationhub-PutResourceAttributes-request-ResourceAttributeList ResourceAttributeList>
--     parameter when specifying any \"VM\" related value.
--
-- Because this is an asynchronous call, it will always return 200, whether
-- an association occurs or not. To confirm if an association was found
-- based on the provided details, call @ListDiscoveredResources@.
module Network.AWS.MigrationHub.PutResourceAttributes
  ( -- * Creating a Request
    PutResourceAttributes (..),
    newPutResourceAttributes,

    -- * Request Lenses
    putResourceAttributes_dryRun,
    putResourceAttributes_progressUpdateStream,
    putResourceAttributes_migrationTaskName,
    putResourceAttributes_resourceAttributeList,

    -- * Destructuring the Response
    PutResourceAttributesResponse (..),
    newPutResourceAttributesResponse,

    -- * Response Lenses
    putResourceAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Core.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Core.Text,
    -- | Information about the resource that is being migrated. This data will be
    -- used to map the task to a resource in the Application Discovery Service
    -- repository.
    --
    -- Takes the object array of @ResourceAttribute@ where the @Type@ field is
    -- reserved for the following values:
    -- @IPV4_ADDRESS | IPV6_ADDRESS | MAC_ADDRESS | FQDN | VM_MANAGER_ID | VM_MANAGED_OBJECT_REFERENCE | VM_NAME | VM_PATH | BIOS_ID | MOTHERBOARD_SERIAL_NUMBER@
    -- where the identifying value can be a string up to 256 characters.
    --
    -- -   If any \"VM\" related value is set for a @ResourceAttribute@ object,
    --     it is required that @VM_MANAGER_ID@, as a minimum, is always set. If
    --     @VM_MANAGER_ID@ is not set, then all \"VM\" fields will be discarded
    --     and \"VM\" fields will not be used for matching the migration task
    --     to a server in Application Discovery Service repository. See the
    --     <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example>
    --     section below for a use case of specifying \"VM\" related values.
    --
    -- -   If a server you are trying to match has multiple IP or MAC
    --     addresses, you should provide as many as you know in separate
    --     type\/value pairs passed to the @ResourceAttributeList@ parameter to
    --     maximize the chances of matching.
    resourceAttributeList :: Core.NonEmpty ResourceAttribute
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'putResourceAttributes_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'putResourceAttributes_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'putResourceAttributes_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'resourceAttributeList', 'putResourceAttributes_resourceAttributeList' - Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
--
-- Takes the object array of @ResourceAttribute@ where the @Type@ field is
-- reserved for the following values:
-- @IPV4_ADDRESS | IPV6_ADDRESS | MAC_ADDRESS | FQDN | VM_MANAGER_ID | VM_MANAGED_OBJECT_REFERENCE | VM_NAME | VM_PATH | BIOS_ID | MOTHERBOARD_SERIAL_NUMBER@
-- where the identifying value can be a string up to 256 characters.
--
-- -   If any \"VM\" related value is set for a @ResourceAttribute@ object,
--     it is required that @VM_MANAGER_ID@, as a minimum, is always set. If
--     @VM_MANAGER_ID@ is not set, then all \"VM\" fields will be discarded
--     and \"VM\" fields will not be used for matching the migration task
--     to a server in Application Discovery Service repository. See the
--     <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example>
--     section below for a use case of specifying \"VM\" related values.
--
-- -   If a server you are trying to match has multiple IP or MAC
--     addresses, you should provide as many as you know in separate
--     type\/value pairs passed to the @ResourceAttributeList@ parameter to
--     maximize the chances of matching.
newPutResourceAttributes ::
  -- | 'progressUpdateStream'
  Core.Text ->
  -- | 'migrationTaskName'
  Core.Text ->
  -- | 'resourceAttributeList'
  Core.NonEmpty ResourceAttribute ->
  PutResourceAttributes
newPutResourceAttributes
  pProgressUpdateStream_
  pMigrationTaskName_
  pResourceAttributeList_ =
    PutResourceAttributes'
      { dryRun = Core.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        resourceAttributeList =
          Lens._Coerce Lens.# pResourceAttributeList_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
putResourceAttributes_dryRun :: Lens.Lens' PutResourceAttributes (Core.Maybe Core.Bool)
putResourceAttributes_dryRun = Lens.lens (\PutResourceAttributes' {dryRun} -> dryRun) (\s@PutResourceAttributes' {} a -> s {dryRun = a} :: PutResourceAttributes)

-- | The name of the ProgressUpdateStream.
putResourceAttributes_progressUpdateStream :: Lens.Lens' PutResourceAttributes Core.Text
putResourceAttributes_progressUpdateStream = Lens.lens (\PutResourceAttributes' {progressUpdateStream} -> progressUpdateStream) (\s@PutResourceAttributes' {} a -> s {progressUpdateStream = a} :: PutResourceAttributes)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
putResourceAttributes_migrationTaskName :: Lens.Lens' PutResourceAttributes Core.Text
putResourceAttributes_migrationTaskName = Lens.lens (\PutResourceAttributes' {migrationTaskName} -> migrationTaskName) (\s@PutResourceAttributes' {} a -> s {migrationTaskName = a} :: PutResourceAttributes)

-- | Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
--
-- Takes the object array of @ResourceAttribute@ where the @Type@ field is
-- reserved for the following values:
-- @IPV4_ADDRESS | IPV6_ADDRESS | MAC_ADDRESS | FQDN | VM_MANAGER_ID | VM_MANAGED_OBJECT_REFERENCE | VM_NAME | VM_PATH | BIOS_ID | MOTHERBOARD_SERIAL_NUMBER@
-- where the identifying value can be a string up to 256 characters.
--
-- -   If any \"VM\" related value is set for a @ResourceAttribute@ object,
--     it is required that @VM_MANAGER_ID@, as a minimum, is always set. If
--     @VM_MANAGER_ID@ is not set, then all \"VM\" fields will be discarded
--     and \"VM\" fields will not be used for matching the migration task
--     to a server in Application Discovery Service repository. See the
--     <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example>
--     section below for a use case of specifying \"VM\" related values.
--
-- -   If a server you are trying to match has multiple IP or MAC
--     addresses, you should provide as many as you know in separate
--     type\/value pairs passed to the @ResourceAttributeList@ parameter to
--     maximize the chances of matching.
putResourceAttributes_resourceAttributeList :: Lens.Lens' PutResourceAttributes (Core.NonEmpty ResourceAttribute)
putResourceAttributes_resourceAttributeList = Lens.lens (\PutResourceAttributes' {resourceAttributeList} -> resourceAttributeList) (\s@PutResourceAttributes' {} a -> s {resourceAttributeList = a} :: PutResourceAttributes) Core.. Lens._Coerce

instance Core.AWSRequest PutResourceAttributes where
  type
    AWSResponse PutResourceAttributes =
      PutResourceAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutResourceAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutResourceAttributes

instance Core.NFData PutResourceAttributes

instance Core.ToHeaders PutResourceAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.PutResourceAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutResourceAttributes where
  toJSON PutResourceAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Core.Just
              ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just
              ( "ResourceAttributeList"
                  Core..= resourceAttributeList
              )
          ]
      )

instance Core.ToPath PutResourceAttributes where
  toPath = Core.const "/"

instance Core.ToQuery PutResourceAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutResourceAttributesResponse' smart constructor.
data PutResourceAttributesResponse = PutResourceAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourceAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putResourceAttributesResponse_httpStatus' - The response's http status code.
newPutResourceAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutResourceAttributesResponse
newPutResourceAttributesResponse pHttpStatus_ =
  PutResourceAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putResourceAttributesResponse_httpStatus :: Lens.Lens' PutResourceAttributesResponse Core.Int
putResourceAttributesResponse_httpStatus = Lens.lens (\PutResourceAttributesResponse' {httpStatus} -> httpStatus) (\s@PutResourceAttributesResponse' {} a -> s {httpStatus = a} :: PutResourceAttributesResponse)

instance Core.NFData PutResourceAttributesResponse
