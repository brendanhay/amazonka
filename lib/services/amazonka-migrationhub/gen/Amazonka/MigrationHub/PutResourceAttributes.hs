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
-- Module      : Amazonka.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MigrationHub.PutResourceAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Text,
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
    resourceAttributeList :: Prelude.NonEmpty ResourceAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'resourceAttributeList'
  Prelude.NonEmpty ResourceAttribute ->
  PutResourceAttributes
newPutResourceAttributes
  pProgressUpdateStream_
  pMigrationTaskName_
  pResourceAttributeList_ =
    PutResourceAttributes'
      { dryRun = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        resourceAttributeList =
          Lens.coerced Lens.# pResourceAttributeList_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
putResourceAttributes_dryRun :: Lens.Lens' PutResourceAttributes (Prelude.Maybe Prelude.Bool)
putResourceAttributes_dryRun = Lens.lens (\PutResourceAttributes' {dryRun} -> dryRun) (\s@PutResourceAttributes' {} a -> s {dryRun = a} :: PutResourceAttributes)

-- | The name of the ProgressUpdateStream.
putResourceAttributes_progressUpdateStream :: Lens.Lens' PutResourceAttributes Prelude.Text
putResourceAttributes_progressUpdateStream = Lens.lens (\PutResourceAttributes' {progressUpdateStream} -> progressUpdateStream) (\s@PutResourceAttributes' {} a -> s {progressUpdateStream = a} :: PutResourceAttributes)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
putResourceAttributes_migrationTaskName :: Lens.Lens' PutResourceAttributes Prelude.Text
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
putResourceAttributes_resourceAttributeList :: Lens.Lens' PutResourceAttributes (Prelude.NonEmpty ResourceAttribute)
putResourceAttributes_resourceAttributeList = Lens.lens (\PutResourceAttributes' {resourceAttributeList} -> resourceAttributeList) (\s@PutResourceAttributes' {} a -> s {resourceAttributeList = a} :: PutResourceAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest PutResourceAttributes where
  type
    AWSResponse PutResourceAttributes =
      PutResourceAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutResourceAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourceAttributes where
  hashWithSalt _salt PutResourceAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` resourceAttributeList

instance Prelude.NFData PutResourceAttributes where
  rnf PutResourceAttributes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf resourceAttributeList

instance Data.ToHeaders PutResourceAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.PutResourceAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourceAttributes where
  toJSON PutResourceAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Data..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Data..= migrationTaskName),
            Prelude.Just
              ( "ResourceAttributeList"
                  Data..= resourceAttributeList
              )
          ]
      )

instance Data.ToPath PutResourceAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourceAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourceAttributesResponse' smart constructor.
data PutResourceAttributesResponse = PutResourceAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutResourceAttributesResponse
newPutResourceAttributesResponse pHttpStatus_ =
  PutResourceAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putResourceAttributesResponse_httpStatus :: Lens.Lens' PutResourceAttributesResponse Prelude.Int
putResourceAttributesResponse_httpStatus = Lens.lens (\PutResourceAttributesResponse' {httpStatus} -> httpStatus) (\s@PutResourceAttributesResponse' {} a -> s {httpStatus = a} :: PutResourceAttributesResponse)

instance Prelude.NFData PutResourceAttributesResponse where
  rnf PutResourceAttributesResponse' {..} =
    Prelude.rnf httpStatus
