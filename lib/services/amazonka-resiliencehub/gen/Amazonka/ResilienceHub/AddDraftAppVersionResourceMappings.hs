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
-- Module      : Amazonka.ResilienceHub.AddDraftAppVersionResourceMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the resource mapping for the draft application version.
module Amazonka.ResilienceHub.AddDraftAppVersionResourceMappings
  ( -- * Creating a Request
    AddDraftAppVersionResourceMappings (..),
    newAddDraftAppVersionResourceMappings,

    -- * Request Lenses
    addDraftAppVersionResourceMappings_appArn,
    addDraftAppVersionResourceMappings_resourceMappings,

    -- * Destructuring the Response
    AddDraftAppVersionResourceMappingsResponse (..),
    newAddDraftAppVersionResourceMappingsResponse,

    -- * Response Lenses
    addDraftAppVersionResourceMappingsResponse_httpStatus,
    addDraftAppVersionResourceMappingsResponse_appArn,
    addDraftAppVersionResourceMappingsResponse_appVersion,
    addDraftAppVersionResourceMappingsResponse_resourceMappings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddDraftAppVersionResourceMappings' smart constructor.
data AddDraftAppVersionResourceMappings = AddDraftAppVersionResourceMappings'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | Mappings used to map logical resources from the template to physical
    -- resources. You can use the mapping type @CFN_STACK@ if the application
    -- template uses a logical stack name. Or you can map individual resources
    -- by using the mapping type @RESOURCE@. We recommend using the mapping
    -- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
    resourceMappings :: [ResourceMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddDraftAppVersionResourceMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'addDraftAppVersionResourceMappings_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'resourceMappings', 'addDraftAppVersionResourceMappings_resourceMappings' - Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
newAddDraftAppVersionResourceMappings ::
  -- | 'appArn'
  Prelude.Text ->
  AddDraftAppVersionResourceMappings
newAddDraftAppVersionResourceMappings pAppArn_ =
  AddDraftAppVersionResourceMappings'
    { appArn =
        pAppArn_,
      resourceMappings = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
addDraftAppVersionResourceMappings_appArn :: Lens.Lens' AddDraftAppVersionResourceMappings Prelude.Text
addDraftAppVersionResourceMappings_appArn = Lens.lens (\AddDraftAppVersionResourceMappings' {appArn} -> appArn) (\s@AddDraftAppVersionResourceMappings' {} a -> s {appArn = a} :: AddDraftAppVersionResourceMappings)

-- | Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
addDraftAppVersionResourceMappings_resourceMappings :: Lens.Lens' AddDraftAppVersionResourceMappings [ResourceMapping]
addDraftAppVersionResourceMappings_resourceMappings = Lens.lens (\AddDraftAppVersionResourceMappings' {resourceMappings} -> resourceMappings) (\s@AddDraftAppVersionResourceMappings' {} a -> s {resourceMappings = a} :: AddDraftAppVersionResourceMappings) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AddDraftAppVersionResourceMappings
  where
  type
    AWSResponse AddDraftAppVersionResourceMappings =
      AddDraftAppVersionResourceMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddDraftAppVersionResourceMappingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
            Prelude.<*> ( x
                            Data..?> "resourceMappings"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AddDraftAppVersionResourceMappings
  where
  hashWithSalt
    _salt
    AddDraftAppVersionResourceMappings' {..} =
      _salt
        `Prelude.hashWithSalt` appArn
        `Prelude.hashWithSalt` resourceMappings

instance
  Prelude.NFData
    AddDraftAppVersionResourceMappings
  where
  rnf AddDraftAppVersionResourceMappings' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf resourceMappings

instance
  Data.ToHeaders
    AddDraftAppVersionResourceMappings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    AddDraftAppVersionResourceMappings
  where
  toJSON AddDraftAppVersionResourceMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just
              ("resourceMappings" Data..= resourceMappings)
          ]
      )

instance
  Data.ToPath
    AddDraftAppVersionResourceMappings
  where
  toPath =
    Prelude.const
      "/add-draft-app-version-resource-mappings"

instance
  Data.ToQuery
    AddDraftAppVersionResourceMappings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddDraftAppVersionResourceMappingsResponse' smart constructor.
data AddDraftAppVersionResourceMappingsResponse = AddDraftAppVersionResourceMappingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text,
    -- | Mappings used to map logical resources from the template to physical
    -- resources. You can use the mapping type @CFN_STACK@ if the application
    -- template uses a logical stack name. Or you can map individual resources
    -- by using the mapping type @RESOURCE@. We recommend using the mapping
    -- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
    resourceMappings :: [ResourceMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddDraftAppVersionResourceMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addDraftAppVersionResourceMappingsResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'addDraftAppVersionResourceMappingsResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'addDraftAppVersionResourceMappingsResponse_appVersion' - The version of the application.
--
-- 'resourceMappings', 'addDraftAppVersionResourceMappingsResponse_resourceMappings' - Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
newAddDraftAppVersionResourceMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  AddDraftAppVersionResourceMappingsResponse
newAddDraftAppVersionResourceMappingsResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    AddDraftAppVersionResourceMappingsResponse'
      { httpStatus =
          pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_,
        resourceMappings =
          Prelude.mempty
      }

-- | The response's http status code.
addDraftAppVersionResourceMappingsResponse_httpStatus :: Lens.Lens' AddDraftAppVersionResourceMappingsResponse Prelude.Int
addDraftAppVersionResourceMappingsResponse_httpStatus = Lens.lens (\AddDraftAppVersionResourceMappingsResponse' {httpStatus} -> httpStatus) (\s@AddDraftAppVersionResourceMappingsResponse' {} a -> s {httpStatus = a} :: AddDraftAppVersionResourceMappingsResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
addDraftAppVersionResourceMappingsResponse_appArn :: Lens.Lens' AddDraftAppVersionResourceMappingsResponse Prelude.Text
addDraftAppVersionResourceMappingsResponse_appArn = Lens.lens (\AddDraftAppVersionResourceMappingsResponse' {appArn} -> appArn) (\s@AddDraftAppVersionResourceMappingsResponse' {} a -> s {appArn = a} :: AddDraftAppVersionResourceMappingsResponse)

-- | The version of the application.
addDraftAppVersionResourceMappingsResponse_appVersion :: Lens.Lens' AddDraftAppVersionResourceMappingsResponse Prelude.Text
addDraftAppVersionResourceMappingsResponse_appVersion = Lens.lens (\AddDraftAppVersionResourceMappingsResponse' {appVersion} -> appVersion) (\s@AddDraftAppVersionResourceMappingsResponse' {} a -> s {appVersion = a} :: AddDraftAppVersionResourceMappingsResponse)

-- | Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
addDraftAppVersionResourceMappingsResponse_resourceMappings :: Lens.Lens' AddDraftAppVersionResourceMappingsResponse [ResourceMapping]
addDraftAppVersionResourceMappingsResponse_resourceMappings = Lens.lens (\AddDraftAppVersionResourceMappingsResponse' {resourceMappings} -> resourceMappings) (\s@AddDraftAppVersionResourceMappingsResponse' {} a -> s {resourceMappings = a} :: AddDraftAppVersionResourceMappingsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    AddDraftAppVersionResourceMappingsResponse
  where
  rnf AddDraftAppVersionResourceMappingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf resourceMappings
