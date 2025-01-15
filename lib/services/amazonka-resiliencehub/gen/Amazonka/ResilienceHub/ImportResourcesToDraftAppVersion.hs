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
-- Module      : Amazonka.ResilienceHub.ImportResourcesToDraftAppVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports resources from sources such as a CloudFormation stack,
-- resource-groups, or application registry app to a draft application
-- version.
module Amazonka.ResilienceHub.ImportResourcesToDraftAppVersion
  ( -- * Creating a Request
    ImportResourcesToDraftAppVersion (..),
    newImportResourcesToDraftAppVersion,

    -- * Request Lenses
    importResourcesToDraftAppVersion_sourceArns,
    importResourcesToDraftAppVersion_terraformSources,
    importResourcesToDraftAppVersion_appArn,

    -- * Destructuring the Response
    ImportResourcesToDraftAppVersionResponse (..),
    newImportResourcesToDraftAppVersionResponse,

    -- * Response Lenses
    importResourcesToDraftAppVersionResponse_sourceArns,
    importResourcesToDraftAppVersionResponse_terraformSources,
    importResourcesToDraftAppVersionResponse_httpStatus,
    importResourcesToDraftAppVersionResponse_appArn,
    importResourcesToDraftAppVersionResponse_appVersion,
    importResourcesToDraftAppVersionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportResourcesToDraftAppVersion' smart constructor.
data ImportResourcesToDraftAppVersion = ImportResourcesToDraftAppVersion'
  { -- | The Amazon Resource Names (ARNs) for the resources that you want to
    -- import.
    sourceArns :: Prelude.Maybe [Prelude.Text],
    -- | A list of terraform file s3 URLs you need to import.
    terraformSources :: Prelude.Maybe [TerraformSource],
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportResourcesToDraftAppVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceArns', 'importResourcesToDraftAppVersion_sourceArns' - The Amazon Resource Names (ARNs) for the resources that you want to
-- import.
--
-- 'terraformSources', 'importResourcesToDraftAppVersion_terraformSources' - A list of terraform file s3 URLs you need to import.
--
-- 'appArn', 'importResourcesToDraftAppVersion_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newImportResourcesToDraftAppVersion ::
  -- | 'appArn'
  Prelude.Text ->
  ImportResourcesToDraftAppVersion
newImportResourcesToDraftAppVersion pAppArn_ =
  ImportResourcesToDraftAppVersion'
    { sourceArns =
        Prelude.Nothing,
      terraformSources = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | The Amazon Resource Names (ARNs) for the resources that you want to
-- import.
importResourcesToDraftAppVersion_sourceArns :: Lens.Lens' ImportResourcesToDraftAppVersion (Prelude.Maybe [Prelude.Text])
importResourcesToDraftAppVersion_sourceArns = Lens.lens (\ImportResourcesToDraftAppVersion' {sourceArns} -> sourceArns) (\s@ImportResourcesToDraftAppVersion' {} a -> s {sourceArns = a} :: ImportResourcesToDraftAppVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of terraform file s3 URLs you need to import.
importResourcesToDraftAppVersion_terraformSources :: Lens.Lens' ImportResourcesToDraftAppVersion (Prelude.Maybe [TerraformSource])
importResourcesToDraftAppVersion_terraformSources = Lens.lens (\ImportResourcesToDraftAppVersion' {terraformSources} -> terraformSources) (\s@ImportResourcesToDraftAppVersion' {} a -> s {terraformSources = a} :: ImportResourcesToDraftAppVersion) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
importResourcesToDraftAppVersion_appArn :: Lens.Lens' ImportResourcesToDraftAppVersion Prelude.Text
importResourcesToDraftAppVersion_appArn = Lens.lens (\ImportResourcesToDraftAppVersion' {appArn} -> appArn) (\s@ImportResourcesToDraftAppVersion' {} a -> s {appArn = a} :: ImportResourcesToDraftAppVersion)

instance
  Core.AWSRequest
    ImportResourcesToDraftAppVersion
  where
  type
    AWSResponse ImportResourcesToDraftAppVersion =
      ImportResourcesToDraftAppVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportResourcesToDraftAppVersionResponse'
            Prelude.<$> (x Data..?> "sourceArns" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "terraformSources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
            Prelude.<*> (x Data..:> "status")
      )

instance
  Prelude.Hashable
    ImportResourcesToDraftAppVersion
  where
  hashWithSalt
    _salt
    ImportResourcesToDraftAppVersion' {..} =
      _salt
        `Prelude.hashWithSalt` sourceArns
        `Prelude.hashWithSalt` terraformSources
        `Prelude.hashWithSalt` appArn

instance
  Prelude.NFData
    ImportResourcesToDraftAppVersion
  where
  rnf ImportResourcesToDraftAppVersion' {..} =
    Prelude.rnf sourceArns `Prelude.seq`
      Prelude.rnf terraformSources `Prelude.seq`
        Prelude.rnf appArn

instance
  Data.ToHeaders
    ImportResourcesToDraftAppVersion
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

instance Data.ToJSON ImportResourcesToDraftAppVersion where
  toJSON ImportResourcesToDraftAppVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceArns" Data..=) Prelude.<$> sourceArns,
            ("terraformSources" Data..=)
              Prelude.<$> terraformSources,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath ImportResourcesToDraftAppVersion where
  toPath =
    Prelude.const
      "/import-resources-to-draft-app-version"

instance
  Data.ToQuery
    ImportResourcesToDraftAppVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportResourcesToDraftAppVersionResponse' smart constructor.
data ImportResourcesToDraftAppVersionResponse = ImportResourcesToDraftAppVersionResponse'
  { -- | The Amazon Resource Names (ARNs) for the resources that you imported.
    sourceArns :: Prelude.Maybe [Prelude.Text],
    -- | A list of terraform file s3 URLs you need to import.
    terraformSources :: Prelude.Maybe [TerraformSource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text,
    -- | The status of the action.
    status :: ResourceImportStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportResourcesToDraftAppVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceArns', 'importResourcesToDraftAppVersionResponse_sourceArns' - The Amazon Resource Names (ARNs) for the resources that you imported.
--
-- 'terraformSources', 'importResourcesToDraftAppVersionResponse_terraformSources' - A list of terraform file s3 URLs you need to import.
--
-- 'httpStatus', 'importResourcesToDraftAppVersionResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'importResourcesToDraftAppVersionResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'importResourcesToDraftAppVersionResponse_appVersion' - The version of the application.
--
-- 'status', 'importResourcesToDraftAppVersionResponse_status' - The status of the action.
newImportResourcesToDraftAppVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'status'
  ResourceImportStatusType ->
  ImportResourcesToDraftAppVersionResponse
newImportResourcesToDraftAppVersionResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_
  pStatus_ =
    ImportResourcesToDraftAppVersionResponse'
      { sourceArns =
          Prelude.Nothing,
        terraformSources =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_,
        status = pStatus_
      }

-- | The Amazon Resource Names (ARNs) for the resources that you imported.
importResourcesToDraftAppVersionResponse_sourceArns :: Lens.Lens' ImportResourcesToDraftAppVersionResponse (Prelude.Maybe [Prelude.Text])
importResourcesToDraftAppVersionResponse_sourceArns = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {sourceArns} -> sourceArns) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {sourceArns = a} :: ImportResourcesToDraftAppVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of terraform file s3 URLs you need to import.
importResourcesToDraftAppVersionResponse_terraformSources :: Lens.Lens' ImportResourcesToDraftAppVersionResponse (Prelude.Maybe [TerraformSource])
importResourcesToDraftAppVersionResponse_terraformSources = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {terraformSources} -> terraformSources) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {terraformSources = a} :: ImportResourcesToDraftAppVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
importResourcesToDraftAppVersionResponse_httpStatus :: Lens.Lens' ImportResourcesToDraftAppVersionResponse Prelude.Int
importResourcesToDraftAppVersionResponse_httpStatus = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {httpStatus} -> httpStatus) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {httpStatus = a} :: ImportResourcesToDraftAppVersionResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
importResourcesToDraftAppVersionResponse_appArn :: Lens.Lens' ImportResourcesToDraftAppVersionResponse Prelude.Text
importResourcesToDraftAppVersionResponse_appArn = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {appArn} -> appArn) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {appArn = a} :: ImportResourcesToDraftAppVersionResponse)

-- | The version of the application.
importResourcesToDraftAppVersionResponse_appVersion :: Lens.Lens' ImportResourcesToDraftAppVersionResponse Prelude.Text
importResourcesToDraftAppVersionResponse_appVersion = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {appVersion} -> appVersion) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {appVersion = a} :: ImportResourcesToDraftAppVersionResponse)

-- | The status of the action.
importResourcesToDraftAppVersionResponse_status :: Lens.Lens' ImportResourcesToDraftAppVersionResponse ResourceImportStatusType
importResourcesToDraftAppVersionResponse_status = Lens.lens (\ImportResourcesToDraftAppVersionResponse' {status} -> status) (\s@ImportResourcesToDraftAppVersionResponse' {} a -> s {status = a} :: ImportResourcesToDraftAppVersionResponse)

instance
  Prelude.NFData
    ImportResourcesToDraftAppVersionResponse
  where
  rnf ImportResourcesToDraftAppVersionResponse' {..} =
    Prelude.rnf sourceArns `Prelude.seq`
      Prelude.rnf terraformSources `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf appArn `Prelude.seq`
            Prelude.rnf appVersion `Prelude.seq`
              Prelude.rnf status
