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
-- Module      : Amazonka.ResilienceHub.DeleteAppInputSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the input source and all of its imported resources from the
-- Resilience Hub application.
module Amazonka.ResilienceHub.DeleteAppInputSource
  ( -- * Creating a Request
    DeleteAppInputSource (..),
    newDeleteAppInputSource,

    -- * Request Lenses
    deleteAppInputSource_clientToken,
    deleteAppInputSource_eksSourceClusterNamespace,
    deleteAppInputSource_sourceArn,
    deleteAppInputSource_terraformSource,
    deleteAppInputSource_appArn,

    -- * Destructuring the Response
    DeleteAppInputSourceResponse (..),
    newDeleteAppInputSourceResponse,

    -- * Response Lenses
    deleteAppInputSourceResponse_appArn,
    deleteAppInputSourceResponse_appInputSource,
    deleteAppInputSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppInputSource' smart constructor.
data DeleteAppInputSource = DeleteAppInputSource'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The namespace on your Amazon Elastic Kubernetes Service cluster that you
    -- want to delete from the Resilience Hub application.
    eksSourceClusterNamespace :: Prelude.Maybe EksSourceClusterNamespace,
    -- | The Amazon Resource Name (ARN) of the imported resource you want to
    -- remove from the Resilience Hub application. For more information about
    -- ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The imported Terraform s3 state ﬁle you want to remove from the
    -- Resilience Hub application.
    terraformSource :: Prelude.Maybe TerraformSource,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInputSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAppInputSource_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'eksSourceClusterNamespace', 'deleteAppInputSource_eksSourceClusterNamespace' - The namespace on your Amazon Elastic Kubernetes Service cluster that you
-- want to delete from the Resilience Hub application.
--
-- 'sourceArn', 'deleteAppInputSource_sourceArn' - The Amazon Resource Name (ARN) of the imported resource you want to
-- remove from the Resilience Hub application. For more information about
-- ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'terraformSource', 'deleteAppInputSource_terraformSource' - The imported Terraform s3 state ﬁle you want to remove from the
-- Resilience Hub application.
--
-- 'appArn', 'deleteAppInputSource_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newDeleteAppInputSource ::
  -- | 'appArn'
  Prelude.Text ->
  DeleteAppInputSource
newDeleteAppInputSource pAppArn_ =
  DeleteAppInputSource'
    { clientToken =
        Prelude.Nothing,
      eksSourceClusterNamespace = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      terraformSource = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteAppInputSource_clientToken :: Lens.Lens' DeleteAppInputSource (Prelude.Maybe Prelude.Text)
deleteAppInputSource_clientToken = Lens.lens (\DeleteAppInputSource' {clientToken} -> clientToken) (\s@DeleteAppInputSource' {} a -> s {clientToken = a} :: DeleteAppInputSource)

-- | The namespace on your Amazon Elastic Kubernetes Service cluster that you
-- want to delete from the Resilience Hub application.
deleteAppInputSource_eksSourceClusterNamespace :: Lens.Lens' DeleteAppInputSource (Prelude.Maybe EksSourceClusterNamespace)
deleteAppInputSource_eksSourceClusterNamespace = Lens.lens (\DeleteAppInputSource' {eksSourceClusterNamespace} -> eksSourceClusterNamespace) (\s@DeleteAppInputSource' {} a -> s {eksSourceClusterNamespace = a} :: DeleteAppInputSource)

-- | The Amazon Resource Name (ARN) of the imported resource you want to
-- remove from the Resilience Hub application. For more information about
-- ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppInputSource_sourceArn :: Lens.Lens' DeleteAppInputSource (Prelude.Maybe Prelude.Text)
deleteAppInputSource_sourceArn = Lens.lens (\DeleteAppInputSource' {sourceArn} -> sourceArn) (\s@DeleteAppInputSource' {} a -> s {sourceArn = a} :: DeleteAppInputSource)

-- | The imported Terraform s3 state ﬁle you want to remove from the
-- Resilience Hub application.
deleteAppInputSource_terraformSource :: Lens.Lens' DeleteAppInputSource (Prelude.Maybe TerraformSource)
deleteAppInputSource_terraformSource = Lens.lens (\DeleteAppInputSource' {terraformSource} -> terraformSource) (\s@DeleteAppInputSource' {} a -> s {terraformSource = a} :: DeleteAppInputSource)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppInputSource_appArn :: Lens.Lens' DeleteAppInputSource Prelude.Text
deleteAppInputSource_appArn = Lens.lens (\DeleteAppInputSource' {appArn} -> appArn) (\s@DeleteAppInputSource' {} a -> s {appArn = a} :: DeleteAppInputSource)

instance Core.AWSRequest DeleteAppInputSource where
  type
    AWSResponse DeleteAppInputSource =
      DeleteAppInputSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAppInputSourceResponse'
            Prelude.<$> (x Data..?> "appArn")
            Prelude.<*> (x Data..?> "appInputSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAppInputSource where
  hashWithSalt _salt DeleteAppInputSource' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` eksSourceClusterNamespace
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` terraformSource
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData DeleteAppInputSource where
  rnf DeleteAppInputSource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf eksSourceClusterNamespace
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf terraformSource
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders DeleteAppInputSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppInputSource where
  toJSON DeleteAppInputSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("eksSourceClusterNamespace" Data..=)
              Prelude.<$> eksSourceClusterNamespace,
            ("sourceArn" Data..=) Prelude.<$> sourceArn,
            ("terraformSource" Data..=)
              Prelude.<$> terraformSource,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath DeleteAppInputSource where
  toPath = Prelude.const "/delete-app-input-source"

instance Data.ToQuery DeleteAppInputSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppInputSourceResponse' smart constructor.
data DeleteAppInputSourceResponse = DeleteAppInputSourceResponse'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the input source from where the application resource is
    -- imported from.
    appInputSource :: Prelude.Maybe AppInputSource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInputSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'deleteAppInputSourceResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appInputSource', 'deleteAppInputSourceResponse_appInputSource' - The name of the input source from where the application resource is
-- imported from.
--
-- 'httpStatus', 'deleteAppInputSourceResponse_httpStatus' - The response's http status code.
newDeleteAppInputSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppInputSourceResponse
newDeleteAppInputSourceResponse pHttpStatus_ =
  DeleteAppInputSourceResponse'
    { appArn =
        Prelude.Nothing,
      appInputSource = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppInputSourceResponse_appArn :: Lens.Lens' DeleteAppInputSourceResponse (Prelude.Maybe Prelude.Text)
deleteAppInputSourceResponse_appArn = Lens.lens (\DeleteAppInputSourceResponse' {appArn} -> appArn) (\s@DeleteAppInputSourceResponse' {} a -> s {appArn = a} :: DeleteAppInputSourceResponse)

-- | The name of the input source from where the application resource is
-- imported from.
deleteAppInputSourceResponse_appInputSource :: Lens.Lens' DeleteAppInputSourceResponse (Prelude.Maybe AppInputSource)
deleteAppInputSourceResponse_appInputSource = Lens.lens (\DeleteAppInputSourceResponse' {appInputSource} -> appInputSource) (\s@DeleteAppInputSourceResponse' {} a -> s {appInputSource = a} :: DeleteAppInputSourceResponse)

-- | The response's http status code.
deleteAppInputSourceResponse_httpStatus :: Lens.Lens' DeleteAppInputSourceResponse Prelude.Int
deleteAppInputSourceResponse_httpStatus = Lens.lens (\DeleteAppInputSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteAppInputSourceResponse' {} a -> s {httpStatus = a} :: DeleteAppInputSourceResponse)

instance Prelude.NFData DeleteAppInputSourceResponse where
  rnf DeleteAppInputSourceResponse' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appInputSource
      `Prelude.seq` Prelude.rnf httpStatus
