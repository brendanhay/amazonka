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
-- Module      : Amazonka.AccessAnalyzer.CreateAccessPreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access preview that allows you to preview IAM Access Analyzer
-- findings for your resource before deploying resource permissions.
module Amazonka.AccessAnalyzer.CreateAccessPreview
  ( -- * Creating a Request
    CreateAccessPreview (..),
    newCreateAccessPreview,

    -- * Request Lenses
    createAccessPreview_clientToken,
    createAccessPreview_analyzerArn,
    createAccessPreview_configurations,

    -- * Destructuring the Response
    CreateAccessPreviewResponse (..),
    newCreateAccessPreviewResponse,

    -- * Response Lenses
    createAccessPreviewResponse_httpStatus,
    createAccessPreviewResponse_id,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessPreview' smart constructor.
data CreateAccessPreview = CreateAccessPreview'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the account analyzer>
    -- used to generate the access preview. You can only create an access
    -- preview for analyzers with an @Account@ type and @Active@ status.
    analyzerArn :: Prelude.Text,
    -- | Access control configuration for your resource that is used to generate
    -- the access preview. The access preview includes findings for external
    -- access allowed to the resource with the proposed access control
    -- configuration. The configuration must contain exactly one element.
    configurations :: Prelude.HashMap Prelude.Text Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAccessPreview_clientToken' - A client token.
--
-- 'analyzerArn', 'createAccessPreview_analyzerArn' - The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the account analyzer>
-- used to generate the access preview. You can only create an access
-- preview for analyzers with an @Account@ type and @Active@ status.
--
-- 'configurations', 'createAccessPreview_configurations' - Access control configuration for your resource that is used to generate
-- the access preview. The access preview includes findings for external
-- access allowed to the resource with the proposed access control
-- configuration. The configuration must contain exactly one element.
newCreateAccessPreview ::
  -- | 'analyzerArn'
  Prelude.Text ->
  CreateAccessPreview
newCreateAccessPreview pAnalyzerArn_ =
  CreateAccessPreview'
    { clientToken = Prelude.Nothing,
      analyzerArn = pAnalyzerArn_,
      configurations = Prelude.mempty
    }

-- | A client token.
createAccessPreview_clientToken :: Lens.Lens' CreateAccessPreview (Prelude.Maybe Prelude.Text)
createAccessPreview_clientToken = Lens.lens (\CreateAccessPreview' {clientToken} -> clientToken) (\s@CreateAccessPreview' {} a -> s {clientToken = a} :: CreateAccessPreview)

-- | The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the account analyzer>
-- used to generate the access preview. You can only create an access
-- preview for analyzers with an @Account@ type and @Active@ status.
createAccessPreview_analyzerArn :: Lens.Lens' CreateAccessPreview Prelude.Text
createAccessPreview_analyzerArn = Lens.lens (\CreateAccessPreview' {analyzerArn} -> analyzerArn) (\s@CreateAccessPreview' {} a -> s {analyzerArn = a} :: CreateAccessPreview)

-- | Access control configuration for your resource that is used to generate
-- the access preview. The access preview includes findings for external
-- access allowed to the resource with the proposed access control
-- configuration. The configuration must contain exactly one element.
createAccessPreview_configurations :: Lens.Lens' CreateAccessPreview (Prelude.HashMap Prelude.Text Configuration)
createAccessPreview_configurations = Lens.lens (\CreateAccessPreview' {configurations} -> configurations) (\s@CreateAccessPreview' {} a -> s {configurations = a} :: CreateAccessPreview) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAccessPreview where
  type
    AWSResponse CreateAccessPreview =
      CreateAccessPreviewResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessPreviewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
      )

instance Prelude.Hashable CreateAccessPreview where
  hashWithSalt _salt CreateAccessPreview' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` analyzerArn
      `Prelude.hashWithSalt` configurations

instance Prelude.NFData CreateAccessPreview where
  rnf CreateAccessPreview' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf analyzerArn `Prelude.seq`
        Prelude.rnf configurations

instance Data.ToHeaders CreateAccessPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessPreview where
  toJSON CreateAccessPreview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("analyzerArn" Data..= analyzerArn),
            Prelude.Just
              ("configurations" Data..= configurations)
          ]
      )

instance Data.ToPath CreateAccessPreview where
  toPath = Prelude.const "/access-preview"

instance Data.ToQuery CreateAccessPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessPreviewResponse' smart constructor.
data CreateAccessPreviewResponse = CreateAccessPreviewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID for the access preview.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessPreviewResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createAccessPreviewResponse_id' - The unique ID for the access preview.
newCreateAccessPreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  CreateAccessPreviewResponse
newCreateAccessPreviewResponse pHttpStatus_ pId_ =
  CreateAccessPreviewResponse'
    { httpStatus =
        pHttpStatus_,
      id = pId_
    }

-- | The response's http status code.
createAccessPreviewResponse_httpStatus :: Lens.Lens' CreateAccessPreviewResponse Prelude.Int
createAccessPreviewResponse_httpStatus = Lens.lens (\CreateAccessPreviewResponse' {httpStatus} -> httpStatus) (\s@CreateAccessPreviewResponse' {} a -> s {httpStatus = a} :: CreateAccessPreviewResponse)

-- | The unique ID for the access preview.
createAccessPreviewResponse_id :: Lens.Lens' CreateAccessPreviewResponse Prelude.Text
createAccessPreviewResponse_id = Lens.lens (\CreateAccessPreviewResponse' {id} -> id) (\s@CreateAccessPreviewResponse' {} a -> s {id = a} :: CreateAccessPreviewResponse)

instance Prelude.NFData CreateAccessPreviewResponse where
  rnf CreateAccessPreviewResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq` Prelude.rnf id
