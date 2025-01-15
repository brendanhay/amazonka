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
-- Module      : Amazonka.QuickSight.CreateTemplateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template alias for a template.
module Amazonka.QuickSight.CreateTemplateAlias
  ( -- * Creating a Request
    CreateTemplateAlias (..),
    newCreateTemplateAlias,

    -- * Request Lenses
    createTemplateAlias_awsAccountId,
    createTemplateAlias_templateId,
    createTemplateAlias_aliasName,
    createTemplateAlias_templateVersionNumber,

    -- * Destructuring the Response
    CreateTemplateAliasResponse (..),
    newCreateTemplateAliasResponse,

    -- * Response Lenses
    createTemplateAliasResponse_requestId,
    createTemplateAliasResponse_templateAlias,
    createTemplateAliasResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTemplateAlias' smart constructor.
data CreateTemplateAlias = CreateTemplateAlias'
  { -- | The ID of the Amazon Web Services account that contains the template
    -- that you creating an alias for.
    awsAccountId :: Prelude.Text,
    -- | An ID for the template.
    templateId :: Prelude.Text,
    -- | The name that you want to give to the template alias that you\'re
    -- creating. Don\'t start the alias name with the @$@ character. Alias
    -- names that start with @$@ are reserved by Amazon QuickSight.
    aliasName :: Prelude.Text,
    -- | The version number of the template.
    templateVersionNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createTemplateAlias_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- that you creating an alias for.
--
-- 'templateId', 'createTemplateAlias_templateId' - An ID for the template.
--
-- 'aliasName', 'createTemplateAlias_aliasName' - The name that you want to give to the template alias that you\'re
-- creating. Don\'t start the alias name with the @$@ character. Alias
-- names that start with @$@ are reserved by Amazon QuickSight.
--
-- 'templateVersionNumber', 'createTemplateAlias_templateVersionNumber' - The version number of the template.
newCreateTemplateAlias ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'aliasName'
  Prelude.Text ->
  -- | 'templateVersionNumber'
  Prelude.Natural ->
  CreateTemplateAlias
newCreateTemplateAlias
  pAwsAccountId_
  pTemplateId_
  pAliasName_
  pTemplateVersionNumber_ =
    CreateTemplateAlias'
      { awsAccountId = pAwsAccountId_,
        templateId = pTemplateId_,
        aliasName = pAliasName_,
        templateVersionNumber = pTemplateVersionNumber_
      }

-- | The ID of the Amazon Web Services account that contains the template
-- that you creating an alias for.
createTemplateAlias_awsAccountId :: Lens.Lens' CreateTemplateAlias Prelude.Text
createTemplateAlias_awsAccountId = Lens.lens (\CreateTemplateAlias' {awsAccountId} -> awsAccountId) (\s@CreateTemplateAlias' {} a -> s {awsAccountId = a} :: CreateTemplateAlias)

-- | An ID for the template.
createTemplateAlias_templateId :: Lens.Lens' CreateTemplateAlias Prelude.Text
createTemplateAlias_templateId = Lens.lens (\CreateTemplateAlias' {templateId} -> templateId) (\s@CreateTemplateAlias' {} a -> s {templateId = a} :: CreateTemplateAlias)

-- | The name that you want to give to the template alias that you\'re
-- creating. Don\'t start the alias name with the @$@ character. Alias
-- names that start with @$@ are reserved by Amazon QuickSight.
createTemplateAlias_aliasName :: Lens.Lens' CreateTemplateAlias Prelude.Text
createTemplateAlias_aliasName = Lens.lens (\CreateTemplateAlias' {aliasName} -> aliasName) (\s@CreateTemplateAlias' {} a -> s {aliasName = a} :: CreateTemplateAlias)

-- | The version number of the template.
createTemplateAlias_templateVersionNumber :: Lens.Lens' CreateTemplateAlias Prelude.Natural
createTemplateAlias_templateVersionNumber = Lens.lens (\CreateTemplateAlias' {templateVersionNumber} -> templateVersionNumber) (\s@CreateTemplateAlias' {} a -> s {templateVersionNumber = a} :: CreateTemplateAlias)

instance Core.AWSRequest CreateTemplateAlias where
  type
    AWSResponse CreateTemplateAlias =
      CreateTemplateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTemplateAliasResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateAlias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTemplateAlias where
  hashWithSalt _salt CreateTemplateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` templateVersionNumber

instance Prelude.NFData CreateTemplateAlias where
  rnf CreateTemplateAlias' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf templateId `Prelude.seq`
        Prelude.rnf aliasName `Prelude.seq`
          Prelude.rnf templateVersionNumber

instance Data.ToHeaders CreateTemplateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTemplateAlias where
  toJSON CreateTemplateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TemplateVersionNumber"
                  Data..= templateVersionNumber
              )
          ]
      )

instance Data.ToPath CreateTemplateAlias where
  toPath CreateTemplateAlias' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/aliases/",
        Data.toBS aliasName
      ]

instance Data.ToQuery CreateTemplateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTemplateAliasResponse' smart constructor.
data CreateTemplateAliasResponse = CreateTemplateAliasResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the template alias.
    templateAlias :: Prelude.Maybe TemplateAlias,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createTemplateAliasResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateAlias', 'createTemplateAliasResponse_templateAlias' - Information about the template alias.
--
-- 'status', 'createTemplateAliasResponse_status' - The HTTP status of the request.
newCreateTemplateAliasResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateTemplateAliasResponse
newCreateTemplateAliasResponse pStatus_ =
  CreateTemplateAliasResponse'
    { requestId =
        Prelude.Nothing,
      templateAlias = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
createTemplateAliasResponse_requestId :: Lens.Lens' CreateTemplateAliasResponse (Prelude.Maybe Prelude.Text)
createTemplateAliasResponse_requestId = Lens.lens (\CreateTemplateAliasResponse' {requestId} -> requestId) (\s@CreateTemplateAliasResponse' {} a -> s {requestId = a} :: CreateTemplateAliasResponse)

-- | Information about the template alias.
createTemplateAliasResponse_templateAlias :: Lens.Lens' CreateTemplateAliasResponse (Prelude.Maybe TemplateAlias)
createTemplateAliasResponse_templateAlias = Lens.lens (\CreateTemplateAliasResponse' {templateAlias} -> templateAlias) (\s@CreateTemplateAliasResponse' {} a -> s {templateAlias = a} :: CreateTemplateAliasResponse)

-- | The HTTP status of the request.
createTemplateAliasResponse_status :: Lens.Lens' CreateTemplateAliasResponse Prelude.Int
createTemplateAliasResponse_status = Lens.lens (\CreateTemplateAliasResponse' {status} -> status) (\s@CreateTemplateAliasResponse' {} a -> s {status = a} :: CreateTemplateAliasResponse)

instance Prelude.NFData CreateTemplateAliasResponse where
  rnf CreateTemplateAliasResponse' {..} =
    Prelude.rnf requestId `Prelude.seq`
      Prelude.rnf templateAlias `Prelude.seq`
        Prelude.rnf status
