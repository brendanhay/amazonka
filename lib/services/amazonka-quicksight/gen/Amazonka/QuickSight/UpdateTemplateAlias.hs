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
-- Module      : Amazonka.QuickSight.UpdateTemplateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the template alias of a template.
module Amazonka.QuickSight.UpdateTemplateAlias
  ( -- * Creating a Request
    UpdateTemplateAlias (..),
    newUpdateTemplateAlias,

    -- * Request Lenses
    updateTemplateAlias_awsAccountId,
    updateTemplateAlias_templateId,
    updateTemplateAlias_aliasName,
    updateTemplateAlias_templateVersionNumber,

    -- * Destructuring the Response
    UpdateTemplateAliasResponse (..),
    newUpdateTemplateAliasResponse,

    -- * Response Lenses
    updateTemplateAliasResponse_requestId,
    updateTemplateAliasResponse_templateAlias,
    updateTemplateAliasResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTemplateAlias' smart constructor.
data UpdateTemplateAlias = UpdateTemplateAlias'
  { -- | The ID of the Amazon Web Services account that contains the template
    -- alias that you\'re updating.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text,
    -- | The alias of the template that you want to update. If you name a
    -- specific alias, you update the version that the alias points to. You can
    -- specify the latest version of the template by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
    -- doesn\'t apply to templates.
    aliasName :: Prelude.Text,
    -- | The version number of the template.
    templateVersionNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'updateTemplateAlias_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- alias that you\'re updating.
--
-- 'templateId', 'updateTemplateAlias_templateId' - The ID for the template.
--
-- 'aliasName', 'updateTemplateAlias_aliasName' - The alias of the template that you want to update. If you name a
-- specific alias, you update the version that the alias points to. You can
-- specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
--
-- 'templateVersionNumber', 'updateTemplateAlias_templateVersionNumber' - The version number of the template.
newUpdateTemplateAlias ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'aliasName'
  Prelude.Text ->
  -- | 'templateVersionNumber'
  Prelude.Natural ->
  UpdateTemplateAlias
newUpdateTemplateAlias
  pAwsAccountId_
  pTemplateId_
  pAliasName_
  pTemplateVersionNumber_ =
    UpdateTemplateAlias'
      { awsAccountId = pAwsAccountId_,
        templateId = pTemplateId_,
        aliasName = pAliasName_,
        templateVersionNumber = pTemplateVersionNumber_
      }

-- | The ID of the Amazon Web Services account that contains the template
-- alias that you\'re updating.
updateTemplateAlias_awsAccountId :: Lens.Lens' UpdateTemplateAlias Prelude.Text
updateTemplateAlias_awsAccountId = Lens.lens (\UpdateTemplateAlias' {awsAccountId} -> awsAccountId) (\s@UpdateTemplateAlias' {} a -> s {awsAccountId = a} :: UpdateTemplateAlias)

-- | The ID for the template.
updateTemplateAlias_templateId :: Lens.Lens' UpdateTemplateAlias Prelude.Text
updateTemplateAlias_templateId = Lens.lens (\UpdateTemplateAlias' {templateId} -> templateId) (\s@UpdateTemplateAlias' {} a -> s {templateId = a} :: UpdateTemplateAlias)

-- | The alias of the template that you want to update. If you name a
-- specific alias, you update the version that the alias points to. You can
-- specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
updateTemplateAlias_aliasName :: Lens.Lens' UpdateTemplateAlias Prelude.Text
updateTemplateAlias_aliasName = Lens.lens (\UpdateTemplateAlias' {aliasName} -> aliasName) (\s@UpdateTemplateAlias' {} a -> s {aliasName = a} :: UpdateTemplateAlias)

-- | The version number of the template.
updateTemplateAlias_templateVersionNumber :: Lens.Lens' UpdateTemplateAlias Prelude.Natural
updateTemplateAlias_templateVersionNumber = Lens.lens (\UpdateTemplateAlias' {templateVersionNumber} -> templateVersionNumber) (\s@UpdateTemplateAlias' {} a -> s {templateVersionNumber = a} :: UpdateTemplateAlias)

instance Core.AWSRequest UpdateTemplateAlias where
  type
    AWSResponse UpdateTemplateAlias =
      UpdateTemplateAliasResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplateAliasResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateAlias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTemplateAlias where
  hashWithSalt _salt UpdateTemplateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` templateVersionNumber

instance Prelude.NFData UpdateTemplateAlias where
  rnf UpdateTemplateAlias' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf templateVersionNumber

instance Data.ToHeaders UpdateTemplateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTemplateAlias where
  toJSON UpdateTemplateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TemplateVersionNumber"
                  Data..= templateVersionNumber
              )
          ]
      )

instance Data.ToPath UpdateTemplateAlias where
  toPath UpdateTemplateAlias' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/aliases/",
        Data.toBS aliasName
      ]

instance Data.ToQuery UpdateTemplateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTemplateAliasResponse' smart constructor.
data UpdateTemplateAliasResponse = UpdateTemplateAliasResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The template alias.
    templateAlias :: Prelude.Maybe TemplateAlias,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateTemplateAliasResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateAlias', 'updateTemplateAliasResponse_templateAlias' - The template alias.
--
-- 'status', 'updateTemplateAliasResponse_status' - The HTTP status of the request.
newUpdateTemplateAliasResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTemplateAliasResponse
newUpdateTemplateAliasResponse pStatus_ =
  UpdateTemplateAliasResponse'
    { requestId =
        Prelude.Nothing,
      templateAlias = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateTemplateAliasResponse_requestId :: Lens.Lens' UpdateTemplateAliasResponse (Prelude.Maybe Prelude.Text)
updateTemplateAliasResponse_requestId = Lens.lens (\UpdateTemplateAliasResponse' {requestId} -> requestId) (\s@UpdateTemplateAliasResponse' {} a -> s {requestId = a} :: UpdateTemplateAliasResponse)

-- | The template alias.
updateTemplateAliasResponse_templateAlias :: Lens.Lens' UpdateTemplateAliasResponse (Prelude.Maybe TemplateAlias)
updateTemplateAliasResponse_templateAlias = Lens.lens (\UpdateTemplateAliasResponse' {templateAlias} -> templateAlias) (\s@UpdateTemplateAliasResponse' {} a -> s {templateAlias = a} :: UpdateTemplateAliasResponse)

-- | The HTTP status of the request.
updateTemplateAliasResponse_status :: Lens.Lens' UpdateTemplateAliasResponse Prelude.Int
updateTemplateAliasResponse_status = Lens.lens (\UpdateTemplateAliasResponse' {status} -> status) (\s@UpdateTemplateAliasResponse' {} a -> s {status = a} :: UpdateTemplateAliasResponse)

instance Prelude.NFData UpdateTemplateAliasResponse where
  rnf UpdateTemplateAliasResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf templateAlias
      `Prelude.seq` Prelude.rnf status
