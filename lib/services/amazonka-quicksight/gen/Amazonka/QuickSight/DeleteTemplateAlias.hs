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
-- Module      : Amazonka.QuickSight.DeleteTemplateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the item that the specified template alias points to. If you
-- provide a specific alias, you delete the version of the template that
-- the alias points to.
module Amazonka.QuickSight.DeleteTemplateAlias
  ( -- * Creating a Request
    DeleteTemplateAlias (..),
    newDeleteTemplateAlias,

    -- * Request Lenses
    deleteTemplateAlias_awsAccountId,
    deleteTemplateAlias_templateId,
    deleteTemplateAlias_aliasName,

    -- * Destructuring the Response
    DeleteTemplateAliasResponse (..),
    newDeleteTemplateAliasResponse,

    -- * Response Lenses
    deleteTemplateAliasResponse_aliasName,
    deleteTemplateAliasResponse_arn,
    deleteTemplateAliasResponse_requestId,
    deleteTemplateAliasResponse_templateId,
    deleteTemplateAliasResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTemplateAlias' smart constructor.
data DeleteTemplateAlias = DeleteTemplateAlias'
  { -- | The ID of the Amazon Web Services account that contains the item to
    -- delete.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template that the specified alias is for.
    templateId :: Prelude.Text,
    -- | The name for the template alias. To delete a specific alias, you delete
    -- the version that the alias points to. You can specify the alias name, or
    -- specify the latest version of the template by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteTemplateAlias_awsAccountId' - The ID of the Amazon Web Services account that contains the item to
-- delete.
--
-- 'templateId', 'deleteTemplateAlias_templateId' - The ID for the template that the specified alias is for.
--
-- 'aliasName', 'deleteTemplateAlias_aliasName' - The name for the template alias. To delete a specific alias, you delete
-- the version that the alias points to. You can specify the alias name, or
-- specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter.
newDeleteTemplateAlias ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'aliasName'
  Prelude.Text ->
  DeleteTemplateAlias
newDeleteTemplateAlias
  pAwsAccountId_
  pTemplateId_
  pAliasName_ =
    DeleteTemplateAlias'
      { awsAccountId = pAwsAccountId_,
        templateId = pTemplateId_,
        aliasName = pAliasName_
      }

-- | The ID of the Amazon Web Services account that contains the item to
-- delete.
deleteTemplateAlias_awsAccountId :: Lens.Lens' DeleteTemplateAlias Prelude.Text
deleteTemplateAlias_awsAccountId = Lens.lens (\DeleteTemplateAlias' {awsAccountId} -> awsAccountId) (\s@DeleteTemplateAlias' {} a -> s {awsAccountId = a} :: DeleteTemplateAlias)

-- | The ID for the template that the specified alias is for.
deleteTemplateAlias_templateId :: Lens.Lens' DeleteTemplateAlias Prelude.Text
deleteTemplateAlias_templateId = Lens.lens (\DeleteTemplateAlias' {templateId} -> templateId) (\s@DeleteTemplateAlias' {} a -> s {templateId = a} :: DeleteTemplateAlias)

-- | The name for the template alias. To delete a specific alias, you delete
-- the version that the alias points to. You can specify the alias name, or
-- specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter.
deleteTemplateAlias_aliasName :: Lens.Lens' DeleteTemplateAlias Prelude.Text
deleteTemplateAlias_aliasName = Lens.lens (\DeleteTemplateAlias' {aliasName} -> aliasName) (\s@DeleteTemplateAlias' {} a -> s {aliasName = a} :: DeleteTemplateAlias)

instance Core.AWSRequest DeleteTemplateAlias where
  type
    AWSResponse DeleteTemplateAlias =
      DeleteTemplateAliasResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTemplateAliasResponse'
            Prelude.<$> (x Data..?> "AliasName")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTemplateAlias where
  hashWithSalt _salt DeleteTemplateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData DeleteTemplateAlias where
  rnf DeleteTemplateAlias' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf templateId `Prelude.seq`
        Prelude.rnf aliasName

instance Data.ToHeaders DeleteTemplateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTemplateAlias where
  toPath DeleteTemplateAlias' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/aliases/",
        Data.toBS aliasName
      ]

instance Data.ToQuery DeleteTemplateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTemplateAliasResponse' smart constructor.
data DeleteTemplateAliasResponse = DeleteTemplateAliasResponse'
  { -- | The name for the template alias.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the template you want to delete.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An ID for the template associated with the deletion.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'deleteTemplateAliasResponse_aliasName' - The name for the template alias.
--
-- 'arn', 'deleteTemplateAliasResponse_arn' - The Amazon Resource Name (ARN) of the template you want to delete.
--
-- 'requestId', 'deleteTemplateAliasResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateId', 'deleteTemplateAliasResponse_templateId' - An ID for the template associated with the deletion.
--
-- 'status', 'deleteTemplateAliasResponse_status' - The HTTP status of the request.
newDeleteTemplateAliasResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteTemplateAliasResponse
newDeleteTemplateAliasResponse pStatus_ =
  DeleteTemplateAliasResponse'
    { aliasName =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateId = Prelude.Nothing,
      status = pStatus_
    }

-- | The name for the template alias.
deleteTemplateAliasResponse_aliasName :: Lens.Lens' DeleteTemplateAliasResponse (Prelude.Maybe Prelude.Text)
deleteTemplateAliasResponse_aliasName = Lens.lens (\DeleteTemplateAliasResponse' {aliasName} -> aliasName) (\s@DeleteTemplateAliasResponse' {} a -> s {aliasName = a} :: DeleteTemplateAliasResponse)

-- | The Amazon Resource Name (ARN) of the template you want to delete.
deleteTemplateAliasResponse_arn :: Lens.Lens' DeleteTemplateAliasResponse (Prelude.Maybe Prelude.Text)
deleteTemplateAliasResponse_arn = Lens.lens (\DeleteTemplateAliasResponse' {arn} -> arn) (\s@DeleteTemplateAliasResponse' {} a -> s {arn = a} :: DeleteTemplateAliasResponse)

-- | The Amazon Web Services request ID for this operation.
deleteTemplateAliasResponse_requestId :: Lens.Lens' DeleteTemplateAliasResponse (Prelude.Maybe Prelude.Text)
deleteTemplateAliasResponse_requestId = Lens.lens (\DeleteTemplateAliasResponse' {requestId} -> requestId) (\s@DeleteTemplateAliasResponse' {} a -> s {requestId = a} :: DeleteTemplateAliasResponse)

-- | An ID for the template associated with the deletion.
deleteTemplateAliasResponse_templateId :: Lens.Lens' DeleteTemplateAliasResponse (Prelude.Maybe Prelude.Text)
deleteTemplateAliasResponse_templateId = Lens.lens (\DeleteTemplateAliasResponse' {templateId} -> templateId) (\s@DeleteTemplateAliasResponse' {} a -> s {templateId = a} :: DeleteTemplateAliasResponse)

-- | The HTTP status of the request.
deleteTemplateAliasResponse_status :: Lens.Lens' DeleteTemplateAliasResponse Prelude.Int
deleteTemplateAliasResponse_status = Lens.lens (\DeleteTemplateAliasResponse' {status} -> status) (\s@DeleteTemplateAliasResponse' {} a -> s {status = a} :: DeleteTemplateAliasResponse)

instance Prelude.NFData DeleteTemplateAliasResponse where
  rnf DeleteTemplateAliasResponse' {..} =
    Prelude.rnf aliasName `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf templateId `Prelude.seq`
            Prelude.rnf status
