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
-- Module      : Amazonka.QuickSight.DescribeTemplateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the template alias for a template.
module Amazonka.QuickSight.DescribeTemplateAlias
  ( -- * Creating a Request
    DescribeTemplateAlias (..),
    newDescribeTemplateAlias,

    -- * Request Lenses
    describeTemplateAlias_awsAccountId,
    describeTemplateAlias_templateId,
    describeTemplateAlias_aliasName,

    -- * Destructuring the Response
    DescribeTemplateAliasResponse (..),
    newDescribeTemplateAliasResponse,

    -- * Response Lenses
    describeTemplateAliasResponse_requestId,
    describeTemplateAliasResponse_templateAlias,
    describeTemplateAliasResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTemplateAlias' smart constructor.
data DescribeTemplateAlias = DescribeTemplateAlias'
  { -- | The ID of the Amazon Web Services account that contains the template
    -- alias that you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text,
    -- | The name of the template alias that you want to describe. If you name a
    -- specific alias, you describe the version that the alias points to. You
    -- can specify the latest version of the template by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
    -- doesn\'t apply to templates.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTemplateAlias_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- alias that you\'re describing.
--
-- 'templateId', 'describeTemplateAlias_templateId' - The ID for the template.
--
-- 'aliasName', 'describeTemplateAlias_aliasName' - The name of the template alias that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
newDescribeTemplateAlias ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'aliasName'
  Prelude.Text ->
  DescribeTemplateAlias
newDescribeTemplateAlias
  pAwsAccountId_
  pTemplateId_
  pAliasName_ =
    DescribeTemplateAlias'
      { awsAccountId =
          pAwsAccountId_,
        templateId = pTemplateId_,
        aliasName = pAliasName_
      }

-- | The ID of the Amazon Web Services account that contains the template
-- alias that you\'re describing.
describeTemplateAlias_awsAccountId :: Lens.Lens' DescribeTemplateAlias Prelude.Text
describeTemplateAlias_awsAccountId = Lens.lens (\DescribeTemplateAlias' {awsAccountId} -> awsAccountId) (\s@DescribeTemplateAlias' {} a -> s {awsAccountId = a} :: DescribeTemplateAlias)

-- | The ID for the template.
describeTemplateAlias_templateId :: Lens.Lens' DescribeTemplateAlias Prelude.Text
describeTemplateAlias_templateId = Lens.lens (\DescribeTemplateAlias' {templateId} -> templateId) (\s@DescribeTemplateAlias' {} a -> s {templateId = a} :: DescribeTemplateAlias)

-- | The name of the template alias that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
describeTemplateAlias_aliasName :: Lens.Lens' DescribeTemplateAlias Prelude.Text
describeTemplateAlias_aliasName = Lens.lens (\DescribeTemplateAlias' {aliasName} -> aliasName) (\s@DescribeTemplateAlias' {} a -> s {aliasName = a} :: DescribeTemplateAlias)

instance Core.AWSRequest DescribeTemplateAlias where
  type
    AWSResponse DescribeTemplateAlias =
      DescribeTemplateAliasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTemplateAliasResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateAlias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTemplateAlias where
  hashWithSalt _salt DescribeTemplateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` aliasName

instance Prelude.NFData DescribeTemplateAlias where
  rnf DescribeTemplateAlias' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf aliasName

instance Data.ToHeaders DescribeTemplateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTemplateAlias where
  toPath DescribeTemplateAlias' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/aliases/",
        Data.toBS aliasName
      ]

instance Data.ToQuery DescribeTemplateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTemplateAliasResponse' smart constructor.
data DescribeTemplateAliasResponse = DescribeTemplateAliasResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the template alias.
    templateAlias :: Prelude.Maybe TemplateAlias,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeTemplateAliasResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateAlias', 'describeTemplateAliasResponse_templateAlias' - Information about the template alias.
--
-- 'status', 'describeTemplateAliasResponse_status' - The HTTP status of the request.
newDescribeTemplateAliasResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTemplateAliasResponse
newDescribeTemplateAliasResponse pStatus_ =
  DescribeTemplateAliasResponse'
    { requestId =
        Prelude.Nothing,
      templateAlias = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeTemplateAliasResponse_requestId :: Lens.Lens' DescribeTemplateAliasResponse (Prelude.Maybe Prelude.Text)
describeTemplateAliasResponse_requestId = Lens.lens (\DescribeTemplateAliasResponse' {requestId} -> requestId) (\s@DescribeTemplateAliasResponse' {} a -> s {requestId = a} :: DescribeTemplateAliasResponse)

-- | Information about the template alias.
describeTemplateAliasResponse_templateAlias :: Lens.Lens' DescribeTemplateAliasResponse (Prelude.Maybe TemplateAlias)
describeTemplateAliasResponse_templateAlias = Lens.lens (\DescribeTemplateAliasResponse' {templateAlias} -> templateAlias) (\s@DescribeTemplateAliasResponse' {} a -> s {templateAlias = a} :: DescribeTemplateAliasResponse)

-- | The HTTP status of the request.
describeTemplateAliasResponse_status :: Lens.Lens' DescribeTemplateAliasResponse Prelude.Int
describeTemplateAliasResponse_status = Lens.lens (\DescribeTemplateAliasResponse' {status} -> status) (\s@DescribeTemplateAliasResponse' {} a -> s {status = a} :: DescribeTemplateAliasResponse)

instance Prelude.NFData DescribeTemplateAliasResponse where
  rnf DescribeTemplateAliasResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf templateAlias
      `Prelude.seq` Prelude.rnf status
