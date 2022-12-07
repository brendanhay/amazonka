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
-- Module      : Amazonka.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the index fields configured for the search
-- domain. Can be limited to specific fields by name. By default, shows all
-- fields and includes any pending changes to the configuration. Set the
-- @Deployed@ option to @true@ to show the active configuration and exclude
-- pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Domain Information>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeIndexFields
  ( -- * Creating a Request
    DescribeIndexFields (..),
    newDescribeIndexFields,

    -- * Request Lenses
    describeIndexFields_deployed,
    describeIndexFields_fieldNames,
    describeIndexFields_domainName,

    -- * Destructuring the Response
    DescribeIndexFieldsResponse (..),
    newDescribeIndexFieldsResponse,

    -- * Response Lenses
    describeIndexFieldsResponse_httpStatus,
    describeIndexFieldsResponse_indexFields,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeIndexFields@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular index fields, specify the names of the index
-- fields you want to describe. To show the active configuration and
-- exclude any pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'newDescribeIndexFields' smart constructor.
data DescribeIndexFields = DescribeIndexFields'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | A list of the index fields you want to describe. If not specified,
    -- information is returned for all configured index fields.
    fieldNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndexFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeIndexFields_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'fieldNames', 'describeIndexFields_fieldNames' - A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
--
-- 'domainName', 'describeIndexFields_domainName' - The name of the domain you want to describe.
newDescribeIndexFields ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeIndexFields
newDescribeIndexFields pDomainName_ =
  DescribeIndexFields'
    { deployed = Prelude.Nothing,
      fieldNames = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeIndexFields_deployed :: Lens.Lens' DescribeIndexFields (Prelude.Maybe Prelude.Bool)
describeIndexFields_deployed = Lens.lens (\DescribeIndexFields' {deployed} -> deployed) (\s@DescribeIndexFields' {} a -> s {deployed = a} :: DescribeIndexFields)

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
describeIndexFields_fieldNames :: Lens.Lens' DescribeIndexFields (Prelude.Maybe [Prelude.Text])
describeIndexFields_fieldNames = Lens.lens (\DescribeIndexFields' {fieldNames} -> fieldNames) (\s@DescribeIndexFields' {} a -> s {fieldNames = a} :: DescribeIndexFields) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain you want to describe.
describeIndexFields_domainName :: Lens.Lens' DescribeIndexFields Prelude.Text
describeIndexFields_domainName = Lens.lens (\DescribeIndexFields' {domainName} -> domainName) (\s@DescribeIndexFields' {} a -> s {domainName = a} :: DescribeIndexFields)

instance Core.AWSRequest DescribeIndexFields where
  type
    AWSResponse DescribeIndexFields =
      DescribeIndexFieldsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeIndexFieldsResult"
      ( \s h x ->
          DescribeIndexFieldsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "IndexFields" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeIndexFields where
  hashWithSalt _salt DescribeIndexFields' {..} =
    _salt `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` fieldNames
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeIndexFields where
  rnf DescribeIndexFields' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf fieldNames
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeIndexFields where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIndexFields where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIndexFields where
  toQuery DescribeIndexFields' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIndexFields" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Data.=: deployed,
        "FieldNames"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> fieldNames),
        "DomainName" Data.=: domainName
      ]

-- | The result of a @DescribeIndexFields@ request. Contains the index fields
-- configured for the domain specified in the request.
--
-- /See:/ 'newDescribeIndexFieldsResponse' smart constructor.
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The index fields configured for the domain.
    indexFields :: [IndexFieldStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndexFieldsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeIndexFieldsResponse_httpStatus' - The response's http status code.
--
-- 'indexFields', 'describeIndexFieldsResponse_indexFields' - The index fields configured for the domain.
newDescribeIndexFieldsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexFieldsResponse
newDescribeIndexFieldsResponse pHttpStatus_ =
  DescribeIndexFieldsResponse'
    { httpStatus =
        pHttpStatus_,
      indexFields = Prelude.mempty
    }

-- | The response's http status code.
describeIndexFieldsResponse_httpStatus :: Lens.Lens' DescribeIndexFieldsResponse Prelude.Int
describeIndexFieldsResponse_httpStatus = Lens.lens (\DescribeIndexFieldsResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexFieldsResponse' {} a -> s {httpStatus = a} :: DescribeIndexFieldsResponse)

-- | The index fields configured for the domain.
describeIndexFieldsResponse_indexFields :: Lens.Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
describeIndexFieldsResponse_indexFields = Lens.lens (\DescribeIndexFieldsResponse' {indexFields} -> indexFields) (\s@DescribeIndexFieldsResponse' {} a -> s {indexFields = a} :: DescribeIndexFieldsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeIndexFieldsResponse where
  rnf DescribeIndexFieldsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf indexFields
