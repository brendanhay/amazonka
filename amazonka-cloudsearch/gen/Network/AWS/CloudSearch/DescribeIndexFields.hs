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
-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudSearch.DescribeIndexFields
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

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    deployed :: Core.Maybe Core.Bool,
    -- | A list of the index fields you want to describe. If not specified,
    -- information is returned for all configured index fields.
    fieldNames :: Core.Maybe [Core.Text],
    -- | The name of the domain you want to describe.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeIndexFields
newDescribeIndexFields pDomainName_ =
  DescribeIndexFields'
    { deployed = Core.Nothing,
      fieldNames = Core.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeIndexFields_deployed :: Lens.Lens' DescribeIndexFields (Core.Maybe Core.Bool)
describeIndexFields_deployed = Lens.lens (\DescribeIndexFields' {deployed} -> deployed) (\s@DescribeIndexFields' {} a -> s {deployed = a} :: DescribeIndexFields)

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
describeIndexFields_fieldNames :: Lens.Lens' DescribeIndexFields (Core.Maybe [Core.Text])
describeIndexFields_fieldNames = Lens.lens (\DescribeIndexFields' {fieldNames} -> fieldNames) (\s@DescribeIndexFields' {} a -> s {fieldNames = a} :: DescribeIndexFields) Core.. Lens.mapping Lens._Coerce

-- | The name of the domain you want to describe.
describeIndexFields_domainName :: Lens.Lens' DescribeIndexFields Core.Text
describeIndexFields_domainName = Lens.lens (\DescribeIndexFields' {domainName} -> domainName) (\s@DescribeIndexFields' {} a -> s {domainName = a} :: DescribeIndexFields)

instance Core.AWSRequest DescribeIndexFields where
  type
    AWSResponse DescribeIndexFields =
      DescribeIndexFieldsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeIndexFieldsResult"
      ( \s h x ->
          DescribeIndexFieldsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "IndexFields" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable DescribeIndexFields

instance Core.NFData DescribeIndexFields

instance Core.ToHeaders DescribeIndexFields where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeIndexFields where
  toPath = Core.const "/"

instance Core.ToQuery DescribeIndexFields where
  toQuery DescribeIndexFields' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeIndexFields" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "Deployed" Core.=: deployed,
        "FieldNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> fieldNames),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeIndexFields@ request. Contains the index fields
-- configured for the domain specified in the request.
--
-- /See:/ 'newDescribeIndexFieldsResponse' smart constructor.
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The index fields configured for the domain.
    indexFields :: [IndexFieldStatus]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeIndexFieldsResponse
newDescribeIndexFieldsResponse pHttpStatus_ =
  DescribeIndexFieldsResponse'
    { httpStatus =
        pHttpStatus_,
      indexFields = Core.mempty
    }

-- | The response's http status code.
describeIndexFieldsResponse_httpStatus :: Lens.Lens' DescribeIndexFieldsResponse Core.Int
describeIndexFieldsResponse_httpStatus = Lens.lens (\DescribeIndexFieldsResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexFieldsResponse' {} a -> s {httpStatus = a} :: DescribeIndexFieldsResponse)

-- | The index fields configured for the domain.
describeIndexFieldsResponse_indexFields :: Lens.Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
describeIndexFieldsResponse_indexFields = Lens.lens (\DescribeIndexFieldsResponse' {indexFields} -> indexFields) (\s@DescribeIndexFieldsResponse' {} a -> s {indexFields = a} :: DescribeIndexFieldsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeIndexFieldsResponse
