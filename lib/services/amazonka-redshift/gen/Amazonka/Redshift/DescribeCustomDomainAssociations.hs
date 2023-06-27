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
-- Module      : Amazonka.Redshift.DescribeCustomDomainAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information for custom domain associations for a cluster.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeCustomDomainAssociations
  ( -- * Creating a Request
    DescribeCustomDomainAssociations (..),
    newDescribeCustomDomainAssociations,

    -- * Request Lenses
    describeCustomDomainAssociations_customDomainCertificateArn,
    describeCustomDomainAssociations_customDomainName,
    describeCustomDomainAssociations_marker,
    describeCustomDomainAssociations_maxRecords,

    -- * Destructuring the Response
    DescribeCustomDomainAssociationsResponse (..),
    newDescribeCustomDomainAssociationsResponse,

    -- * Response Lenses
    describeCustomDomainAssociationsResponse_associations,
    describeCustomDomainAssociationsResponse_marker,
    describeCustomDomainAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomDomainAssociations' smart constructor.
data DescribeCustomDomainAssociations = DescribeCustomDomainAssociations'
  { -- | The certificate Amazon Resource Name (ARN) for the custom domain
    -- association.
    customDomainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name for the custom domain association.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The marker for the custom domain association.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum records setting for the associated custom domain.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDomainCertificateArn', 'describeCustomDomainAssociations_customDomainCertificateArn' - The certificate Amazon Resource Name (ARN) for the custom domain
-- association.
--
-- 'customDomainName', 'describeCustomDomainAssociations_customDomainName' - The custom domain name for the custom domain association.
--
-- 'marker', 'describeCustomDomainAssociations_marker' - The marker for the custom domain association.
--
-- 'maxRecords', 'describeCustomDomainAssociations_maxRecords' - The maximum records setting for the associated custom domain.
newDescribeCustomDomainAssociations ::
  DescribeCustomDomainAssociations
newDescribeCustomDomainAssociations =
  DescribeCustomDomainAssociations'
    { customDomainCertificateArn =
        Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The certificate Amazon Resource Name (ARN) for the custom domain
-- association.
describeCustomDomainAssociations_customDomainCertificateArn :: Lens.Lens' DescribeCustomDomainAssociations (Prelude.Maybe Prelude.Text)
describeCustomDomainAssociations_customDomainCertificateArn = Lens.lens (\DescribeCustomDomainAssociations' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@DescribeCustomDomainAssociations' {} a -> s {customDomainCertificateArn = a} :: DescribeCustomDomainAssociations)

-- | The custom domain name for the custom domain association.
describeCustomDomainAssociations_customDomainName :: Lens.Lens' DescribeCustomDomainAssociations (Prelude.Maybe Prelude.Text)
describeCustomDomainAssociations_customDomainName = Lens.lens (\DescribeCustomDomainAssociations' {customDomainName} -> customDomainName) (\s@DescribeCustomDomainAssociations' {} a -> s {customDomainName = a} :: DescribeCustomDomainAssociations)

-- | The marker for the custom domain association.
describeCustomDomainAssociations_marker :: Lens.Lens' DescribeCustomDomainAssociations (Prelude.Maybe Prelude.Text)
describeCustomDomainAssociations_marker = Lens.lens (\DescribeCustomDomainAssociations' {marker} -> marker) (\s@DescribeCustomDomainAssociations' {} a -> s {marker = a} :: DescribeCustomDomainAssociations)

-- | The maximum records setting for the associated custom domain.
describeCustomDomainAssociations_maxRecords :: Lens.Lens' DescribeCustomDomainAssociations (Prelude.Maybe Prelude.Int)
describeCustomDomainAssociations_maxRecords = Lens.lens (\DescribeCustomDomainAssociations' {maxRecords} -> maxRecords) (\s@DescribeCustomDomainAssociations' {} a -> s {maxRecords = a} :: DescribeCustomDomainAssociations)

instance
  Core.AWSPager
    DescribeCustomDomainAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCustomDomainAssociationsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCustomDomainAssociationsResponse_associations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeCustomDomainAssociations_marker
          Lens..~ rs
          Lens.^? describeCustomDomainAssociationsResponse_marker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeCustomDomainAssociations
  where
  type
    AWSResponse DescribeCustomDomainAssociations =
      DescribeCustomDomainAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCustomDomainAssociationsResult"
      ( \s h x ->
          DescribeCustomDomainAssociationsResponse'
            Prelude.<$> ( x
                            Data..@? "Associations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Association")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomDomainAssociations
  where
  hashWithSalt
    _salt
    DescribeCustomDomainAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` customDomainCertificateArn
        `Prelude.hashWithSalt` customDomainName
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeCustomDomainAssociations
  where
  rnf DescribeCustomDomainAssociations' {..} =
    Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeCustomDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCustomDomainAssociations where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeCustomDomainAssociations
  where
  toQuery DescribeCustomDomainAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeCustomDomainAssociations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "CustomDomainCertificateArn"
          Data.=: customDomainCertificateArn,
        "CustomDomainName" Data.=: customDomainName,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeCustomDomainAssociationsResponse' smart constructor.
data DescribeCustomDomainAssociationsResponse = DescribeCustomDomainAssociationsResponse'
  { -- | The associations for the custom domain.
    associations :: Prelude.Maybe [Association],
    -- | The marker for the custom domain association.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'describeCustomDomainAssociationsResponse_associations' - The associations for the custom domain.
--
-- 'marker', 'describeCustomDomainAssociationsResponse_marker' - The marker for the custom domain association.
--
-- 'httpStatus', 'describeCustomDomainAssociationsResponse_httpStatus' - The response's http status code.
newDescribeCustomDomainAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomDomainAssociationsResponse
newDescribeCustomDomainAssociationsResponse
  pHttpStatus_ =
    DescribeCustomDomainAssociationsResponse'
      { associations =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The associations for the custom domain.
describeCustomDomainAssociationsResponse_associations :: Lens.Lens' DescribeCustomDomainAssociationsResponse (Prelude.Maybe [Association])
describeCustomDomainAssociationsResponse_associations = Lens.lens (\DescribeCustomDomainAssociationsResponse' {associations} -> associations) (\s@DescribeCustomDomainAssociationsResponse' {} a -> s {associations = a} :: DescribeCustomDomainAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the custom domain association.
describeCustomDomainAssociationsResponse_marker :: Lens.Lens' DescribeCustomDomainAssociationsResponse (Prelude.Maybe Prelude.Text)
describeCustomDomainAssociationsResponse_marker = Lens.lens (\DescribeCustomDomainAssociationsResponse' {marker} -> marker) (\s@DescribeCustomDomainAssociationsResponse' {} a -> s {marker = a} :: DescribeCustomDomainAssociationsResponse)

-- | The response's http status code.
describeCustomDomainAssociationsResponse_httpStatus :: Lens.Lens' DescribeCustomDomainAssociationsResponse Prelude.Int
describeCustomDomainAssociationsResponse_httpStatus = Lens.lens (\DescribeCustomDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomDomainAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeCustomDomainAssociationsResponse)

instance
  Prelude.NFData
    DescribeCustomDomainAssociationsResponse
  where
  rnf DescribeCustomDomainAssociationsResponse' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
