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
-- Module      : Amazonka.ElasticSearch.AssociatePackage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a package with an Amazon ES domain.
module Amazonka.ElasticSearch.AssociatePackage
  ( -- * Creating a Request
    AssociatePackage (..),
    newAssociatePackage,

    -- * Request Lenses
    associatePackage_packageID,
    associatePackage_domainName,

    -- * Destructuring the Response
    AssociatePackageResponse (..),
    newAssociatePackageResponse,

    -- * Response Lenses
    associatePackageResponse_domainPackageDetails,
    associatePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ AssociatePackage @ operation.
--
-- /See:/ 'newAssociatePackage' smart constructor.
data AssociatePackage = AssociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use
    -- @DescribePackages@ to find this value.
    packageID :: Prelude.Text,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageID', 'associatePackage_packageID' - Internal ID of the package that you want to associate with a domain. Use
-- @DescribePackages@ to find this value.
--
-- 'domainName', 'associatePackage_domainName' - Name of the domain that you want to associate the package with.
newAssociatePackage ::
  -- | 'packageID'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  AssociatePackage
newAssociatePackage pPackageID_ pDomainName_ =
  AssociatePackage'
    { packageID = pPackageID_,
      domainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use
-- @DescribePackages@ to find this value.
associatePackage_packageID :: Lens.Lens' AssociatePackage Prelude.Text
associatePackage_packageID = Lens.lens (\AssociatePackage' {packageID} -> packageID) (\s@AssociatePackage' {} a -> s {packageID = a} :: AssociatePackage)

-- | Name of the domain that you want to associate the package with.
associatePackage_domainName :: Lens.Lens' AssociatePackage Prelude.Text
associatePackage_domainName = Lens.lens (\AssociatePackage' {domainName} -> domainName) (\s@AssociatePackage' {} a -> s {domainName = a} :: AssociatePackage)

instance Core.AWSRequest AssociatePackage where
  type
    AWSResponse AssociatePackage =
      AssociatePackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePackageResponse'
            Prelude.<$> (x Core..?> "DomainPackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociatePackage where
  hashWithSalt _salt AssociatePackage' {..} =
    _salt `Prelude.hashWithSalt` packageID
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData AssociatePackage where
  rnf AssociatePackage' {..} =
    Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders AssociatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AssociatePackage where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath AssociatePackage where
  toPath AssociatePackage' {..} =
    Prelude.mconcat
      [ "/2015-01-01/packages/associate/",
        Core.toBS packageID,
        "/",
        Core.toBS domainName
      ]

instance Core.ToQuery AssociatePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ AssociatePackage @ operation.
--
-- /See:/ 'newAssociatePackageResponse' smart constructor.
data AssociatePackageResponse = AssociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Prelude.Maybe DomainPackageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainPackageDetails', 'associatePackageResponse_domainPackageDetails' - @DomainPackageDetails@
--
-- 'httpStatus', 'associatePackageResponse_httpStatus' - The response's http status code.
newAssociatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePackageResponse
newAssociatePackageResponse pHttpStatus_ =
  AssociatePackageResponse'
    { domainPackageDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | @DomainPackageDetails@
associatePackageResponse_domainPackageDetails :: Lens.Lens' AssociatePackageResponse (Prelude.Maybe DomainPackageDetails)
associatePackageResponse_domainPackageDetails = Lens.lens (\AssociatePackageResponse' {domainPackageDetails} -> domainPackageDetails) (\s@AssociatePackageResponse' {} a -> s {domainPackageDetails = a} :: AssociatePackageResponse)

-- | The response's http status code.
associatePackageResponse_httpStatus :: Lens.Lens' AssociatePackageResponse Prelude.Int
associatePackageResponse_httpStatus = Lens.lens (\AssociatePackageResponse' {httpStatus} -> httpStatus) (\s@AssociatePackageResponse' {} a -> s {httpStatus = a} :: AssociatePackageResponse)

instance Prelude.NFData AssociatePackageResponse where
  rnf AssociatePackageResponse' {..} =
    Prelude.rnf domainPackageDetails
      `Prelude.seq` Prelude.rnf httpStatus
