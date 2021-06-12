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
-- Module      : Network.AWS.ElasticSearch.AssociatePackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a package with an Amazon ES domain.
module Network.AWS.ElasticSearch.AssociatePackage
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ AssociatePackage @ operation.
--
-- /See:/ 'newAssociatePackage' smart constructor.
data AssociatePackage = AssociatePackage'
  { -- | Internal ID of the package that you want to associate with a domain. Use
    -- @DescribePackages@ to find this value.
    packageID :: Core.Text,
    -- | Name of the domain that you want to associate the package with.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'domainName'
  Core.Text ->
  AssociatePackage
newAssociatePackage pPackageID_ pDomainName_ =
  AssociatePackage'
    { packageID = pPackageID_,
      domainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use
-- @DescribePackages@ to find this value.
associatePackage_packageID :: Lens.Lens' AssociatePackage Core.Text
associatePackage_packageID = Lens.lens (\AssociatePackage' {packageID} -> packageID) (\s@AssociatePackage' {} a -> s {packageID = a} :: AssociatePackage)

-- | Name of the domain that you want to associate the package with.
associatePackage_domainName :: Lens.Lens' AssociatePackage Core.Text
associatePackage_domainName = Lens.lens (\AssociatePackage' {domainName} -> domainName) (\s@AssociatePackage' {} a -> s {domainName = a} :: AssociatePackage)

instance Core.AWSRequest AssociatePackage where
  type
    AWSResponse AssociatePackage =
      AssociatePackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePackageResponse'
            Core.<$> (x Core..?> "DomainPackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociatePackage

instance Core.NFData AssociatePackage

instance Core.ToHeaders AssociatePackage where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AssociatePackage where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath AssociatePackage where
  toPath AssociatePackage' {..} =
    Core.mconcat
      [ "/2015-01-01/packages/associate/",
        Core.toBS packageID,
        "/",
        Core.toBS domainName
      ]

instance Core.ToQuery AssociatePackage where
  toQuery = Core.const Core.mempty

-- | Container for response returned by @ AssociatePackage @ operation.
--
-- /See:/ 'newAssociatePackageResponse' smart constructor.
data AssociatePackageResponse = AssociatePackageResponse'
  { -- | @DomainPackageDetails@
    domainPackageDetails :: Core.Maybe DomainPackageDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociatePackageResponse
newAssociatePackageResponse pHttpStatus_ =
  AssociatePackageResponse'
    { domainPackageDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | @DomainPackageDetails@
associatePackageResponse_domainPackageDetails :: Lens.Lens' AssociatePackageResponse (Core.Maybe DomainPackageDetails)
associatePackageResponse_domainPackageDetails = Lens.lens (\AssociatePackageResponse' {domainPackageDetails} -> domainPackageDetails) (\s@AssociatePackageResponse' {} a -> s {domainPackageDetails = a} :: AssociatePackageResponse)

-- | The response's http status code.
associatePackageResponse_httpStatus :: Lens.Lens' AssociatePackageResponse Core.Int
associatePackageResponse_httpStatus = Lens.lens (\AssociatePackageResponse' {httpStatus} -> httpStatus) (\s@AssociatePackageResponse' {} a -> s {httpStatus = a} :: AssociatePackageResponse)

instance Core.NFData AssociatePackageResponse
