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
-- Module      : Amazonka.OpenSearch.DissociatePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a package from the specified Amazon OpenSearch Service domain.
-- The package can\'t be in use with any OpenSearch index for the
-- dissociation to succeed. The package is still available in OpenSearch
-- Service for association later. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DissociatePackage
  ( -- * Creating a Request
    DissociatePackage (..),
    newDissociatePackage,

    -- * Request Lenses
    dissociatePackage_packageID,
    dissociatePackage_domainName,

    -- * Destructuring the Response
    DissociatePackageResponse (..),
    newDissociatePackageResponse,

    -- * Response Lenses
    dissociatePackageResponse_domainPackageDetails,
    dissociatePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @DissociatePackage@
-- operation.
--
-- /See:/ 'newDissociatePackage' smart constructor.
data DissociatePackage = DissociatePackage'
  { -- | Internal ID of the package to dissociate from the domain. Use
    -- @ListPackagesForDomain@ to find this value.
    packageID :: Prelude.Text,
    -- | Name of the domain to dissociate the package from.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DissociatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageID', 'dissociatePackage_packageID' - Internal ID of the package to dissociate from the domain. Use
-- @ListPackagesForDomain@ to find this value.
--
-- 'domainName', 'dissociatePackage_domainName' - Name of the domain to dissociate the package from.
newDissociatePackage ::
  -- | 'packageID'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DissociatePackage
newDissociatePackage pPackageID_ pDomainName_ =
  DissociatePackage'
    { packageID = pPackageID_,
      domainName = pDomainName_
    }

-- | Internal ID of the package to dissociate from the domain. Use
-- @ListPackagesForDomain@ to find this value.
dissociatePackage_packageID :: Lens.Lens' DissociatePackage Prelude.Text
dissociatePackage_packageID = Lens.lens (\DissociatePackage' {packageID} -> packageID) (\s@DissociatePackage' {} a -> s {packageID = a} :: DissociatePackage)

-- | Name of the domain to dissociate the package from.
dissociatePackage_domainName :: Lens.Lens' DissociatePackage Prelude.Text
dissociatePackage_domainName = Lens.lens (\DissociatePackage' {domainName} -> domainName) (\s@DissociatePackage' {} a -> s {domainName = a} :: DissociatePackage)

instance Core.AWSRequest DissociatePackage where
  type
    AWSResponse DissociatePackage =
      DissociatePackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DissociatePackageResponse'
            Prelude.<$> (x Data..?> "DomainPackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DissociatePackage where
  hashWithSalt _salt DissociatePackage' {..} =
    _salt
      `Prelude.hashWithSalt` packageID
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DissociatePackage where
  rnf DissociatePackage' {..} =
    Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DissociatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DissociatePackage where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DissociatePackage where
  toPath DissociatePackage' {..} =
    Prelude.mconcat
      [ "/2021-01-01/packages/dissociate/",
        Data.toBS packageID,
        "/",
        Data.toBS domainName
      ]

instance Data.ToQuery DissociatePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by an @DissociatePackage@ operation.
--
-- /See:/ 'newDissociatePackageResponse' smart constructor.
data DissociatePackageResponse = DissociatePackageResponse'
  { -- | Information about a package that has been dissociated from the domain.
    domainPackageDetails :: Prelude.Maybe DomainPackageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DissociatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainPackageDetails', 'dissociatePackageResponse_domainPackageDetails' - Information about a package that has been dissociated from the domain.
--
-- 'httpStatus', 'dissociatePackageResponse_httpStatus' - The response's http status code.
newDissociatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DissociatePackageResponse
newDissociatePackageResponse pHttpStatus_ =
  DissociatePackageResponse'
    { domainPackageDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a package that has been dissociated from the domain.
dissociatePackageResponse_domainPackageDetails :: Lens.Lens' DissociatePackageResponse (Prelude.Maybe DomainPackageDetails)
dissociatePackageResponse_domainPackageDetails = Lens.lens (\DissociatePackageResponse' {domainPackageDetails} -> domainPackageDetails) (\s@DissociatePackageResponse' {} a -> s {domainPackageDetails = a} :: DissociatePackageResponse)

-- | The response's http status code.
dissociatePackageResponse_httpStatus :: Lens.Lens' DissociatePackageResponse Prelude.Int
dissociatePackageResponse_httpStatus = Lens.lens (\DissociatePackageResponse' {httpStatus} -> httpStatus) (\s@DissociatePackageResponse' {} a -> s {httpStatus = a} :: DissociatePackageResponse)

instance Prelude.NFData DissociatePackageResponse where
  rnf DissociatePackageResponse' {..} =
    Prelude.rnf domainPackageDetails
      `Prelude.seq` Prelude.rnf httpStatus
