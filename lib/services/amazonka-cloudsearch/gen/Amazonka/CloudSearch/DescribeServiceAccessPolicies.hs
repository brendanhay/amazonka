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
-- Module      : Amazonka.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the access policies that control access to the
-- domain\'s document and search endpoints. By default, shows the
-- configuration with any pending changes. Set the @Deployed@ option to
-- @true@ to show the active configuration and exclude pending changes. For
-- more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeServiceAccessPolicies
  ( -- * Creating a Request
    DescribeServiceAccessPolicies (..),
    newDescribeServiceAccessPolicies,

    -- * Request Lenses
    describeServiceAccessPolicies_deployed,
    describeServiceAccessPolicies_domainName,

    -- * Destructuring the Response
    DescribeServiceAccessPoliciesResponse (..),
    newDescribeServiceAccessPoliciesResponse,

    -- * Response Lenses
    describeServiceAccessPoliciesResponse_httpStatus,
    describeServiceAccessPoliciesResponse_accessPolicies,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeServiceAccessPolicies@
-- operation. Specifies the name of the domain you want to describe. To
-- show the active configuration and exclude any pending changes, set the
-- @Deployed@ option to @true@.
--
-- /See:/ 'newDescribeServiceAccessPolicies' smart constructor.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceAccessPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeServiceAccessPolicies_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'domainName', 'describeServiceAccessPolicies_domainName' - The name of the domain you want to describe.
newDescribeServiceAccessPolicies ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeServiceAccessPolicies
newDescribeServiceAccessPolicies pDomainName_ =
  DescribeServiceAccessPolicies'
    { deployed =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeServiceAccessPolicies_deployed :: Lens.Lens' DescribeServiceAccessPolicies (Prelude.Maybe Prelude.Bool)
describeServiceAccessPolicies_deployed = Lens.lens (\DescribeServiceAccessPolicies' {deployed} -> deployed) (\s@DescribeServiceAccessPolicies' {} a -> s {deployed = a} :: DescribeServiceAccessPolicies)

-- | The name of the domain you want to describe.
describeServiceAccessPolicies_domainName :: Lens.Lens' DescribeServiceAccessPolicies Prelude.Text
describeServiceAccessPolicies_domainName = Lens.lens (\DescribeServiceAccessPolicies' {domainName} -> domainName) (\s@DescribeServiceAccessPolicies' {} a -> s {domainName = a} :: DescribeServiceAccessPolicies)

instance
  Core.AWSRequest
    DescribeServiceAccessPolicies
  where
  type
    AWSResponse DescribeServiceAccessPolicies =
      DescribeServiceAccessPoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeServiceAccessPoliciesResult"
      ( \s h x ->
          DescribeServiceAccessPoliciesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "AccessPolicies")
      )

instance
  Prelude.Hashable
    DescribeServiceAccessPolicies
  where
  hashWithSalt _salt DescribeServiceAccessPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeServiceAccessPolicies where
  rnf DescribeServiceAccessPolicies' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeServiceAccessPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeServiceAccessPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServiceAccessPolicies where
  toQuery DescribeServiceAccessPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeServiceAccessPolicies" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Data.=: deployed,
        "DomainName" Data.=: domainName
      ]

-- | The result of a @DescribeServiceAccessPolicies@ request.
--
-- /See:/ 'newDescribeServiceAccessPoliciesResponse' smart constructor.
data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The access rules configured for the domain specified in the request.
    accessPolicies :: AccessPoliciesStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceAccessPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeServiceAccessPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'accessPolicies', 'describeServiceAccessPoliciesResponse_accessPolicies' - The access rules configured for the domain specified in the request.
newDescribeServiceAccessPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessPolicies'
  AccessPoliciesStatus ->
  DescribeServiceAccessPoliciesResponse
newDescribeServiceAccessPoliciesResponse
  pHttpStatus_
  pAccessPolicies_ =
    DescribeServiceAccessPoliciesResponse'
      { httpStatus =
          pHttpStatus_,
        accessPolicies = pAccessPolicies_
      }

-- | The response's http status code.
describeServiceAccessPoliciesResponse_httpStatus :: Lens.Lens' DescribeServiceAccessPoliciesResponse Prelude.Int
describeServiceAccessPoliciesResponse_httpStatus = Lens.lens (\DescribeServiceAccessPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceAccessPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeServiceAccessPoliciesResponse)

-- | The access rules configured for the domain specified in the request.
describeServiceAccessPoliciesResponse_accessPolicies :: Lens.Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
describeServiceAccessPoliciesResponse_accessPolicies = Lens.lens (\DescribeServiceAccessPoliciesResponse' {accessPolicies} -> accessPolicies) (\s@DescribeServiceAccessPoliciesResponse' {} a -> s {accessPolicies = a} :: DescribeServiceAccessPoliciesResponse)

instance
  Prelude.NFData
    DescribeServiceAccessPoliciesResponse
  where
  rnf DescribeServiceAccessPoliciesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessPolicies
