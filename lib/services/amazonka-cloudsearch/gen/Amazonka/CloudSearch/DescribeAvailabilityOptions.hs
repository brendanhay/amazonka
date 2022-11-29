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
-- Module      : Amazonka.CloudSearch.DescribeAvailabilityOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the @Deployed@ option to
-- @true@ to show the active configuration and exclude pending changes. For
-- more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeAvailabilityOptions
  ( -- * Creating a Request
    DescribeAvailabilityOptions (..),
    newDescribeAvailabilityOptions,

    -- * Request Lenses
    describeAvailabilityOptions_deployed,
    describeAvailabilityOptions_domainName,

    -- * Destructuring the Response
    DescribeAvailabilityOptionsResponse (..),
    newDescribeAvailabilityOptionsResponse,

    -- * Response Lenses
    describeAvailabilityOptionsResponse_availabilityOptions,
    describeAvailabilityOptionsResponse_httpStatus,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeAvailabilityOptions@
-- operation. Specifies the name of the domain you want to describe. To
-- show the active configuration and exclude any pending changes, set the
-- Deployed option to @true@.
--
-- /See:/ 'newDescribeAvailabilityOptions' smart constructor.
data DescribeAvailabilityOptions = DescribeAvailabilityOptions'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeAvailabilityOptions_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'domainName', 'describeAvailabilityOptions_domainName' - The name of the domain you want to describe.
newDescribeAvailabilityOptions ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeAvailabilityOptions
newDescribeAvailabilityOptions pDomainName_ =
  DescribeAvailabilityOptions'
    { deployed =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeAvailabilityOptions_deployed :: Lens.Lens' DescribeAvailabilityOptions (Prelude.Maybe Prelude.Bool)
describeAvailabilityOptions_deployed = Lens.lens (\DescribeAvailabilityOptions' {deployed} -> deployed) (\s@DescribeAvailabilityOptions' {} a -> s {deployed = a} :: DescribeAvailabilityOptions)

-- | The name of the domain you want to describe.
describeAvailabilityOptions_domainName :: Lens.Lens' DescribeAvailabilityOptions Prelude.Text
describeAvailabilityOptions_domainName = Lens.lens (\DescribeAvailabilityOptions' {domainName} -> domainName) (\s@DescribeAvailabilityOptions' {} a -> s {domainName = a} :: DescribeAvailabilityOptions)

instance Core.AWSRequest DescribeAvailabilityOptions where
  type
    AWSResponse DescribeAvailabilityOptions =
      DescribeAvailabilityOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAvailabilityOptionsResult"
      ( \s h x ->
          DescribeAvailabilityOptionsResponse'
            Prelude.<$> (x Core..@? "AvailabilityOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAvailabilityOptions where
  hashWithSalt _salt DescribeAvailabilityOptions' {..} =
    _salt `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeAvailabilityOptions where
  rnf DescribeAvailabilityOptions' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders DescribeAvailabilityOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAvailabilityOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAvailabilityOptions where
  toQuery DescribeAvailabilityOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeAvailabilityOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Core.=: deployed,
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeAvailabilityOptions@ request. Indicates whether
-- or not the Multi-AZ option is enabled for the domain specified in the
-- request.
--
-- /See:/ 'newDescribeAvailabilityOptionsResponse' smart constructor.
data DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse'
  { -- | The availability options configured for the domain. Indicates whether
    -- Multi-AZ is enabled for the domain.
    availabilityOptions :: Prelude.Maybe AvailabilityOptionsStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityOptions', 'describeAvailabilityOptionsResponse_availabilityOptions' - The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
--
-- 'httpStatus', 'describeAvailabilityOptionsResponse_httpStatus' - The response's http status code.
newDescribeAvailabilityOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAvailabilityOptionsResponse
newDescribeAvailabilityOptionsResponse pHttpStatus_ =
  DescribeAvailabilityOptionsResponse'
    { availabilityOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
describeAvailabilityOptionsResponse_availabilityOptions :: Lens.Lens' DescribeAvailabilityOptionsResponse (Prelude.Maybe AvailabilityOptionsStatus)
describeAvailabilityOptionsResponse_availabilityOptions = Lens.lens (\DescribeAvailabilityOptionsResponse' {availabilityOptions} -> availabilityOptions) (\s@DescribeAvailabilityOptionsResponse' {} a -> s {availabilityOptions = a} :: DescribeAvailabilityOptionsResponse)

-- | The response's http status code.
describeAvailabilityOptionsResponse_httpStatus :: Lens.Lens' DescribeAvailabilityOptionsResponse Prelude.Int
describeAvailabilityOptionsResponse_httpStatus = Lens.lens (\DescribeAvailabilityOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailabilityOptionsResponse' {} a -> s {httpStatus = a} :: DescribeAvailabilityOptionsResponse)

instance
  Prelude.NFData
    DescribeAvailabilityOptionsResponse
  where
  rnf DescribeAvailabilityOptionsResponse' {..} =
    Prelude.rnf availabilityOptions
      `Prelude.seq` Prelude.rnf httpStatus
