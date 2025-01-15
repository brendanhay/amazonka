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
-- Module      : Amazonka.CloudSearch.DescribeDomainEndpointOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain\'s endpoint options, specifically whether all
-- requests to the domain must arrive over HTTPS. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeDomainEndpointOptions
  ( -- * Creating a Request
    DescribeDomainEndpointOptions (..),
    newDescribeDomainEndpointOptions,

    -- * Request Lenses
    describeDomainEndpointOptions_deployed,
    describeDomainEndpointOptions_domainName,

    -- * Destructuring the Response
    DescribeDomainEndpointOptionsResponse (..),
    newDescribeDomainEndpointOptionsResponse,

    -- * Response Lenses
    describeDomainEndpointOptionsResponse_domainEndpointOptions,
    describeDomainEndpointOptionsResponse_httpStatus,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainEndpointOptions@
-- operation. Specify the name of the domain you want to describe. To show
-- the active configuration and exclude any pending changes, set the
-- Deployed option to @true@.
--
-- /See:/ 'newDescribeDomainEndpointOptions' smart constructor.
data DescribeDomainEndpointOptions = DescribeDomainEndpointOptions'
  { -- | Whether to retrieve the latest configuration (which might be in a
    -- Processing state) or the current, active configuration. Defaults to
    -- @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | A string that represents the name of a domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeDomainEndpointOptions_deployed' - Whether to retrieve the latest configuration (which might be in a
-- Processing state) or the current, active configuration. Defaults to
-- @false@.
--
-- 'domainName', 'describeDomainEndpointOptions_domainName' - A string that represents the name of a domain.
newDescribeDomainEndpointOptions ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainEndpointOptions
newDescribeDomainEndpointOptions pDomainName_ =
  DescribeDomainEndpointOptions'
    { deployed =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to retrieve the latest configuration (which might be in a
-- Processing state) or the current, active configuration. Defaults to
-- @false@.
describeDomainEndpointOptions_deployed :: Lens.Lens' DescribeDomainEndpointOptions (Prelude.Maybe Prelude.Bool)
describeDomainEndpointOptions_deployed = Lens.lens (\DescribeDomainEndpointOptions' {deployed} -> deployed) (\s@DescribeDomainEndpointOptions' {} a -> s {deployed = a} :: DescribeDomainEndpointOptions)

-- | A string that represents the name of a domain.
describeDomainEndpointOptions_domainName :: Lens.Lens' DescribeDomainEndpointOptions Prelude.Text
describeDomainEndpointOptions_domainName = Lens.lens (\DescribeDomainEndpointOptions' {domainName} -> domainName) (\s@DescribeDomainEndpointOptions' {} a -> s {domainName = a} :: DescribeDomainEndpointOptions)

instance
  Core.AWSRequest
    DescribeDomainEndpointOptions
  where
  type
    AWSResponse DescribeDomainEndpointOptions =
      DescribeDomainEndpointOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDomainEndpointOptionsResult"
      ( \s h x ->
          DescribeDomainEndpointOptionsResponse'
            Prelude.<$> (x Data..@? "DomainEndpointOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDomainEndpointOptions
  where
  hashWithSalt _salt DescribeDomainEndpointOptions' {..} =
    _salt
      `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainEndpointOptions where
  rnf DescribeDomainEndpointOptions' {..} =
    Prelude.rnf deployed `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainEndpointOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainEndpointOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDomainEndpointOptions where
  toQuery DescribeDomainEndpointOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDomainEndpointOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Data.=: deployed,
        "DomainName" Data.=: domainName
      ]

-- | The result of a @DescribeDomainEndpointOptions@ request. Contains the
-- status and configuration of a search domain\'s endpoint options.
--
-- /See:/ 'newDescribeDomainEndpointOptionsResponse' smart constructor.
data DescribeDomainEndpointOptionsResponse = DescribeDomainEndpointOptionsResponse'
  { -- | The status and configuration of a search domain\'s endpoint options.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainEndpointOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainEndpointOptions', 'describeDomainEndpointOptionsResponse_domainEndpointOptions' - The status and configuration of a search domain\'s endpoint options.
--
-- 'httpStatus', 'describeDomainEndpointOptionsResponse_httpStatus' - The response's http status code.
newDescribeDomainEndpointOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainEndpointOptionsResponse
newDescribeDomainEndpointOptionsResponse pHttpStatus_ =
  DescribeDomainEndpointOptionsResponse'
    { domainEndpointOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status and configuration of a search domain\'s endpoint options.
describeDomainEndpointOptionsResponse_domainEndpointOptions :: Lens.Lens' DescribeDomainEndpointOptionsResponse (Prelude.Maybe DomainEndpointOptionsStatus)
describeDomainEndpointOptionsResponse_domainEndpointOptions = Lens.lens (\DescribeDomainEndpointOptionsResponse' {domainEndpointOptions} -> domainEndpointOptions) (\s@DescribeDomainEndpointOptionsResponse' {} a -> s {domainEndpointOptions = a} :: DescribeDomainEndpointOptionsResponse)

-- | The response's http status code.
describeDomainEndpointOptionsResponse_httpStatus :: Lens.Lens' DescribeDomainEndpointOptionsResponse Prelude.Int
describeDomainEndpointOptionsResponse_httpStatus = Lens.lens (\DescribeDomainEndpointOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainEndpointOptionsResponse' {} a -> s {httpStatus = a} :: DescribeDomainEndpointOptionsResponse)

instance
  Prelude.NFData
    DescribeDomainEndpointOptionsResponse
  where
  rnf DescribeDomainEndpointOptionsResponse' {..} =
    Prelude.rnf domainEndpointOptions `Prelude.seq`
      Prelude.rnf httpStatus
