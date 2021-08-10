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
-- Module      : Network.AWS.CloudSearch.UpdateDomainEndpointOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the domain\'s endpoint options, specifically whether all
-- requests to the domain must arrive over HTTPS. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.UpdateDomainEndpointOptions
  ( -- * Creating a Request
    UpdateDomainEndpointOptions (..),
    newUpdateDomainEndpointOptions,

    -- * Request Lenses
    updateDomainEndpointOptions_domainName,
    updateDomainEndpointOptions_domainEndpointOptions,

    -- * Destructuring the Response
    UpdateDomainEndpointOptionsResponse (..),
    newUpdateDomainEndpointOptionsResponse,

    -- * Response Lenses
    updateDomainEndpointOptionsResponse_domainEndpointOptions,
    updateDomainEndpointOptionsResponse_httpStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @UpdateDomainEndpointOptions@
-- operation. Specifies the name of the domain you want to update and the
-- domain endpoint options.
--
-- /See:/ 'newUpdateDomainEndpointOptions' smart constructor.
data UpdateDomainEndpointOptions = UpdateDomainEndpointOptions'
  { -- | A string that represents the name of a domain.
    domainName :: Prelude.Text,
    -- | Whether to require that all requests to the domain arrive over HTTPS. We
    -- recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For
    -- compatibility with older clients, the default is
    -- Policy-Min-TLS-1-0-2019-07.
    domainEndpointOptions :: DomainEndpointOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainEndpointOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'updateDomainEndpointOptions_domainName' - A string that represents the name of a domain.
--
-- 'domainEndpointOptions', 'updateDomainEndpointOptions_domainEndpointOptions' - Whether to require that all requests to the domain arrive over HTTPS. We
-- recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For
-- compatibility with older clients, the default is
-- Policy-Min-TLS-1-0-2019-07.
newUpdateDomainEndpointOptions ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'domainEndpointOptions'
  DomainEndpointOptions ->
  UpdateDomainEndpointOptions
newUpdateDomainEndpointOptions
  pDomainName_
  pDomainEndpointOptions_ =
    UpdateDomainEndpointOptions'
      { domainName =
          pDomainName_,
        domainEndpointOptions =
          pDomainEndpointOptions_
      }

-- | A string that represents the name of a domain.
updateDomainEndpointOptions_domainName :: Lens.Lens' UpdateDomainEndpointOptions Prelude.Text
updateDomainEndpointOptions_domainName = Lens.lens (\UpdateDomainEndpointOptions' {domainName} -> domainName) (\s@UpdateDomainEndpointOptions' {} a -> s {domainName = a} :: UpdateDomainEndpointOptions)

-- | Whether to require that all requests to the domain arrive over HTTPS. We
-- recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For
-- compatibility with older clients, the default is
-- Policy-Min-TLS-1-0-2019-07.
updateDomainEndpointOptions_domainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptions DomainEndpointOptions
updateDomainEndpointOptions_domainEndpointOptions = Lens.lens (\UpdateDomainEndpointOptions' {domainEndpointOptions} -> domainEndpointOptions) (\s@UpdateDomainEndpointOptions' {} a -> s {domainEndpointOptions = a} :: UpdateDomainEndpointOptions)

instance Core.AWSRequest UpdateDomainEndpointOptions where
  type
    AWSResponse UpdateDomainEndpointOptions =
      UpdateDomainEndpointOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateDomainEndpointOptionsResult"
      ( \s h x ->
          UpdateDomainEndpointOptionsResponse'
            Prelude.<$> (x Core..@? "DomainEndpointOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainEndpointOptions

instance Prelude.NFData UpdateDomainEndpointOptions

instance Core.ToHeaders UpdateDomainEndpointOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateDomainEndpointOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDomainEndpointOptions where
  toQuery UpdateDomainEndpointOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "UpdateDomainEndpointOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Core.=: domainName,
        "DomainEndpointOptions"
          Core.=: domainEndpointOptions
      ]

-- | The result of a @UpdateDomainEndpointOptions@ request. Contains the
-- configuration and status of the domain\'s endpoint options.
--
-- /See:/ 'newUpdateDomainEndpointOptionsResponse' smart constructor.
data UpdateDomainEndpointOptionsResponse = UpdateDomainEndpointOptionsResponse'
  { -- | The newly-configured domain endpoint options.
    domainEndpointOptions :: Prelude.Maybe DomainEndpointOptionsStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainEndpointOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainEndpointOptions', 'updateDomainEndpointOptionsResponse_domainEndpointOptions' - The newly-configured domain endpoint options.
--
-- 'httpStatus', 'updateDomainEndpointOptionsResponse_httpStatus' - The response's http status code.
newUpdateDomainEndpointOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainEndpointOptionsResponse
newUpdateDomainEndpointOptionsResponse pHttpStatus_ =
  UpdateDomainEndpointOptionsResponse'
    { domainEndpointOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly-configured domain endpoint options.
updateDomainEndpointOptionsResponse_domainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptionsResponse (Prelude.Maybe DomainEndpointOptionsStatus)
updateDomainEndpointOptionsResponse_domainEndpointOptions = Lens.lens (\UpdateDomainEndpointOptionsResponse' {domainEndpointOptions} -> domainEndpointOptions) (\s@UpdateDomainEndpointOptionsResponse' {} a -> s {domainEndpointOptions = a} :: UpdateDomainEndpointOptionsResponse)

-- | The response's http status code.
updateDomainEndpointOptionsResponse_httpStatus :: Lens.Lens' UpdateDomainEndpointOptionsResponse Prelude.Int
updateDomainEndpointOptionsResponse_httpStatus = Lens.lens (\UpdateDomainEndpointOptionsResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainEndpointOptionsResponse' {} a -> s {httpStatus = a} :: UpdateDomainEndpointOptionsResponse)

instance
  Prelude.NFData
    UpdateDomainEndpointOptionsResponse
