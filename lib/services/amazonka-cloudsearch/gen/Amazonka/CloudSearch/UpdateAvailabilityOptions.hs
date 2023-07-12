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
-- Module      : Amazonka.CloudSearch.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional
-- Availability Zone in the same Region to increase fault tolerance in the
-- event of a service disruption. Changes to the Multi-AZ option can take
-- about half an hour to become active. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.UpdateAvailabilityOptions
  ( -- * Creating a Request
    UpdateAvailabilityOptions (..),
    newUpdateAvailabilityOptions,

    -- * Request Lenses
    updateAvailabilityOptions_domainName,
    updateAvailabilityOptions_multiAZ,

    -- * Destructuring the Response
    UpdateAvailabilityOptionsResponse (..),
    newUpdateAvailabilityOptionsResponse,

    -- * Response Lenses
    updateAvailabilityOptionsResponse_availabilityOptions,
    updateAvailabilityOptionsResponse_httpStatus,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @UpdateAvailabilityOptions@
-- operation. Specifies the name of the domain you want to update and the
-- Multi-AZ availability option.
--
-- /See:/ 'newUpdateAvailabilityOptions' smart constructor.
data UpdateAvailabilityOptions = UpdateAvailabilityOptions'
  { domainName :: Prelude.Text,
    -- | You expand an existing search domain to a second Availability Zone by
    -- setting the Multi-AZ option to true. Similarly, you can turn off the
    -- Multi-AZ option to downgrade the domain to a single Availability Zone by
    -- setting the Multi-AZ option to @false@.
    multiAZ :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAvailabilityOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'updateAvailabilityOptions_domainName' - Undocumented member.
--
-- 'multiAZ', 'updateAvailabilityOptions_multiAZ' - You expand an existing search domain to a second Availability Zone by
-- setting the Multi-AZ option to true. Similarly, you can turn off the
-- Multi-AZ option to downgrade the domain to a single Availability Zone by
-- setting the Multi-AZ option to @false@.
newUpdateAvailabilityOptions ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'multiAZ'
  Prelude.Bool ->
  UpdateAvailabilityOptions
newUpdateAvailabilityOptions pDomainName_ pMultiAZ_ =
  UpdateAvailabilityOptions'
    { domainName =
        pDomainName_,
      multiAZ = pMultiAZ_
    }

-- | Undocumented member.
updateAvailabilityOptions_domainName :: Lens.Lens' UpdateAvailabilityOptions Prelude.Text
updateAvailabilityOptions_domainName = Lens.lens (\UpdateAvailabilityOptions' {domainName} -> domainName) (\s@UpdateAvailabilityOptions' {} a -> s {domainName = a} :: UpdateAvailabilityOptions)

-- | You expand an existing search domain to a second Availability Zone by
-- setting the Multi-AZ option to true. Similarly, you can turn off the
-- Multi-AZ option to downgrade the domain to a single Availability Zone by
-- setting the Multi-AZ option to @false@.
updateAvailabilityOptions_multiAZ :: Lens.Lens' UpdateAvailabilityOptions Prelude.Bool
updateAvailabilityOptions_multiAZ = Lens.lens (\UpdateAvailabilityOptions' {multiAZ} -> multiAZ) (\s@UpdateAvailabilityOptions' {} a -> s {multiAZ = a} :: UpdateAvailabilityOptions)

instance Core.AWSRequest UpdateAvailabilityOptions where
  type
    AWSResponse UpdateAvailabilityOptions =
      UpdateAvailabilityOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateAvailabilityOptionsResult"
      ( \s h x ->
          UpdateAvailabilityOptionsResponse'
            Prelude.<$> (x Data..@? "AvailabilityOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAvailabilityOptions where
  hashWithSalt _salt UpdateAvailabilityOptions' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` multiAZ

instance Prelude.NFData UpdateAvailabilityOptions where
  rnf UpdateAvailabilityOptions' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf multiAZ

instance Data.ToHeaders UpdateAvailabilityOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateAvailabilityOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAvailabilityOptions where
  toQuery UpdateAvailabilityOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateAvailabilityOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "MultiAZ" Data.=: multiAZ
      ]

-- | The result of a @UpdateAvailabilityOptions@ request. Contains the status
-- of the domain\'s availability options.
--
-- /See:/ 'newUpdateAvailabilityOptionsResponse' smart constructor.
data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse'
  { -- | The newly-configured availability options. Indicates whether Multi-AZ is
    -- enabled for the domain.
    availabilityOptions :: Prelude.Maybe AvailabilityOptionsStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAvailabilityOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityOptions', 'updateAvailabilityOptionsResponse_availabilityOptions' - The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
--
-- 'httpStatus', 'updateAvailabilityOptionsResponse_httpStatus' - The response's http status code.
newUpdateAvailabilityOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAvailabilityOptionsResponse
newUpdateAvailabilityOptionsResponse pHttpStatus_ =
  UpdateAvailabilityOptionsResponse'
    { availabilityOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
updateAvailabilityOptionsResponse_availabilityOptions :: Lens.Lens' UpdateAvailabilityOptionsResponse (Prelude.Maybe AvailabilityOptionsStatus)
updateAvailabilityOptionsResponse_availabilityOptions = Lens.lens (\UpdateAvailabilityOptionsResponse' {availabilityOptions} -> availabilityOptions) (\s@UpdateAvailabilityOptionsResponse' {} a -> s {availabilityOptions = a} :: UpdateAvailabilityOptionsResponse)

-- | The response's http status code.
updateAvailabilityOptionsResponse_httpStatus :: Lens.Lens' UpdateAvailabilityOptionsResponse Prelude.Int
updateAvailabilityOptionsResponse_httpStatus = Lens.lens (\UpdateAvailabilityOptionsResponse' {httpStatus} -> httpStatus) (\s@UpdateAvailabilityOptionsResponse' {} a -> s {httpStatus = a} :: UpdateAvailabilityOptionsResponse)

instance
  Prelude.NFData
    UpdateAvailabilityOptionsResponse
  where
  rnf UpdateAvailabilityOptionsResponse' {..} =
    Prelude.rnf availabilityOptions
      `Prelude.seq` Prelude.rnf httpStatus
