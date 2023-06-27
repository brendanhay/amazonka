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
-- Module      : Amazonka.Route53Domains.CheckDomainTransferability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether a domain name can be transferred to Amazon Route 53.
module Amazonka.Route53Domains.CheckDomainTransferability
  ( -- * Creating a Request
    CheckDomainTransferability (..),
    newCheckDomainTransferability,

    -- * Request Lenses
    checkDomainTransferability_authCode,
    checkDomainTransferability_domainName,

    -- * Destructuring the Response
    CheckDomainTransferabilityResponse (..),
    newCheckDomainTransferabilityResponse,

    -- * Response Lenses
    checkDomainTransferabilityResponse_transferability,
    checkDomainTransferabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The CheckDomainTransferability request contains the following elements.
--
-- /See:/ 'newCheckDomainTransferability' smart constructor.
data CheckDomainTransferability = CheckDomainTransferability'
  { -- | If the registrar for the top-level domain (TLD) requires an
    -- authorization code to transfer the domain, the code that you got from
    -- the current registrar for the domain.
    authCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the domain that you want to transfer to Route 53. The
    -- top-level domain (TLD), such as .com, must be a TLD that Route 53
    -- supports. For a list of supported TLDs, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- The domain name can contain only the following characters:
    --
    -- -   Letters a through z. Domain names are not case sensitive.
    --
    -- -   Numbers 0 through 9.
    --
    -- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
    --     label.
    --
    -- -   Period (.) to separate the labels in the name, such as the @.@ in
    --     @example.com@.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckDomainTransferability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authCode', 'checkDomainTransferability_authCode' - If the registrar for the top-level domain (TLD) requires an
-- authorization code to transfer the domain, the code that you got from
-- the current registrar for the domain.
--
-- 'domainName', 'checkDomainTransferability_domainName' - The name of the domain that you want to transfer to Route 53. The
-- top-level domain (TLD), such as .com, must be a TLD that Route 53
-- supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
newCheckDomainTransferability ::
  -- | 'domainName'
  Prelude.Text ->
  CheckDomainTransferability
newCheckDomainTransferability pDomainName_ =
  CheckDomainTransferability'
    { authCode =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | If the registrar for the top-level domain (TLD) requires an
-- authorization code to transfer the domain, the code that you got from
-- the current registrar for the domain.
checkDomainTransferability_authCode :: Lens.Lens' CheckDomainTransferability (Prelude.Maybe Prelude.Text)
checkDomainTransferability_authCode = Lens.lens (\CheckDomainTransferability' {authCode} -> authCode) (\s@CheckDomainTransferability' {} a -> s {authCode = a} :: CheckDomainTransferability) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the domain that you want to transfer to Route 53. The
-- top-level domain (TLD), such as .com, must be a TLD that Route 53
-- supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
checkDomainTransferability_domainName :: Lens.Lens' CheckDomainTransferability Prelude.Text
checkDomainTransferability_domainName = Lens.lens (\CheckDomainTransferability' {domainName} -> domainName) (\s@CheckDomainTransferability' {} a -> s {domainName = a} :: CheckDomainTransferability)

instance Core.AWSRequest CheckDomainTransferability where
  type
    AWSResponse CheckDomainTransferability =
      CheckDomainTransferabilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckDomainTransferabilityResponse'
            Prelude.<$> (x Data..?> "Transferability")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckDomainTransferability where
  hashWithSalt _salt CheckDomainTransferability' {..} =
    _salt
      `Prelude.hashWithSalt` authCode
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CheckDomainTransferability where
  rnf CheckDomainTransferability' {..} =
    Prelude.rnf authCode
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CheckDomainTransferability where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.CheckDomainTransferability" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CheckDomainTransferability where
  toJSON CheckDomainTransferability' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthCode" Data..=) Prelude.<$> authCode,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath CheckDomainTransferability where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckDomainTransferability where
  toQuery = Prelude.const Prelude.mempty

-- | The CheckDomainTransferability response includes the following elements.
--
-- /See:/ 'newCheckDomainTransferabilityResponse' smart constructor.
data CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse'
  { -- | A complex type that contains information about whether the specified
    -- domain can be transferred to Route 53.
    transferability :: Prelude.Maybe DomainTransferability,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckDomainTransferabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferability', 'checkDomainTransferabilityResponse_transferability' - A complex type that contains information about whether the specified
-- domain can be transferred to Route 53.
--
-- 'httpStatus', 'checkDomainTransferabilityResponse_httpStatus' - The response's http status code.
newCheckDomainTransferabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckDomainTransferabilityResponse
newCheckDomainTransferabilityResponse pHttpStatus_ =
  CheckDomainTransferabilityResponse'
    { transferability =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about whether the specified
-- domain can be transferred to Route 53.
checkDomainTransferabilityResponse_transferability :: Lens.Lens' CheckDomainTransferabilityResponse (Prelude.Maybe DomainTransferability)
checkDomainTransferabilityResponse_transferability = Lens.lens (\CheckDomainTransferabilityResponse' {transferability} -> transferability) (\s@CheckDomainTransferabilityResponse' {} a -> s {transferability = a} :: CheckDomainTransferabilityResponse)

-- | The response's http status code.
checkDomainTransferabilityResponse_httpStatus :: Lens.Lens' CheckDomainTransferabilityResponse Prelude.Int
checkDomainTransferabilityResponse_httpStatus = Lens.lens (\CheckDomainTransferabilityResponse' {httpStatus} -> httpStatus) (\s@CheckDomainTransferabilityResponse' {} a -> s {httpStatus = a} :: CheckDomainTransferabilityResponse)

instance
  Prelude.NFData
    CheckDomainTransferabilityResponse
  where
  rnf CheckDomainTransferabilityResponse' {..} =
    Prelude.rnf transferability
      `Prelude.seq` Prelude.rnf httpStatus
