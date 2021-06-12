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
-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation checks the availability of one domain name. Note that if
-- the availability status of a domain is pending, you must submit another
-- request to determine the availability of the domain name.
module Network.AWS.Route53Domains.CheckDomainAvailability
  ( -- * Creating a Request
    CheckDomainAvailability (..),
    newCheckDomainAvailability,

    -- * Request Lenses
    checkDomainAvailability_idnLangCode,
    checkDomainAvailability_domainName,

    -- * Destructuring the Response
    CheckDomainAvailabilityResponse (..),
    newCheckDomainAvailabilityResponse,

    -- * Response Lenses
    checkDomainAvailabilityResponse_httpStatus,
    checkDomainAvailabilityResponse_availability,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The CheckDomainAvailability request contains the following elements.
--
-- /See:/ 'newCheckDomainAvailability' smart constructor.
data CheckDomainAvailability = CheckDomainAvailability'
  { -- | Reserved for future use.
    idnLangCode :: Core.Maybe Core.Text,
    -- | The name of the domain that you want to get availability for. The
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
    --
    -- Internationalized domain names are not supported for some top-level
    -- domains. To determine whether the TLD that you want to use supports
    -- internationalized domain names, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CheckDomainAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idnLangCode', 'checkDomainAvailability_idnLangCode' - Reserved for future use.
--
-- 'domainName', 'checkDomainAvailability_domainName' - The name of the domain that you want to get availability for. The
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
--
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
newCheckDomainAvailability ::
  -- | 'domainName'
  Core.Text ->
  CheckDomainAvailability
newCheckDomainAvailability pDomainName_ =
  CheckDomainAvailability'
    { idnLangCode =
        Core.Nothing,
      domainName = pDomainName_
    }

-- | Reserved for future use.
checkDomainAvailability_idnLangCode :: Lens.Lens' CheckDomainAvailability (Core.Maybe Core.Text)
checkDomainAvailability_idnLangCode = Lens.lens (\CheckDomainAvailability' {idnLangCode} -> idnLangCode) (\s@CheckDomainAvailability' {} a -> s {idnLangCode = a} :: CheckDomainAvailability)

-- | The name of the domain that you want to get availability for. The
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
--
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names>.
checkDomainAvailability_domainName :: Lens.Lens' CheckDomainAvailability Core.Text
checkDomainAvailability_domainName = Lens.lens (\CheckDomainAvailability' {domainName} -> domainName) (\s@CheckDomainAvailability' {} a -> s {domainName = a} :: CheckDomainAvailability)

instance Core.AWSRequest CheckDomainAvailability where
  type
    AWSResponse CheckDomainAvailability =
      CheckDomainAvailabilityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckDomainAvailabilityResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Availability")
      )

instance Core.Hashable CheckDomainAvailability

instance Core.NFData CheckDomainAvailability

instance Core.ToHeaders CheckDomainAvailability where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.CheckDomainAvailability" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CheckDomainAvailability where
  toJSON CheckDomainAvailability' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IdnLangCode" Core..=) Core.<$> idnLangCode,
            Core.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath CheckDomainAvailability where
  toPath = Core.const "/"

instance Core.ToQuery CheckDomainAvailability where
  toQuery = Core.const Core.mempty

-- | The CheckDomainAvailability response includes the following elements.
--
-- /See:/ 'newCheckDomainAvailabilityResponse' smart constructor.
data CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Whether the domain name is available for registering.
    --
    -- You can register only domains designated as @AVAILABLE@.
    --
    -- Valid values:
    --
    -- [AVAILABLE]
    --     The domain name is available.
    --
    -- [AVAILABLE_RESERVED]
    --     The domain name is reserved under specific conditions.
    --
    -- [AVAILABLE_PREORDER]
    --     The domain name is available and can be preordered.
    --
    -- [DONT_KNOW]
    --     The TLD registry didn\'t reply with a definitive answer about
    --     whether the domain name is available. Route 53 can return this
    --     response for a variety of reasons, for example, the registry is
    --     performing maintenance. Try again later.
    --
    -- [PENDING]
    --     The TLD registry didn\'t return a response in the expected amount of
    --     time. When the response is delayed, it usually takes just a few
    --     extra seconds. You can resubmit the request immediately.
    --
    -- [RESERVED]
    --     The domain name has been reserved for another person or
    --     organization.
    --
    -- [UNAVAILABLE]
    --     The domain name is not available.
    --
    -- [UNAVAILABLE_PREMIUM]
    --     The domain name is not available.
    --
    -- [UNAVAILABLE_RESTRICTED]
    --     The domain name is forbidden.
    availability :: DomainAvailability
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CheckDomainAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'checkDomainAvailabilityResponse_httpStatus' - The response's http status code.
--
-- 'availability', 'checkDomainAvailabilityResponse_availability' - Whether the domain name is available for registering.
--
-- You can register only domains designated as @AVAILABLE@.
--
-- Valid values:
--
-- [AVAILABLE]
--     The domain name is available.
--
-- [AVAILABLE_RESERVED]
--     The domain name is reserved under specific conditions.
--
-- [AVAILABLE_PREORDER]
--     The domain name is available and can be preordered.
--
-- [DONT_KNOW]
--     The TLD registry didn\'t reply with a definitive answer about
--     whether the domain name is available. Route 53 can return this
--     response for a variety of reasons, for example, the registry is
--     performing maintenance. Try again later.
--
-- [PENDING]
--     The TLD registry didn\'t return a response in the expected amount of
--     time. When the response is delayed, it usually takes just a few
--     extra seconds. You can resubmit the request immediately.
--
-- [RESERVED]
--     The domain name has been reserved for another person or
--     organization.
--
-- [UNAVAILABLE]
--     The domain name is not available.
--
-- [UNAVAILABLE_PREMIUM]
--     The domain name is not available.
--
-- [UNAVAILABLE_RESTRICTED]
--     The domain name is forbidden.
newCheckDomainAvailabilityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'availability'
  DomainAvailability ->
  CheckDomainAvailabilityResponse
newCheckDomainAvailabilityResponse
  pHttpStatus_
  pAvailability_ =
    CheckDomainAvailabilityResponse'
      { httpStatus =
          pHttpStatus_,
        availability = pAvailability_
      }

-- | The response's http status code.
checkDomainAvailabilityResponse_httpStatus :: Lens.Lens' CheckDomainAvailabilityResponse Core.Int
checkDomainAvailabilityResponse_httpStatus = Lens.lens (\CheckDomainAvailabilityResponse' {httpStatus} -> httpStatus) (\s@CheckDomainAvailabilityResponse' {} a -> s {httpStatus = a} :: CheckDomainAvailabilityResponse)

-- | Whether the domain name is available for registering.
--
-- You can register only domains designated as @AVAILABLE@.
--
-- Valid values:
--
-- [AVAILABLE]
--     The domain name is available.
--
-- [AVAILABLE_RESERVED]
--     The domain name is reserved under specific conditions.
--
-- [AVAILABLE_PREORDER]
--     The domain name is available and can be preordered.
--
-- [DONT_KNOW]
--     The TLD registry didn\'t reply with a definitive answer about
--     whether the domain name is available. Route 53 can return this
--     response for a variety of reasons, for example, the registry is
--     performing maintenance. Try again later.
--
-- [PENDING]
--     The TLD registry didn\'t return a response in the expected amount of
--     time. When the response is delayed, it usually takes just a few
--     extra seconds. You can resubmit the request immediately.
--
-- [RESERVED]
--     The domain name has been reserved for another person or
--     organization.
--
-- [UNAVAILABLE]
--     The domain name is not available.
--
-- [UNAVAILABLE_PREMIUM]
--     The domain name is not available.
--
-- [UNAVAILABLE_RESTRICTED]
--     The domain name is forbidden.
checkDomainAvailabilityResponse_availability :: Lens.Lens' CheckDomainAvailabilityResponse DomainAvailability
checkDomainAvailabilityResponse_availability = Lens.lens (\CheckDomainAvailabilityResponse' {availability} -> availability) (\s@CheckDomainAvailabilityResponse' {} a -> s {availability = a} :: CheckDomainAvailabilityResponse)

instance Core.NFData CheckDomainAvailabilityResponse
