{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkLink.Types.WebsiteAuthorizationProviderSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.WebsiteAuthorizationProviderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkLink.Types.AuthorizationProviderType

-- | The summary of the website authorization provider.
--
-- /See:/ 'newWebsiteAuthorizationProviderSummary' smart constructor.
data WebsiteAuthorizationProviderSummary = WebsiteAuthorizationProviderSummary'
  { -- | A unique identifier for the authorization provider.
    authorizationProviderId :: Prelude.Maybe Prelude.Text,
    -- | The time of creation.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The domain name of the authorization provider. This applies only to
    -- SAML-based authorization providers.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The authorization provider type.
    authorizationProviderType :: AuthorizationProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebsiteAuthorizationProviderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationProviderId', 'websiteAuthorizationProviderSummary_authorizationProviderId' - A unique identifier for the authorization provider.
--
-- 'createdTime', 'websiteAuthorizationProviderSummary_createdTime' - The time of creation.
--
-- 'domainName', 'websiteAuthorizationProviderSummary_domainName' - The domain name of the authorization provider. This applies only to
-- SAML-based authorization providers.
--
-- 'authorizationProviderType', 'websiteAuthorizationProviderSummary_authorizationProviderType' - The authorization provider type.
newWebsiteAuthorizationProviderSummary ::
  -- | 'authorizationProviderType'
  AuthorizationProviderType ->
  WebsiteAuthorizationProviderSummary
newWebsiteAuthorizationProviderSummary
  pAuthorizationProviderType_ =
    WebsiteAuthorizationProviderSummary'
      { authorizationProviderId =
          Prelude.Nothing,
        createdTime = Prelude.Nothing,
        domainName = Prelude.Nothing,
        authorizationProviderType =
          pAuthorizationProviderType_
      }

-- | A unique identifier for the authorization provider.
websiteAuthorizationProviderSummary_authorizationProviderId :: Lens.Lens' WebsiteAuthorizationProviderSummary (Prelude.Maybe Prelude.Text)
websiteAuthorizationProviderSummary_authorizationProviderId = Lens.lens (\WebsiteAuthorizationProviderSummary' {authorizationProviderId} -> authorizationProviderId) (\s@WebsiteAuthorizationProviderSummary' {} a -> s {authorizationProviderId = a} :: WebsiteAuthorizationProviderSummary)

-- | The time of creation.
websiteAuthorizationProviderSummary_createdTime :: Lens.Lens' WebsiteAuthorizationProviderSummary (Prelude.Maybe Prelude.UTCTime)
websiteAuthorizationProviderSummary_createdTime = Lens.lens (\WebsiteAuthorizationProviderSummary' {createdTime} -> createdTime) (\s@WebsiteAuthorizationProviderSummary' {} a -> s {createdTime = a} :: WebsiteAuthorizationProviderSummary) Prelude.. Lens.mapping Core._Time

-- | The domain name of the authorization provider. This applies only to
-- SAML-based authorization providers.
websiteAuthorizationProviderSummary_domainName :: Lens.Lens' WebsiteAuthorizationProviderSummary (Prelude.Maybe Prelude.Text)
websiteAuthorizationProviderSummary_domainName = Lens.lens (\WebsiteAuthorizationProviderSummary' {domainName} -> domainName) (\s@WebsiteAuthorizationProviderSummary' {} a -> s {domainName = a} :: WebsiteAuthorizationProviderSummary)

-- | The authorization provider type.
websiteAuthorizationProviderSummary_authorizationProviderType :: Lens.Lens' WebsiteAuthorizationProviderSummary AuthorizationProviderType
websiteAuthorizationProviderSummary_authorizationProviderType = Lens.lens (\WebsiteAuthorizationProviderSummary' {authorizationProviderType} -> authorizationProviderType) (\s@WebsiteAuthorizationProviderSummary' {} a -> s {authorizationProviderType = a} :: WebsiteAuthorizationProviderSummary)

instance
  Core.FromJSON
    WebsiteAuthorizationProviderSummary
  where
  parseJSON =
    Core.withObject
      "WebsiteAuthorizationProviderSummary"
      ( \x ->
          WebsiteAuthorizationProviderSummary'
            Prelude.<$> (x Core..:? "AuthorizationProviderId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..: "AuthorizationProviderType")
      )

instance
  Prelude.Hashable
    WebsiteAuthorizationProviderSummary

instance
  Prelude.NFData
    WebsiteAuthorizationProviderSummary
