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
-- Module      : Amazonka.EC2.Types.VerifiedAccessTrustProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessTrustProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DeviceOptions
import Amazonka.EC2.Types.DeviceTrustProviderType
import Amazonka.EC2.Types.OidcOptions
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TrustProviderType
import Amazonka.EC2.Types.UserTrustProviderType
import qualified Amazonka.Prelude as Prelude

-- | Describes a Verified Access trust provider.
--
-- /See:/ 'newVerifiedAccessTrustProvider' smart constructor.
data VerifiedAccessTrustProvider = VerifiedAccessTrustProvider'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access trust
    -- provider.
    description :: Prelude.Maybe Prelude.Text,
    -- | The options for device-identity type trust provider.
    deviceOptions :: Prelude.Maybe DeviceOptions,
    -- | The type of device-based trust provider.
    deviceTrustProviderType :: Prelude.Maybe DeviceTrustProviderType,
    -- | The last updated time.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The OpenID Connect details for an @oidc@-type, user-identity based trust
    -- provider.
    oidcOptions :: Prelude.Maybe OidcOptions,
    -- | The identifier to be used when working with policy rules.
    policyReferenceName :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The type of Verified Access trust provider.
    trustProviderType :: Prelude.Maybe TrustProviderType,
    -- | The type of user-based trust provider.
    userTrustProviderType :: Prelude.Maybe UserTrustProviderType,
    -- | The ID of the Amazon Web Services Verified Access trust provider.
    verifiedAccessTrustProviderId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessTrustProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'verifiedAccessTrustProvider_creationTime' - The creation time.
--
-- 'description', 'verifiedAccessTrustProvider_description' - A description for the Amazon Web Services Verified Access trust
-- provider.
--
-- 'deviceOptions', 'verifiedAccessTrustProvider_deviceOptions' - The options for device-identity type trust provider.
--
-- 'deviceTrustProviderType', 'verifiedAccessTrustProvider_deviceTrustProviderType' - The type of device-based trust provider.
--
-- 'lastUpdatedTime', 'verifiedAccessTrustProvider_lastUpdatedTime' - The last updated time.
--
-- 'oidcOptions', 'verifiedAccessTrustProvider_oidcOptions' - The OpenID Connect details for an @oidc@-type, user-identity based trust
-- provider.
--
-- 'policyReferenceName', 'verifiedAccessTrustProvider_policyReferenceName' - The identifier to be used when working with policy rules.
--
-- 'tags', 'verifiedAccessTrustProvider_tags' - The tags.
--
-- 'trustProviderType', 'verifiedAccessTrustProvider_trustProviderType' - The type of Verified Access trust provider.
--
-- 'userTrustProviderType', 'verifiedAccessTrustProvider_userTrustProviderType' - The type of user-based trust provider.
--
-- 'verifiedAccessTrustProviderId', 'verifiedAccessTrustProvider_verifiedAccessTrustProviderId' - The ID of the Amazon Web Services Verified Access trust provider.
newVerifiedAccessTrustProvider ::
  VerifiedAccessTrustProvider
newVerifiedAccessTrustProvider =
  VerifiedAccessTrustProvider'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      deviceOptions = Prelude.Nothing,
      deviceTrustProviderType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      oidcOptions = Prelude.Nothing,
      policyReferenceName = Prelude.Nothing,
      tags = Prelude.Nothing,
      trustProviderType = Prelude.Nothing,
      userTrustProviderType = Prelude.Nothing,
      verifiedAccessTrustProviderId =
        Prelude.Nothing
    }

-- | The creation time.
verifiedAccessTrustProvider_creationTime :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProvider_creationTime = Lens.lens (\VerifiedAccessTrustProvider' {creationTime} -> creationTime) (\s@VerifiedAccessTrustProvider' {} a -> s {creationTime = a} :: VerifiedAccessTrustProvider)

-- | A description for the Amazon Web Services Verified Access trust
-- provider.
verifiedAccessTrustProvider_description :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProvider_description = Lens.lens (\VerifiedAccessTrustProvider' {description} -> description) (\s@VerifiedAccessTrustProvider' {} a -> s {description = a} :: VerifiedAccessTrustProvider)

-- | The options for device-identity type trust provider.
verifiedAccessTrustProvider_deviceOptions :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe DeviceOptions)
verifiedAccessTrustProvider_deviceOptions = Lens.lens (\VerifiedAccessTrustProvider' {deviceOptions} -> deviceOptions) (\s@VerifiedAccessTrustProvider' {} a -> s {deviceOptions = a} :: VerifiedAccessTrustProvider)

-- | The type of device-based trust provider.
verifiedAccessTrustProvider_deviceTrustProviderType :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe DeviceTrustProviderType)
verifiedAccessTrustProvider_deviceTrustProviderType = Lens.lens (\VerifiedAccessTrustProvider' {deviceTrustProviderType} -> deviceTrustProviderType) (\s@VerifiedAccessTrustProvider' {} a -> s {deviceTrustProviderType = a} :: VerifiedAccessTrustProvider)

-- | The last updated time.
verifiedAccessTrustProvider_lastUpdatedTime :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProvider_lastUpdatedTime = Lens.lens (\VerifiedAccessTrustProvider' {lastUpdatedTime} -> lastUpdatedTime) (\s@VerifiedAccessTrustProvider' {} a -> s {lastUpdatedTime = a} :: VerifiedAccessTrustProvider)

-- | The OpenID Connect details for an @oidc@-type, user-identity based trust
-- provider.
verifiedAccessTrustProvider_oidcOptions :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe OidcOptions)
verifiedAccessTrustProvider_oidcOptions = Lens.lens (\VerifiedAccessTrustProvider' {oidcOptions} -> oidcOptions) (\s@VerifiedAccessTrustProvider' {} a -> s {oidcOptions = a} :: VerifiedAccessTrustProvider)

-- | The identifier to be used when working with policy rules.
verifiedAccessTrustProvider_policyReferenceName :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProvider_policyReferenceName = Lens.lens (\VerifiedAccessTrustProvider' {policyReferenceName} -> policyReferenceName) (\s@VerifiedAccessTrustProvider' {} a -> s {policyReferenceName = a} :: VerifiedAccessTrustProvider)

-- | The tags.
verifiedAccessTrustProvider_tags :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe [Tag])
verifiedAccessTrustProvider_tags = Lens.lens (\VerifiedAccessTrustProvider' {tags} -> tags) (\s@VerifiedAccessTrustProvider' {} a -> s {tags = a} :: VerifiedAccessTrustProvider) Prelude.. Lens.mapping Lens.coerced

-- | The type of Verified Access trust provider.
verifiedAccessTrustProvider_trustProviderType :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe TrustProviderType)
verifiedAccessTrustProvider_trustProviderType = Lens.lens (\VerifiedAccessTrustProvider' {trustProviderType} -> trustProviderType) (\s@VerifiedAccessTrustProvider' {} a -> s {trustProviderType = a} :: VerifiedAccessTrustProvider)

-- | The type of user-based trust provider.
verifiedAccessTrustProvider_userTrustProviderType :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe UserTrustProviderType)
verifiedAccessTrustProvider_userTrustProviderType = Lens.lens (\VerifiedAccessTrustProvider' {userTrustProviderType} -> userTrustProviderType) (\s@VerifiedAccessTrustProvider' {} a -> s {userTrustProviderType = a} :: VerifiedAccessTrustProvider)

-- | The ID of the Amazon Web Services Verified Access trust provider.
verifiedAccessTrustProvider_verifiedAccessTrustProviderId :: Lens.Lens' VerifiedAccessTrustProvider (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProvider_verifiedAccessTrustProviderId = Lens.lens (\VerifiedAccessTrustProvider' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@VerifiedAccessTrustProvider' {} a -> s {verifiedAccessTrustProviderId = a} :: VerifiedAccessTrustProvider)

instance Data.FromXML VerifiedAccessTrustProvider where
  parseXML x =
    VerifiedAccessTrustProvider'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "deviceOptions")
      Prelude.<*> (x Data..@? "deviceTrustProviderType")
      Prelude.<*> (x Data..@? "lastUpdatedTime")
      Prelude.<*> (x Data..@? "oidcOptions")
      Prelude.<*> (x Data..@? "policyReferenceName")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "trustProviderType")
      Prelude.<*> (x Data..@? "userTrustProviderType")
      Prelude.<*> (x Data..@? "verifiedAccessTrustProviderId")

instance Prelude.Hashable VerifiedAccessTrustProvider where
  hashWithSalt _salt VerifiedAccessTrustProvider' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceOptions
      `Prelude.hashWithSalt` deviceTrustProviderType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` oidcOptions
      `Prelude.hashWithSalt` policyReferenceName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trustProviderType
      `Prelude.hashWithSalt` userTrustProviderType
      `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance Prelude.NFData VerifiedAccessTrustProvider where
  rnf VerifiedAccessTrustProvider' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceOptions
      `Prelude.seq` Prelude.rnf deviceTrustProviderType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf oidcOptions
      `Prelude.seq` Prelude.rnf policyReferenceName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trustProviderType
      `Prelude.seq` Prelude.rnf userTrustProviderType
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderId
