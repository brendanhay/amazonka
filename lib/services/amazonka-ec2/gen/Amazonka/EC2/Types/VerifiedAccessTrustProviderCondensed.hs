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
-- Module      : Amazonka.EC2.Types.VerifiedAccessTrustProviderCondensed
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessTrustProviderCondensed where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DeviceTrustProviderType
import Amazonka.EC2.Types.TrustProviderType
import Amazonka.EC2.Types.UserTrustProviderType
import qualified Amazonka.Prelude as Prelude

-- | Condensed information about a trust provider.
--
-- /See:/ 'newVerifiedAccessTrustProviderCondensed' smart constructor.
data VerifiedAccessTrustProviderCondensed = VerifiedAccessTrustProviderCondensed'
  { -- | The description of trust provider.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of device-based trust provider.
    deviceTrustProviderType :: Prelude.Maybe DeviceTrustProviderType,
    -- | The type of trust provider (user- or device-based).
    trustProviderType :: Prelude.Maybe TrustProviderType,
    -- | The type of user-based trust provider.
    userTrustProviderType :: Prelude.Maybe UserTrustProviderType,
    -- | The ID of the trust provider.
    verifiedAccessTrustProviderId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessTrustProviderCondensed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'verifiedAccessTrustProviderCondensed_description' - The description of trust provider.
--
-- 'deviceTrustProviderType', 'verifiedAccessTrustProviderCondensed_deviceTrustProviderType' - The type of device-based trust provider.
--
-- 'trustProviderType', 'verifiedAccessTrustProviderCondensed_trustProviderType' - The type of trust provider (user- or device-based).
--
-- 'userTrustProviderType', 'verifiedAccessTrustProviderCondensed_userTrustProviderType' - The type of user-based trust provider.
--
-- 'verifiedAccessTrustProviderId', 'verifiedAccessTrustProviderCondensed_verifiedAccessTrustProviderId' - The ID of the trust provider.
newVerifiedAccessTrustProviderCondensed ::
  VerifiedAccessTrustProviderCondensed
newVerifiedAccessTrustProviderCondensed =
  VerifiedAccessTrustProviderCondensed'
    { description =
        Prelude.Nothing,
      deviceTrustProviderType =
        Prelude.Nothing,
      trustProviderType = Prelude.Nothing,
      userTrustProviderType =
        Prelude.Nothing,
      verifiedAccessTrustProviderId =
        Prelude.Nothing
    }

-- | The description of trust provider.
verifiedAccessTrustProviderCondensed_description :: Lens.Lens' VerifiedAccessTrustProviderCondensed (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProviderCondensed_description = Lens.lens (\VerifiedAccessTrustProviderCondensed' {description} -> description) (\s@VerifiedAccessTrustProviderCondensed' {} a -> s {description = a} :: VerifiedAccessTrustProviderCondensed)

-- | The type of device-based trust provider.
verifiedAccessTrustProviderCondensed_deviceTrustProviderType :: Lens.Lens' VerifiedAccessTrustProviderCondensed (Prelude.Maybe DeviceTrustProviderType)
verifiedAccessTrustProviderCondensed_deviceTrustProviderType = Lens.lens (\VerifiedAccessTrustProviderCondensed' {deviceTrustProviderType} -> deviceTrustProviderType) (\s@VerifiedAccessTrustProviderCondensed' {} a -> s {deviceTrustProviderType = a} :: VerifiedAccessTrustProviderCondensed)

-- | The type of trust provider (user- or device-based).
verifiedAccessTrustProviderCondensed_trustProviderType :: Lens.Lens' VerifiedAccessTrustProviderCondensed (Prelude.Maybe TrustProviderType)
verifiedAccessTrustProviderCondensed_trustProviderType = Lens.lens (\VerifiedAccessTrustProviderCondensed' {trustProviderType} -> trustProviderType) (\s@VerifiedAccessTrustProviderCondensed' {} a -> s {trustProviderType = a} :: VerifiedAccessTrustProviderCondensed)

-- | The type of user-based trust provider.
verifiedAccessTrustProviderCondensed_userTrustProviderType :: Lens.Lens' VerifiedAccessTrustProviderCondensed (Prelude.Maybe UserTrustProviderType)
verifiedAccessTrustProviderCondensed_userTrustProviderType = Lens.lens (\VerifiedAccessTrustProviderCondensed' {userTrustProviderType} -> userTrustProviderType) (\s@VerifiedAccessTrustProviderCondensed' {} a -> s {userTrustProviderType = a} :: VerifiedAccessTrustProviderCondensed)

-- | The ID of the trust provider.
verifiedAccessTrustProviderCondensed_verifiedAccessTrustProviderId :: Lens.Lens' VerifiedAccessTrustProviderCondensed (Prelude.Maybe Prelude.Text)
verifiedAccessTrustProviderCondensed_verifiedAccessTrustProviderId = Lens.lens (\VerifiedAccessTrustProviderCondensed' {verifiedAccessTrustProviderId} -> verifiedAccessTrustProviderId) (\s@VerifiedAccessTrustProviderCondensed' {} a -> s {verifiedAccessTrustProviderId = a} :: VerifiedAccessTrustProviderCondensed)

instance
  Data.FromXML
    VerifiedAccessTrustProviderCondensed
  where
  parseXML x =
    VerifiedAccessTrustProviderCondensed'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "deviceTrustProviderType")
      Prelude.<*> (x Data..@? "trustProviderType")
      Prelude.<*> (x Data..@? "userTrustProviderType")
      Prelude.<*> (x Data..@? "verifiedAccessTrustProviderId")

instance
  Prelude.Hashable
    VerifiedAccessTrustProviderCondensed
  where
  hashWithSalt
    _salt
    VerifiedAccessTrustProviderCondensed' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` deviceTrustProviderType
        `Prelude.hashWithSalt` trustProviderType
        `Prelude.hashWithSalt` userTrustProviderType
        `Prelude.hashWithSalt` verifiedAccessTrustProviderId

instance
  Prelude.NFData
    VerifiedAccessTrustProviderCondensed
  where
  rnf VerifiedAccessTrustProviderCondensed' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceTrustProviderType
      `Prelude.seq` Prelude.rnf trustProviderType
      `Prelude.seq` Prelude.rnf userTrustProviderType
      `Prelude.seq` Prelude.rnf verifiedAccessTrustProviderId
