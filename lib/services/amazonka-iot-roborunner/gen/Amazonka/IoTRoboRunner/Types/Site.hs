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
-- Module      : Amazonka.IoTRoboRunner.Types.Site
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.Site where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Facility containing destinations, workers, activities, and tasks.
--
-- /See:/ 'newSite' smart constructor.
data Site = Site'
  { arn :: Prelude.Text,
    -- | The name of the site. Mutable after creation and unique within a given
    -- account.
    name :: Prelude.Text,
    countryCode :: Prelude.Text,
    createdAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Site' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'site_arn' - Undocumented member.
--
-- 'name', 'site_name' - The name of the site. Mutable after creation and unique within a given
-- account.
--
-- 'countryCode', 'site_countryCode' - Undocumented member.
--
-- 'createdAt', 'site_createdAt' - Undocumented member.
newSite ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'countryCode'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  Site
newSite pArn_ pName_ pCountryCode_ pCreatedAt_ =
  Site'
    { arn = pArn_,
      name = pName_,
      countryCode = pCountryCode_,
      createdAt = Data._Time Lens.# pCreatedAt_
    }

-- | Undocumented member.
site_arn :: Lens.Lens' Site Prelude.Text
site_arn = Lens.lens (\Site' {arn} -> arn) (\s@Site' {} a -> s {arn = a} :: Site)

-- | The name of the site. Mutable after creation and unique within a given
-- account.
site_name :: Lens.Lens' Site Prelude.Text
site_name = Lens.lens (\Site' {name} -> name) (\s@Site' {} a -> s {name = a} :: Site)

-- | Undocumented member.
site_countryCode :: Lens.Lens' Site Prelude.Text
site_countryCode = Lens.lens (\Site' {countryCode} -> countryCode) (\s@Site' {} a -> s {countryCode = a} :: Site)

-- | Undocumented member.
site_createdAt :: Lens.Lens' Site Prelude.UTCTime
site_createdAt = Lens.lens (\Site' {createdAt} -> createdAt) (\s@Site' {} a -> s {createdAt = a} :: Site) Prelude.. Data._Time

instance Data.FromJSON Site where
  parseJSON =
    Data.withObject
      "Site"
      ( \x ->
          Site'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "countryCode")
            Prelude.<*> (x Data..: "createdAt")
      )

instance Prelude.Hashable Site where
  hashWithSalt _salt Site' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Site where
  rnf Site' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf createdAt
