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
-- Module      : Amazonka.Lightsail.Types.AccessKeyLastUsed
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AccessKeyLastUsed where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the last time an access key was used.
--
-- This object does not include data in the response of a
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
-- action.
--
-- /See:/ 'newAccessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { -- | The date and time when the access key was most recently used.
    --
    -- This value is null if the access key has not been used.
    lastUsedDate :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services Region where this access key was most recently
    -- used.
    --
    -- This value is @N\/A@ if the access key has not been used.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services service with which this access key
    -- was most recently used.
    --
    -- This value is @N\/A@ if the access key has not been used.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKeyLastUsed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUsedDate', 'accessKeyLastUsed_lastUsedDate' - The date and time when the access key was most recently used.
--
-- This value is null if the access key has not been used.
--
-- 'region', 'accessKeyLastUsed_region' - The Amazon Web Services Region where this access key was most recently
-- used.
--
-- This value is @N\/A@ if the access key has not been used.
--
-- 'serviceName', 'accessKeyLastUsed_serviceName' - The name of the Amazon Web Services service with which this access key
-- was most recently used.
--
-- This value is @N\/A@ if the access key has not been used.
newAccessKeyLastUsed ::
  AccessKeyLastUsed
newAccessKeyLastUsed =
  AccessKeyLastUsed'
    { lastUsedDate = Prelude.Nothing,
      region = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The date and time when the access key was most recently used.
--
-- This value is null if the access key has not been used.
accessKeyLastUsed_lastUsedDate :: Lens.Lens' AccessKeyLastUsed (Prelude.Maybe Prelude.UTCTime)
accessKeyLastUsed_lastUsedDate = Lens.lens (\AccessKeyLastUsed' {lastUsedDate} -> lastUsedDate) (\s@AccessKeyLastUsed' {} a -> s {lastUsedDate = a} :: AccessKeyLastUsed) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services Region where this access key was most recently
-- used.
--
-- This value is @N\/A@ if the access key has not been used.
accessKeyLastUsed_region :: Lens.Lens' AccessKeyLastUsed (Prelude.Maybe Prelude.Text)
accessKeyLastUsed_region = Lens.lens (\AccessKeyLastUsed' {region} -> region) (\s@AccessKeyLastUsed' {} a -> s {region = a} :: AccessKeyLastUsed)

-- | The name of the Amazon Web Services service with which this access key
-- was most recently used.
--
-- This value is @N\/A@ if the access key has not been used.
accessKeyLastUsed_serviceName :: Lens.Lens' AccessKeyLastUsed (Prelude.Maybe Prelude.Text)
accessKeyLastUsed_serviceName = Lens.lens (\AccessKeyLastUsed' {serviceName} -> serviceName) (\s@AccessKeyLastUsed' {} a -> s {serviceName = a} :: AccessKeyLastUsed)

instance Core.FromJSON AccessKeyLastUsed where
  parseJSON =
    Core.withObject
      "AccessKeyLastUsed"
      ( \x ->
          AccessKeyLastUsed'
            Prelude.<$> (x Core..:? "lastUsedDate")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "serviceName")
      )

instance Prelude.Hashable AccessKeyLastUsed where
  hashWithSalt _salt AccessKeyLastUsed' {..} =
    _salt `Prelude.hashWithSalt` lastUsedDate
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData AccessKeyLastUsed where
  rnf AccessKeyLastUsed' {..} =
    Prelude.rnf lastUsedDate
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf serviceName
