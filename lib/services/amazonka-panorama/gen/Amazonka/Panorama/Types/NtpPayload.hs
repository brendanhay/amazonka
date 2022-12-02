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
-- Module      : Amazonka.Panorama.Types.NtpPayload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NtpPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network time protocol (NTP) server settings. Use this option to connect
-- to local NTP servers instead of @pool.ntp.org@.
--
-- /See:/ 'newNtpPayload' smart constructor.
data NtpPayload = NtpPayload'
  { -- | NTP servers to use, in order of preference.
    ntpServers :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NtpPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ntpServers', 'ntpPayload_ntpServers' - NTP servers to use, in order of preference.
newNtpPayload ::
  NtpPayload
newNtpPayload =
  NtpPayload' {ntpServers = Prelude.mempty}

-- | NTP servers to use, in order of preference.
ntpPayload_ntpServers :: Lens.Lens' NtpPayload [Prelude.Text]
ntpPayload_ntpServers = Lens.lens (\NtpPayload' {ntpServers} -> ntpServers) (\s@NtpPayload' {} a -> s {ntpServers = a} :: NtpPayload) Prelude.. Lens.coerced

instance Data.FromJSON NtpPayload where
  parseJSON =
    Data.withObject
      "NtpPayload"
      ( \x ->
          NtpPayload'
            Prelude.<$> (x Data..:? "NtpServers" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NtpPayload where
  hashWithSalt _salt NtpPayload' {..} =
    _salt `Prelude.hashWithSalt` ntpServers

instance Prelude.NFData NtpPayload where
  rnf NtpPayload' {..} = Prelude.rnf ntpServers

instance Data.ToJSON NtpPayload where
  toJSON NtpPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NtpServers" Data..= ntpServers)]
      )
