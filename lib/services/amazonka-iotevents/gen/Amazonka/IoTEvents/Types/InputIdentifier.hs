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
-- Module      : Amazonka.IoTEvents.Types.InputIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.InputIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.IotEventsInputIdentifier
import Amazonka.IoTEvents.Types.IotSiteWiseInputIdentifier
import qualified Amazonka.Prelude as Prelude

-- | The identifer of the input.
--
-- /See:/ 'newInputIdentifier' smart constructor.
data InputIdentifier = InputIdentifier'
  { -- | The identifer of the input routed from AWS IoT SiteWise.
    iotSiteWiseInputIdentifier :: Prelude.Maybe IotSiteWiseInputIdentifier,
    -- | The identifier of the input routed to AWS IoT Events.
    iotEventsInputIdentifier :: Prelude.Maybe IotEventsInputIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotSiteWiseInputIdentifier', 'inputIdentifier_iotSiteWiseInputIdentifier' - The identifer of the input routed from AWS IoT SiteWise.
--
-- 'iotEventsInputIdentifier', 'inputIdentifier_iotEventsInputIdentifier' - The identifier of the input routed to AWS IoT Events.
newInputIdentifier ::
  InputIdentifier
newInputIdentifier =
  InputIdentifier'
    { iotSiteWiseInputIdentifier =
        Prelude.Nothing,
      iotEventsInputIdentifier = Prelude.Nothing
    }

-- | The identifer of the input routed from AWS IoT SiteWise.
inputIdentifier_iotSiteWiseInputIdentifier :: Lens.Lens' InputIdentifier (Prelude.Maybe IotSiteWiseInputIdentifier)
inputIdentifier_iotSiteWiseInputIdentifier = Lens.lens (\InputIdentifier' {iotSiteWiseInputIdentifier} -> iotSiteWiseInputIdentifier) (\s@InputIdentifier' {} a -> s {iotSiteWiseInputIdentifier = a} :: InputIdentifier)

-- | The identifier of the input routed to AWS IoT Events.
inputIdentifier_iotEventsInputIdentifier :: Lens.Lens' InputIdentifier (Prelude.Maybe IotEventsInputIdentifier)
inputIdentifier_iotEventsInputIdentifier = Lens.lens (\InputIdentifier' {iotEventsInputIdentifier} -> iotEventsInputIdentifier) (\s@InputIdentifier' {} a -> s {iotEventsInputIdentifier = a} :: InputIdentifier)

instance Prelude.Hashable InputIdentifier where
  hashWithSalt _salt InputIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` iotSiteWiseInputIdentifier
      `Prelude.hashWithSalt` iotEventsInputIdentifier

instance Prelude.NFData InputIdentifier where
  rnf InputIdentifier' {..} =
    Prelude.rnf iotSiteWiseInputIdentifier
      `Prelude.seq` Prelude.rnf iotEventsInputIdentifier

instance Data.ToJSON InputIdentifier where
  toJSON InputIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iotSiteWiseInputIdentifier" Data..=)
              Prelude.<$> iotSiteWiseInputIdentifier,
            ("iotEventsInputIdentifier" Data..=)
              Prelude.<$> iotEventsInputIdentifier
          ]
      )
