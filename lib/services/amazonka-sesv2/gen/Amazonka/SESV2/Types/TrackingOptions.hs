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
-- Module      : Amazonka.SESV2.Types.TrackingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.TrackingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the tracking options for a configuration set.
-- When you use the Amazon SES API v2 to send an email, it contains an
-- invisible image that\'s used to track when recipients open your email.
-- If your email contains links, those links are changed slightly in order
-- to track when recipients click them.
--
-- These images and links include references to a domain operated by Amazon
-- Web Services. You can optionally configure the Amazon SES to use a
-- domain that you operate for these images and links.
--
-- /See:/ 'newTrackingOptions' smart constructor.
data TrackingOptions = TrackingOptions'
  { -- | The domain to use for tracking open and click events.
    customRedirectDomain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrackingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRedirectDomain', 'trackingOptions_customRedirectDomain' - The domain to use for tracking open and click events.
newTrackingOptions ::
  -- | 'customRedirectDomain'
  Prelude.Text ->
  TrackingOptions
newTrackingOptions pCustomRedirectDomain_ =
  TrackingOptions'
    { customRedirectDomain =
        pCustomRedirectDomain_
    }

-- | The domain to use for tracking open and click events.
trackingOptions_customRedirectDomain :: Lens.Lens' TrackingOptions Prelude.Text
trackingOptions_customRedirectDomain = Lens.lens (\TrackingOptions' {customRedirectDomain} -> customRedirectDomain) (\s@TrackingOptions' {} a -> s {customRedirectDomain = a} :: TrackingOptions)

instance Data.FromJSON TrackingOptions where
  parseJSON =
    Data.withObject
      "TrackingOptions"
      ( \x ->
          TrackingOptions'
            Prelude.<$> (x Data..: "CustomRedirectDomain")
      )

instance Prelude.Hashable TrackingOptions where
  hashWithSalt _salt TrackingOptions' {..} =
    _salt `Prelude.hashWithSalt` customRedirectDomain

instance Prelude.NFData TrackingOptions where
  rnf TrackingOptions' {..} =
    Prelude.rnf customRedirectDomain

instance Data.ToJSON TrackingOptions where
  toJSON TrackingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CustomRedirectDomain"
                  Data..= customRedirectDomain
              )
          ]
      )
