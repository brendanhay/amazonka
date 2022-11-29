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
-- Module      : Amazonka.AppFlow.Types.GoogleAnalyticsSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.GoogleAnalyticsSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Google Analytics is being used as a
-- source.
--
-- /See:/ 'newGoogleAnalyticsSourceProperties' smart constructor.
data GoogleAnalyticsSourceProperties = GoogleAnalyticsSourceProperties'
  { -- | The object specified in the Google Analytics flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GoogleAnalyticsSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'googleAnalyticsSourceProperties_object' - The object specified in the Google Analytics flow source.
newGoogleAnalyticsSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  GoogleAnalyticsSourceProperties
newGoogleAnalyticsSourceProperties pObject_ =
  GoogleAnalyticsSourceProperties'
    { object' =
        pObject_
    }

-- | The object specified in the Google Analytics flow source.
googleAnalyticsSourceProperties_object :: Lens.Lens' GoogleAnalyticsSourceProperties Prelude.Text
googleAnalyticsSourceProperties_object = Lens.lens (\GoogleAnalyticsSourceProperties' {object'} -> object') (\s@GoogleAnalyticsSourceProperties' {} a -> s {object' = a} :: GoogleAnalyticsSourceProperties)

instance
  Core.FromJSON
    GoogleAnalyticsSourceProperties
  where
  parseJSON =
    Core.withObject
      "GoogleAnalyticsSourceProperties"
      ( \x ->
          GoogleAnalyticsSourceProperties'
            Prelude.<$> (x Core..: "object")
      )

instance
  Prelude.Hashable
    GoogleAnalyticsSourceProperties
  where
  hashWithSalt
    _salt
    GoogleAnalyticsSourceProperties' {..} =
      _salt `Prelude.hashWithSalt` object'

instance
  Prelude.NFData
    GoogleAnalyticsSourceProperties
  where
  rnf GoogleAnalyticsSourceProperties' {..} =
    Prelude.rnf object'

instance Core.ToJSON GoogleAnalyticsSourceProperties where
  toJSON GoogleAnalyticsSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Core..= object')]
      )
