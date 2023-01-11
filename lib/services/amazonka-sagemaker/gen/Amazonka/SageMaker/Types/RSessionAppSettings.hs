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
-- Module      : Amazonka.SageMaker.Types.RSessionAppSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RSessionAppSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CustomImage
import Amazonka.SageMaker.Types.ResourceSpec

-- | A collection of settings that apply to an @RSessionGateway@ app.
--
-- /See:/ 'newRSessionAppSettings' smart constructor.
data RSessionAppSettings = RSessionAppSettings'
  { -- | A list of custom SageMaker images that are configured to run as a
    -- RSession app.
    customImages :: Prelude.Maybe [CustomImage],
    defaultResourceSpec :: Prelude.Maybe ResourceSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RSessionAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customImages', 'rSessionAppSettings_customImages' - A list of custom SageMaker images that are configured to run as a
-- RSession app.
--
-- 'defaultResourceSpec', 'rSessionAppSettings_defaultResourceSpec' - Undocumented member.
newRSessionAppSettings ::
  RSessionAppSettings
newRSessionAppSettings =
  RSessionAppSettings'
    { customImages =
        Prelude.Nothing,
      defaultResourceSpec = Prelude.Nothing
    }

-- | A list of custom SageMaker images that are configured to run as a
-- RSession app.
rSessionAppSettings_customImages :: Lens.Lens' RSessionAppSettings (Prelude.Maybe [CustomImage])
rSessionAppSettings_customImages = Lens.lens (\RSessionAppSettings' {customImages} -> customImages) (\s@RSessionAppSettings' {} a -> s {customImages = a} :: RSessionAppSettings) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
rSessionAppSettings_defaultResourceSpec :: Lens.Lens' RSessionAppSettings (Prelude.Maybe ResourceSpec)
rSessionAppSettings_defaultResourceSpec = Lens.lens (\RSessionAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@RSessionAppSettings' {} a -> s {defaultResourceSpec = a} :: RSessionAppSettings)

instance Data.FromJSON RSessionAppSettings where
  parseJSON =
    Data.withObject
      "RSessionAppSettings"
      ( \x ->
          RSessionAppSettings'
            Prelude.<$> (x Data..:? "CustomImages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DefaultResourceSpec")
      )

instance Prelude.Hashable RSessionAppSettings where
  hashWithSalt _salt RSessionAppSettings' {..} =
    _salt `Prelude.hashWithSalt` customImages
      `Prelude.hashWithSalt` defaultResourceSpec

instance Prelude.NFData RSessionAppSettings where
  rnf RSessionAppSettings' {..} =
    Prelude.rnf customImages
      `Prelude.seq` Prelude.rnf defaultResourceSpec

instance Data.ToJSON RSessionAppSettings where
  toJSON RSessionAppSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomImages" Data..=) Prelude.<$> customImages,
            ("DefaultResourceSpec" Data..=)
              Prelude.<$> defaultResourceSpec
          ]
      )
