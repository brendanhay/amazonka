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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ReverseGeocodingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ReverseGeocodingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newReverseGeocodingConfig' smart constructor.
data ReverseGeocodingConfig = ReverseGeocodingConfig'
  { xAttributeName :: Prelude.Text,
    yAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReverseGeocodingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xAttributeName', 'reverseGeocodingConfig_xAttributeName' -
--
-- 'yAttributeName', 'reverseGeocodingConfig_yAttributeName' -
newReverseGeocodingConfig ::
  -- | 'xAttributeName'
  Prelude.Text ->
  -- | 'yAttributeName'
  Prelude.Text ->
  ReverseGeocodingConfig
newReverseGeocodingConfig
  pXAttributeName_
  pYAttributeName_ =
    ReverseGeocodingConfig'
      { xAttributeName =
          pXAttributeName_,
        yAttributeName = pYAttributeName_
      }

reverseGeocodingConfig_xAttributeName :: Lens.Lens' ReverseGeocodingConfig Prelude.Text
reverseGeocodingConfig_xAttributeName = Lens.lens (\ReverseGeocodingConfig' {xAttributeName} -> xAttributeName) (\s@ReverseGeocodingConfig' {} a -> s {xAttributeName = a} :: ReverseGeocodingConfig)

reverseGeocodingConfig_yAttributeName :: Lens.Lens' ReverseGeocodingConfig Prelude.Text
reverseGeocodingConfig_yAttributeName = Lens.lens (\ReverseGeocodingConfig' {yAttributeName} -> yAttributeName) (\s@ReverseGeocodingConfig' {} a -> s {yAttributeName = a} :: ReverseGeocodingConfig)

instance Data.FromJSON ReverseGeocodingConfig where
  parseJSON =
    Data.withObject
      "ReverseGeocodingConfig"
      ( \x ->
          ReverseGeocodingConfig'
            Prelude.<$> (x Data..: "XAttributeName")
            Prelude.<*> (x Data..: "YAttributeName")
      )

instance Prelude.Hashable ReverseGeocodingConfig where
  hashWithSalt _salt ReverseGeocodingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` xAttributeName
      `Prelude.hashWithSalt` yAttributeName

instance Prelude.NFData ReverseGeocodingConfig where
  rnf ReverseGeocodingConfig' {..} =
    Prelude.rnf xAttributeName
      `Prelude.seq` Prelude.rnf yAttributeName

instance Data.ToJSON ReverseGeocodingConfig where
  toJSON ReverseGeocodingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("XAttributeName" Data..= xAttributeName),
            Prelude.Just
              ("YAttributeName" Data..= yAttributeName)
          ]
      )
