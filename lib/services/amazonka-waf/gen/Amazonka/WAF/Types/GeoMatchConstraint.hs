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
-- Module      : Amazonka.WAF.Types.GeoMatchConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.GeoMatchConstraint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.GeoMatchConstraintType
import Amazonka.WAF.Types.GeoMatchConstraintValue

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The country from which web requests originate that you want AWS WAF to
-- search for.
--
-- /See:/ 'newGeoMatchConstraint' smart constructor.
data GeoMatchConstraint = GeoMatchConstraint'
  { -- | The type of geographical area you want AWS WAF to search for. Currently
    -- @Country@ is the only valid value.
    type' :: GeoMatchConstraintType,
    -- | The country that you want AWS WAF to search for.
    value :: GeoMatchConstraintValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoMatchConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'geoMatchConstraint_type' - The type of geographical area you want AWS WAF to search for. Currently
-- @Country@ is the only valid value.
--
-- 'value', 'geoMatchConstraint_value' - The country that you want AWS WAF to search for.
newGeoMatchConstraint ::
  -- | 'type''
  GeoMatchConstraintType ->
  -- | 'value'
  GeoMatchConstraintValue ->
  GeoMatchConstraint
newGeoMatchConstraint pType_ pValue_ =
  GeoMatchConstraint'
    { type' = pType_,
      value = pValue_
    }

-- | The type of geographical area you want AWS WAF to search for. Currently
-- @Country@ is the only valid value.
geoMatchConstraint_type :: Lens.Lens' GeoMatchConstraint GeoMatchConstraintType
geoMatchConstraint_type = Lens.lens (\GeoMatchConstraint' {type'} -> type') (\s@GeoMatchConstraint' {} a -> s {type' = a} :: GeoMatchConstraint)

-- | The country that you want AWS WAF to search for.
geoMatchConstraint_value :: Lens.Lens' GeoMatchConstraint GeoMatchConstraintValue
geoMatchConstraint_value = Lens.lens (\GeoMatchConstraint' {value} -> value) (\s@GeoMatchConstraint' {} a -> s {value = a} :: GeoMatchConstraint)

instance Core.FromJSON GeoMatchConstraint where
  parseJSON =
    Core.withObject
      "GeoMatchConstraint"
      ( \x ->
          GeoMatchConstraint'
            Prelude.<$> (x Core..: "Type") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable GeoMatchConstraint where
  hashWithSalt _salt GeoMatchConstraint' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData GeoMatchConstraint where
  rnf GeoMatchConstraint' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Core.ToJSON GeoMatchConstraint where
  toJSON GeoMatchConstraint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Value" Core..= value)
          ]
      )
