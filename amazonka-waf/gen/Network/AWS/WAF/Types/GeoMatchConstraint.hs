{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.GeoMatchConstraintType
import Network.AWS.WAF.Types.GeoMatchConstraintValue

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON GeoMatchConstraint where
  parseJSON =
    Prelude.withObject
      "GeoMatchConstraint"
      ( \x ->
          GeoMatchConstraint'
            Prelude.<$> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable GeoMatchConstraint

instance Prelude.NFData GeoMatchConstraint

instance Prelude.ToJSON GeoMatchConstraint where
  toJSON GeoMatchConstraint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
