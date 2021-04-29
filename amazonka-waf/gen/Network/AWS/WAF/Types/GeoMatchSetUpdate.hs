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
-- Module      : Network.AWS.WAF.Types.GeoMatchSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.GeoMatchConstraint

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the type of update to perform to an GeoMatchSet with
-- UpdateGeoMatchSet.
--
-- /See:/ 'newGeoMatchSetUpdate' smart constructor.
data GeoMatchSetUpdate = GeoMatchSetUpdate'
  { -- | Specifies whether to insert or delete a country with UpdateGeoMatchSet.
    action :: ChangeAction,
    -- | The country from which web requests originate that you want AWS WAF to
    -- search for.
    geoMatchConstraint :: GeoMatchConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GeoMatchSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'geoMatchSetUpdate_action' - Specifies whether to insert or delete a country with UpdateGeoMatchSet.
--
-- 'geoMatchConstraint', 'geoMatchSetUpdate_geoMatchConstraint' - The country from which web requests originate that you want AWS WAF to
-- search for.
newGeoMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'geoMatchConstraint'
  GeoMatchConstraint ->
  GeoMatchSetUpdate
newGeoMatchSetUpdate pAction_ pGeoMatchConstraint_ =
  GeoMatchSetUpdate'
    { action = pAction_,
      geoMatchConstraint = pGeoMatchConstraint_
    }

-- | Specifies whether to insert or delete a country with UpdateGeoMatchSet.
geoMatchSetUpdate_action :: Lens.Lens' GeoMatchSetUpdate ChangeAction
geoMatchSetUpdate_action = Lens.lens (\GeoMatchSetUpdate' {action} -> action) (\s@GeoMatchSetUpdate' {} a -> s {action = a} :: GeoMatchSetUpdate)

-- | The country from which web requests originate that you want AWS WAF to
-- search for.
geoMatchSetUpdate_geoMatchConstraint :: Lens.Lens' GeoMatchSetUpdate GeoMatchConstraint
geoMatchSetUpdate_geoMatchConstraint = Lens.lens (\GeoMatchSetUpdate' {geoMatchConstraint} -> geoMatchConstraint) (\s@GeoMatchSetUpdate' {} a -> s {geoMatchConstraint = a} :: GeoMatchSetUpdate)

instance Prelude.Hashable GeoMatchSetUpdate

instance Prelude.NFData GeoMatchSetUpdate

instance Prelude.ToJSON GeoMatchSetUpdate where
  toJSON GeoMatchSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ( "GeoMatchConstraint"
                  Prelude..= geoMatchConstraint
              )
          ]
      )
