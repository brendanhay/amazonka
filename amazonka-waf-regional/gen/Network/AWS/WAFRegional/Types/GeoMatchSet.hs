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
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchSet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WAFRegional.Types.GeoMatchConstraint

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Contains one or more countries that AWS WAF will search for.
--
-- /See:/ 'newGeoMatchSet' smart constructor.
data GeoMatchSet = GeoMatchSet'
  { -- | A friendly name or description of the GeoMatchSet. You can\'t change the
    -- name of an @GeoMatchSet@ after you create it.
    name :: Core.Maybe Core.Text,
    -- | The @GeoMatchSetId@ for an @GeoMatchSet@. You use @GeoMatchSetId@ to get
    -- information about a @GeoMatchSet@ (see GeoMatchSet), update a
    -- @GeoMatchSet@ (see UpdateGeoMatchSet), insert a @GeoMatchSet@ into a
    -- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
    -- @GeoMatchSet@ from AWS WAF (see DeleteGeoMatchSet).
    --
    -- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
    -- ListGeoMatchSets.
    geoMatchSetId :: Core.Text,
    -- | An array of GeoMatchConstraint objects, which contain the country that
    -- you want AWS WAF to search for.
    geoMatchConstraints :: [GeoMatchConstraint]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GeoMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'geoMatchSet_name' - A friendly name or description of the GeoMatchSet. You can\'t change the
-- name of an @GeoMatchSet@ after you create it.
--
-- 'geoMatchSetId', 'geoMatchSet_geoMatchSetId' - The @GeoMatchSetId@ for an @GeoMatchSet@. You use @GeoMatchSetId@ to get
-- information about a @GeoMatchSet@ (see GeoMatchSet), update a
-- @GeoMatchSet@ (see UpdateGeoMatchSet), insert a @GeoMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
-- @GeoMatchSet@ from AWS WAF (see DeleteGeoMatchSet).
--
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
--
-- 'geoMatchConstraints', 'geoMatchSet_geoMatchConstraints' - An array of GeoMatchConstraint objects, which contain the country that
-- you want AWS WAF to search for.
newGeoMatchSet ::
  -- | 'geoMatchSetId'
  Core.Text ->
  GeoMatchSet
newGeoMatchSet pGeoMatchSetId_ =
  GeoMatchSet'
    { name = Core.Nothing,
      geoMatchSetId = pGeoMatchSetId_,
      geoMatchConstraints = Core.mempty
    }

-- | A friendly name or description of the GeoMatchSet. You can\'t change the
-- name of an @GeoMatchSet@ after you create it.
geoMatchSet_name :: Lens.Lens' GeoMatchSet (Core.Maybe Core.Text)
geoMatchSet_name = Lens.lens (\GeoMatchSet' {name} -> name) (\s@GeoMatchSet' {} a -> s {name = a} :: GeoMatchSet)

-- | The @GeoMatchSetId@ for an @GeoMatchSet@. You use @GeoMatchSetId@ to get
-- information about a @GeoMatchSet@ (see GeoMatchSet), update a
-- @GeoMatchSet@ (see UpdateGeoMatchSet), insert a @GeoMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
-- @GeoMatchSet@ from AWS WAF (see DeleteGeoMatchSet).
--
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
geoMatchSet_geoMatchSetId :: Lens.Lens' GeoMatchSet Core.Text
geoMatchSet_geoMatchSetId = Lens.lens (\GeoMatchSet' {geoMatchSetId} -> geoMatchSetId) (\s@GeoMatchSet' {} a -> s {geoMatchSetId = a} :: GeoMatchSet)

-- | An array of GeoMatchConstraint objects, which contain the country that
-- you want AWS WAF to search for.
geoMatchSet_geoMatchConstraints :: Lens.Lens' GeoMatchSet [GeoMatchConstraint]
geoMatchSet_geoMatchConstraints = Lens.lens (\GeoMatchSet' {geoMatchConstraints} -> geoMatchConstraints) (\s@GeoMatchSet' {} a -> s {geoMatchConstraints = a} :: GeoMatchSet) Core.. Lens._Coerce

instance Core.FromJSON GeoMatchSet where
  parseJSON =
    Core.withObject
      "GeoMatchSet"
      ( \x ->
          GeoMatchSet'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..: "GeoMatchSetId")
            Core.<*> ( x Core..:? "GeoMatchConstraints"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable GeoMatchSet

instance Core.NFData GeoMatchSet
