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
-- Module      : Network.AWS.WAF.Types.GeoMatchSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchSetSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Contains the identifier and the name of the @GeoMatchSet@.
--
-- /See:/ 'newGeoMatchSetSummary' smart constructor.
data GeoMatchSetSummary = GeoMatchSetSummary'
  { -- | The @GeoMatchSetId@ for an GeoMatchSet. You can use @GeoMatchSetId@ in a
    -- GetGeoMatchSet request to get detailed information about an GeoMatchSet.
    geoMatchSetId :: Prelude.Text,
    -- | A friendly name or description of the GeoMatchSet. You can\'t change the
    -- name of an @GeoMatchSet@ after you create it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GeoMatchSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSetId', 'geoMatchSetSummary_geoMatchSetId' - The @GeoMatchSetId@ for an GeoMatchSet. You can use @GeoMatchSetId@ in a
-- GetGeoMatchSet request to get detailed information about an GeoMatchSet.
--
-- 'name', 'geoMatchSetSummary_name' - A friendly name or description of the GeoMatchSet. You can\'t change the
-- name of an @GeoMatchSet@ after you create it.
newGeoMatchSetSummary ::
  -- | 'geoMatchSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GeoMatchSetSummary
newGeoMatchSetSummary pGeoMatchSetId_ pName_ =
  GeoMatchSetSummary'
    { geoMatchSetId =
        pGeoMatchSetId_,
      name = pName_
    }

-- | The @GeoMatchSetId@ for an GeoMatchSet. You can use @GeoMatchSetId@ in a
-- GetGeoMatchSet request to get detailed information about an GeoMatchSet.
geoMatchSetSummary_geoMatchSetId :: Lens.Lens' GeoMatchSetSummary Prelude.Text
geoMatchSetSummary_geoMatchSetId = Lens.lens (\GeoMatchSetSummary' {geoMatchSetId} -> geoMatchSetId) (\s@GeoMatchSetSummary' {} a -> s {geoMatchSetId = a} :: GeoMatchSetSummary)

-- | A friendly name or description of the GeoMatchSet. You can\'t change the
-- name of an @GeoMatchSet@ after you create it.
geoMatchSetSummary_name :: Lens.Lens' GeoMatchSetSummary Prelude.Text
geoMatchSetSummary_name = Lens.lens (\GeoMatchSetSummary' {name} -> name) (\s@GeoMatchSetSummary' {} a -> s {name = a} :: GeoMatchSetSummary)

instance Prelude.FromJSON GeoMatchSetSummary where
  parseJSON =
    Prelude.withObject
      "GeoMatchSetSummary"
      ( \x ->
          GeoMatchSetSummary'
            Prelude.<$> (x Prelude..: "GeoMatchSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable GeoMatchSetSummary

instance Prelude.NFData GeoMatchSetSummary
