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
-- Module      : Network.AWS.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.AttackLayer
import Network.AWS.Shield.Types.AttackPropertyIdentifier
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.Unit

-- | Details of the described attack.
--
-- /See:/ 'newAttackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { -- | The unit of the @Value@ of the contributions.
    unit :: Prelude.Maybe Unit,
    -- | The total contributions made to this attack by all contributors, not
    -- just the five listed in the @TopContributors@ list.
    total :: Prelude.Maybe Prelude.Integer,
    -- | Defines the DDoS attack property information that is provided. The
    -- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
    -- are valid only for WordPress reflective pingback DDoS attacks.
    attackPropertyIdentifier :: Prelude.Maybe AttackPropertyIdentifier,
    -- | The type of distributed denial of service (DDoS) event that was
    -- observed. @NETWORK@ indicates layer 3 and layer 4 events and
    -- @APPLICATION@ indicates layer 7 events.
    attackLayer :: Prelude.Maybe AttackLayer,
    -- | The array of contributor objects that includes the top five contributors
    -- to an attack.
    topContributors :: Prelude.Maybe [Contributor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttackProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'attackProperty_unit' - The unit of the @Value@ of the contributions.
--
-- 'total', 'attackProperty_total' - The total contributions made to this attack by all contributors, not
-- just the five listed in the @TopContributors@ list.
--
-- 'attackPropertyIdentifier', 'attackProperty_attackPropertyIdentifier' - Defines the DDoS attack property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback DDoS attacks.
--
-- 'attackLayer', 'attackProperty_attackLayer' - The type of distributed denial of service (DDoS) event that was
-- observed. @NETWORK@ indicates layer 3 and layer 4 events and
-- @APPLICATION@ indicates layer 7 events.
--
-- 'topContributors', 'attackProperty_topContributors' - The array of contributor objects that includes the top five contributors
-- to an attack.
newAttackProperty ::
  AttackProperty
newAttackProperty =
  AttackProperty'
    { unit = Prelude.Nothing,
      total = Prelude.Nothing,
      attackPropertyIdentifier = Prelude.Nothing,
      attackLayer = Prelude.Nothing,
      topContributors = Prelude.Nothing
    }

-- | The unit of the @Value@ of the contributions.
attackProperty_unit :: Lens.Lens' AttackProperty (Prelude.Maybe Unit)
attackProperty_unit = Lens.lens (\AttackProperty' {unit} -> unit) (\s@AttackProperty' {} a -> s {unit = a} :: AttackProperty)

-- | The total contributions made to this attack by all contributors, not
-- just the five listed in the @TopContributors@ list.
attackProperty_total :: Lens.Lens' AttackProperty (Prelude.Maybe Prelude.Integer)
attackProperty_total = Lens.lens (\AttackProperty' {total} -> total) (\s@AttackProperty' {} a -> s {total = a} :: AttackProperty)

-- | Defines the DDoS attack property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback DDoS attacks.
attackProperty_attackPropertyIdentifier :: Lens.Lens' AttackProperty (Prelude.Maybe AttackPropertyIdentifier)
attackProperty_attackPropertyIdentifier = Lens.lens (\AttackProperty' {attackPropertyIdentifier} -> attackPropertyIdentifier) (\s@AttackProperty' {} a -> s {attackPropertyIdentifier = a} :: AttackProperty)

-- | The type of distributed denial of service (DDoS) event that was
-- observed. @NETWORK@ indicates layer 3 and layer 4 events and
-- @APPLICATION@ indicates layer 7 events.
attackProperty_attackLayer :: Lens.Lens' AttackProperty (Prelude.Maybe AttackLayer)
attackProperty_attackLayer = Lens.lens (\AttackProperty' {attackLayer} -> attackLayer) (\s@AttackProperty' {} a -> s {attackLayer = a} :: AttackProperty)

-- | The array of contributor objects that includes the top five contributors
-- to an attack.
attackProperty_topContributors :: Lens.Lens' AttackProperty (Prelude.Maybe [Contributor])
attackProperty_topContributors = Lens.lens (\AttackProperty' {topContributors} -> topContributors) (\s@AttackProperty' {} a -> s {topContributors = a} :: AttackProperty) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AttackProperty where
  parseJSON =
    Prelude.withObject
      "AttackProperty"
      ( \x ->
          AttackProperty'
            Prelude.<$> (x Prelude..:? "Unit")
            Prelude.<*> (x Prelude..:? "Total")
            Prelude.<*> (x Prelude..:? "AttackPropertyIdentifier")
            Prelude.<*> (x Prelude..:? "AttackLayer")
            Prelude.<*> ( x Prelude..:? "TopContributors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AttackProperty

instance Prelude.NFData AttackProperty
