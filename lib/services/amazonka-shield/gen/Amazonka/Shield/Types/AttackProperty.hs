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
-- Module      : Amazonka.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackLayer
import Amazonka.Shield.Types.AttackPropertyIdentifier
import Amazonka.Shield.Types.Contributor
import Amazonka.Shield.Types.Unit

-- | Details of a Shield event. This is provided as part of an AttackDetail.
--
-- /See:/ 'newAttackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { -- | The total contributions made to this Shield event by all contributors.
    total :: Prelude.Maybe Prelude.Integer,
    -- | The type of Shield event that was observed. @NETWORK@ indicates layer 3
    -- and layer 4 events and @APPLICATION@ indicates layer 7 events.
    --
    -- For infrastructure layer events (L3 and L4 events), you can view metrics
    -- for top contributors in Amazon CloudWatch metrics. For more information,
    -- see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
    -- in the /WAF Developer Guide/.
    attackLayer :: Prelude.Maybe AttackLayer,
    -- | Contributor objects for the top five contributors to a Shield event. A
    -- contributor is a source of traffic that Shield Advanced identifies as
    -- responsible for some or all of an event.
    topContributors :: Prelude.Maybe [Contributor],
    -- | Defines the Shield event property information that is provided. The
    -- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
    -- are valid only for WordPress reflective pingback events.
    attackPropertyIdentifier :: Prelude.Maybe AttackPropertyIdentifier,
    -- | The unit used for the @Contributor@ @Value@ property.
    unit :: Prelude.Maybe Unit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'attackProperty_total' - The total contributions made to this Shield event by all contributors.
--
-- 'attackLayer', 'attackProperty_attackLayer' - The type of Shield event that was observed. @NETWORK@ indicates layer 3
-- and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- For infrastructure layer events (L3 and L4 events), you can view metrics
-- for top contributors in Amazon CloudWatch metrics. For more information,
-- see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
--
-- 'topContributors', 'attackProperty_topContributors' - Contributor objects for the top five contributors to a Shield event. A
-- contributor is a source of traffic that Shield Advanced identifies as
-- responsible for some or all of an event.
--
-- 'attackPropertyIdentifier', 'attackProperty_attackPropertyIdentifier' - Defines the Shield event property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback events.
--
-- 'unit', 'attackProperty_unit' - The unit used for the @Contributor@ @Value@ property.
newAttackProperty ::
  AttackProperty
newAttackProperty =
  AttackProperty'
    { total = Prelude.Nothing,
      attackLayer = Prelude.Nothing,
      topContributors = Prelude.Nothing,
      attackPropertyIdentifier = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The total contributions made to this Shield event by all contributors.
attackProperty_total :: Lens.Lens' AttackProperty (Prelude.Maybe Prelude.Integer)
attackProperty_total = Lens.lens (\AttackProperty' {total} -> total) (\s@AttackProperty' {} a -> s {total = a} :: AttackProperty)

-- | The type of Shield event that was observed. @NETWORK@ indicates layer 3
-- and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- For infrastructure layer events (L3 and L4 events), you can view metrics
-- for top contributors in Amazon CloudWatch metrics. For more information,
-- see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
attackProperty_attackLayer :: Lens.Lens' AttackProperty (Prelude.Maybe AttackLayer)
attackProperty_attackLayer = Lens.lens (\AttackProperty' {attackLayer} -> attackLayer) (\s@AttackProperty' {} a -> s {attackLayer = a} :: AttackProperty)

-- | Contributor objects for the top five contributors to a Shield event. A
-- contributor is a source of traffic that Shield Advanced identifies as
-- responsible for some or all of an event.
attackProperty_topContributors :: Lens.Lens' AttackProperty (Prelude.Maybe [Contributor])
attackProperty_topContributors = Lens.lens (\AttackProperty' {topContributors} -> topContributors) (\s@AttackProperty' {} a -> s {topContributors = a} :: AttackProperty) Prelude.. Lens.mapping Lens.coerced

-- | Defines the Shield event property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback events.
attackProperty_attackPropertyIdentifier :: Lens.Lens' AttackProperty (Prelude.Maybe AttackPropertyIdentifier)
attackProperty_attackPropertyIdentifier = Lens.lens (\AttackProperty' {attackPropertyIdentifier} -> attackPropertyIdentifier) (\s@AttackProperty' {} a -> s {attackPropertyIdentifier = a} :: AttackProperty)

-- | The unit used for the @Contributor@ @Value@ property.
attackProperty_unit :: Lens.Lens' AttackProperty (Prelude.Maybe Unit)
attackProperty_unit = Lens.lens (\AttackProperty' {unit} -> unit) (\s@AttackProperty' {} a -> s {unit = a} :: AttackProperty)

instance Data.FromJSON AttackProperty where
  parseJSON =
    Data.withObject
      "AttackProperty"
      ( \x ->
          AttackProperty'
            Prelude.<$> (x Data..:? "Total")
            Prelude.<*> (x Data..:? "AttackLayer")
            Prelude.<*> ( x Data..:? "TopContributors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AttackPropertyIdentifier")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable AttackProperty where
  hashWithSalt _salt AttackProperty' {..} =
    _salt `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` attackLayer
      `Prelude.hashWithSalt` topContributors
      `Prelude.hashWithSalt` attackPropertyIdentifier
      `Prelude.hashWithSalt` unit

instance Prelude.NFData AttackProperty where
  rnf AttackProperty' {..} =
    Prelude.rnf total
      `Prelude.seq` Prelude.rnf attackLayer
      `Prelude.seq` Prelude.rnf topContributors
      `Prelude.seq` Prelude.rnf attackPropertyIdentifier
      `Prelude.seq` Prelude.rnf unit
