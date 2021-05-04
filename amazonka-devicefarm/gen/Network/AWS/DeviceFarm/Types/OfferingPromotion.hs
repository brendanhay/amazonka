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
-- Module      : Network.AWS.DeviceFarm.Types.OfferingPromotion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingPromotion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about an offering promotion.
--
-- /See:/ 'newOfferingPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { -- | The ID of the offering promotion.
    id :: Prelude.Maybe Prelude.Text,
    -- | A string that describes the offering promotion.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OfferingPromotion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'offeringPromotion_id' - The ID of the offering promotion.
--
-- 'description', 'offeringPromotion_description' - A string that describes the offering promotion.
newOfferingPromotion ::
  OfferingPromotion
newOfferingPromotion =
  OfferingPromotion'
    { id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the offering promotion.
offeringPromotion_id :: Lens.Lens' OfferingPromotion (Prelude.Maybe Prelude.Text)
offeringPromotion_id = Lens.lens (\OfferingPromotion' {id} -> id) (\s@OfferingPromotion' {} a -> s {id = a} :: OfferingPromotion)

-- | A string that describes the offering promotion.
offeringPromotion_description :: Lens.Lens' OfferingPromotion (Prelude.Maybe Prelude.Text)
offeringPromotion_description = Lens.lens (\OfferingPromotion' {description} -> description) (\s@OfferingPromotion' {} a -> s {description = a} :: OfferingPromotion)

instance Prelude.FromJSON OfferingPromotion where
  parseJSON =
    Prelude.withObject
      "OfferingPromotion"
      ( \x ->
          OfferingPromotion'
            Prelude.<$> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable OfferingPromotion

instance Prelude.NFData OfferingPromotion
