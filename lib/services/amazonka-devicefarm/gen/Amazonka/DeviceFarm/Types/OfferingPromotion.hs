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
-- Module      : Amazonka.DeviceFarm.Types.OfferingPromotion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.OfferingPromotion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an offering promotion.
--
-- /See:/ 'newOfferingPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { -- | A string that describes the offering promotion.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the offering promotion.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OfferingPromotion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'offeringPromotion_description' - A string that describes the offering promotion.
--
-- 'id', 'offeringPromotion_id' - The ID of the offering promotion.
newOfferingPromotion ::
  OfferingPromotion
newOfferingPromotion =
  OfferingPromotion'
    { description = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A string that describes the offering promotion.
offeringPromotion_description :: Lens.Lens' OfferingPromotion (Prelude.Maybe Prelude.Text)
offeringPromotion_description = Lens.lens (\OfferingPromotion' {description} -> description) (\s@OfferingPromotion' {} a -> s {description = a} :: OfferingPromotion)

-- | The ID of the offering promotion.
offeringPromotion_id :: Lens.Lens' OfferingPromotion (Prelude.Maybe Prelude.Text)
offeringPromotion_id = Lens.lens (\OfferingPromotion' {id} -> id) (\s@OfferingPromotion' {} a -> s {id = a} :: OfferingPromotion)

instance Data.FromJSON OfferingPromotion where
  parseJSON =
    Data.withObject
      "OfferingPromotion"
      ( \x ->
          OfferingPromotion'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable OfferingPromotion where
  hashWithSalt _salt OfferingPromotion' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData OfferingPromotion where
  rnf OfferingPromotion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
