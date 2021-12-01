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
-- Module      : Amazonka.IoTThingsGraph.Types.Thing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.Thing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An AWS IoT thing.
--
-- /See:/ 'newThing' smart constructor.
data Thing = Thing'
  { -- | The ARN of the thing.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Thing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'thing_thingArn' - The ARN of the thing.
--
-- 'thingName', 'thing_thingName' - The name of the thing.
newThing ::
  Thing
newThing =
  Thing'
    { thingArn = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | The ARN of the thing.
thing_thingArn :: Lens.Lens' Thing (Prelude.Maybe Prelude.Text)
thing_thingArn = Lens.lens (\Thing' {thingArn} -> thingArn) (\s@Thing' {} a -> s {thingArn = a} :: Thing)

-- | The name of the thing.
thing_thingName :: Lens.Lens' Thing (Prelude.Maybe Prelude.Text)
thing_thingName = Lens.lens (\Thing' {thingName} -> thingName) (\s@Thing' {} a -> s {thingName = a} :: Thing)

instance Core.FromJSON Thing where
  parseJSON =
    Core.withObject
      "Thing"
      ( \x ->
          Thing'
            Prelude.<$> (x Core..:? "thingArn")
            Prelude.<*> (x Core..:? "thingName")
      )

instance Prelude.Hashable Thing where
  hashWithSalt salt' Thing' {..} =
    salt' `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` thingArn

instance Prelude.NFData Thing where
  rnf Thing' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf thingName
