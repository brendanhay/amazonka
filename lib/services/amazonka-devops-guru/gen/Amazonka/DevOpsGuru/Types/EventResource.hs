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
-- Module      : Amazonka.DevOpsGuru.Types.EventResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EventResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Web Services resource that emitted an event. Amazon Web
-- Services resource events and metrics are analyzed by DevOps Guru to find
-- anomalous behavior and provide recommendations to improve your
-- operational solutions.
--
-- /See:/ 'newEventResource' smart constructor.
data EventResource = EventResource'
  { -- | The name of the resource that emitted an event.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of resource that emitted an event.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource that emitted an event.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventResource_name' - The name of the resource that emitted an event.
--
-- 'type'', 'eventResource_type' - The type of resource that emitted an event.
--
-- 'arn', 'eventResource_arn' - The Amazon Resource Name (ARN) of the resource that emitted an event.
newEventResource ::
  EventResource
newEventResource =
  EventResource'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name of the resource that emitted an event.
eventResource_name :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_name = Lens.lens (\EventResource' {name} -> name) (\s@EventResource' {} a -> s {name = a} :: EventResource)

-- | The type of resource that emitted an event.
eventResource_type :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_type = Lens.lens (\EventResource' {type'} -> type') (\s@EventResource' {} a -> s {type' = a} :: EventResource)

-- | The Amazon Resource Name (ARN) of the resource that emitted an event.
eventResource_arn :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_arn = Lens.lens (\EventResource' {arn} -> arn) (\s@EventResource' {} a -> s {arn = a} :: EventResource)

instance Core.FromJSON EventResource where
  parseJSON =
    Core.withObject
      "EventResource"
      ( \x ->
          EventResource'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Arn")
      )

instance Prelude.Hashable EventResource where
  hashWithSalt _salt EventResource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn

instance Prelude.NFData EventResource where
  rnf EventResource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
