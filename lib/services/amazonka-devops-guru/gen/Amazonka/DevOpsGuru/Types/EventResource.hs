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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EventResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Web Services resource that emitted an event. Amazon Web
-- Services resource events and metrics are analyzed by DevOps Guru to find
-- anomalous behavior and provide recommendations to improve your
-- operational solutions.
--
-- /See:/ 'newEventResource' smart constructor.
data EventResource = EventResource'
  { -- | The Amazon Resource Name (ARN) of the resource that emitted an event.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource that emitted an event.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of resource that emitted an event.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'eventResource_arn' - The Amazon Resource Name (ARN) of the resource that emitted an event.
--
-- 'name', 'eventResource_name' - The name of the resource that emitted an event.
--
-- 'type'', 'eventResource_type' - The type of resource that emitted an event.
newEventResource ::
  EventResource
newEventResource =
  EventResource'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource that emitted an event.
eventResource_arn :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_arn = Lens.lens (\EventResource' {arn} -> arn) (\s@EventResource' {} a -> s {arn = a} :: EventResource)

-- | The name of the resource that emitted an event.
eventResource_name :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_name = Lens.lens (\EventResource' {name} -> name) (\s@EventResource' {} a -> s {name = a} :: EventResource)

-- | The type of resource that emitted an event.
eventResource_type :: Lens.Lens' EventResource (Prelude.Maybe Prelude.Text)
eventResource_type = Lens.lens (\EventResource' {type'} -> type') (\s@EventResource' {} a -> s {type' = a} :: EventResource)

instance Data.FromJSON EventResource where
  parseJSON =
    Data.withObject
      "EventResource"
      ( \x ->
          EventResource'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable EventResource where
  hashWithSalt _salt EventResource' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EventResource where
  rnf EventResource' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
