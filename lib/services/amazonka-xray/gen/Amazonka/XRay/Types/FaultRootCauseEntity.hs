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
-- Module      : Amazonka.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.FaultRootCauseEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a
-- trace summary fault error.
--
-- /See:/ 'newFaultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Prelude.Maybe [RootCauseException],
    -- | The name of the entity.
    name :: Prelude.Maybe Prelude.Text,
    -- | A flag that denotes a remote subsegment.
    remote :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaultRootCauseEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptions', 'faultRootCauseEntity_exceptions' - The types and messages of the exceptions.
--
-- 'name', 'faultRootCauseEntity_name' - The name of the entity.
--
-- 'remote', 'faultRootCauseEntity_remote' - A flag that denotes a remote subsegment.
newFaultRootCauseEntity ::
  FaultRootCauseEntity
newFaultRootCauseEntity =
  FaultRootCauseEntity'
    { exceptions = Prelude.Nothing,
      name = Prelude.Nothing,
      remote = Prelude.Nothing
    }

-- | The types and messages of the exceptions.
faultRootCauseEntity_exceptions :: Lens.Lens' FaultRootCauseEntity (Prelude.Maybe [RootCauseException])
faultRootCauseEntity_exceptions = Lens.lens (\FaultRootCauseEntity' {exceptions} -> exceptions) (\s@FaultRootCauseEntity' {} a -> s {exceptions = a} :: FaultRootCauseEntity) Prelude.. Lens.mapping Lens.coerced

-- | The name of the entity.
faultRootCauseEntity_name :: Lens.Lens' FaultRootCauseEntity (Prelude.Maybe Prelude.Text)
faultRootCauseEntity_name = Lens.lens (\FaultRootCauseEntity' {name} -> name) (\s@FaultRootCauseEntity' {} a -> s {name = a} :: FaultRootCauseEntity)

-- | A flag that denotes a remote subsegment.
faultRootCauseEntity_remote :: Lens.Lens' FaultRootCauseEntity (Prelude.Maybe Prelude.Bool)
faultRootCauseEntity_remote = Lens.lens (\FaultRootCauseEntity' {remote} -> remote) (\s@FaultRootCauseEntity' {} a -> s {remote = a} :: FaultRootCauseEntity)

instance Data.FromJSON FaultRootCauseEntity where
  parseJSON =
    Data.withObject
      "FaultRootCauseEntity"
      ( \x ->
          FaultRootCauseEntity'
            Prelude.<$> (x Data..:? "Exceptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Remote")
      )

instance Prelude.Hashable FaultRootCauseEntity where
  hashWithSalt _salt FaultRootCauseEntity' {..} =
    _salt
      `Prelude.hashWithSalt` exceptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` remote

instance Prelude.NFData FaultRootCauseEntity where
  rnf FaultRootCauseEntity' {..} =
    Prelude.rnf exceptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf remote
