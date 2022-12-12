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
-- Module      : Amazonka.Evidently.Types.RefResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.RefResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one experiment or launch
-- that uses the specified segment.
--
-- /See:/ 'newRefResource' smart constructor.
data RefResource = RefResource'
  { -- | The ARN of the experiment or launch.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The day and time that this experiment or launch ended.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The day and time that this experiment or launch was most recently
    -- updated.
    lastUpdatedOn :: Prelude.Maybe Prelude.Text,
    -- | The day and time that this experiment or launch started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the experiment or launch.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment or launch.
    name :: Prelude.Text,
    -- | Specifies whether the resource that this structure contains information
    -- about is an experiment or a launch.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'refResource_arn' - The ARN of the experiment or launch.
--
-- 'endTime', 'refResource_endTime' - The day and time that this experiment or launch ended.
--
-- 'lastUpdatedOn', 'refResource_lastUpdatedOn' - The day and time that this experiment or launch was most recently
-- updated.
--
-- 'startTime', 'refResource_startTime' - The day and time that this experiment or launch started.
--
-- 'status', 'refResource_status' - The status of the experiment or launch.
--
-- 'name', 'refResource_name' - The name of the experiment or launch.
--
-- 'type'', 'refResource_type' - Specifies whether the resource that this structure contains information
-- about is an experiment or a launch.
newRefResource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  RefResource
newRefResource pName_ pType_ =
  RefResource'
    { arn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      lastUpdatedOn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The ARN of the experiment or launch.
refResource_arn :: Lens.Lens' RefResource (Prelude.Maybe Prelude.Text)
refResource_arn = Lens.lens (\RefResource' {arn} -> arn) (\s@RefResource' {} a -> s {arn = a} :: RefResource)

-- | The day and time that this experiment or launch ended.
refResource_endTime :: Lens.Lens' RefResource (Prelude.Maybe Prelude.Text)
refResource_endTime = Lens.lens (\RefResource' {endTime} -> endTime) (\s@RefResource' {} a -> s {endTime = a} :: RefResource)

-- | The day and time that this experiment or launch was most recently
-- updated.
refResource_lastUpdatedOn :: Lens.Lens' RefResource (Prelude.Maybe Prelude.Text)
refResource_lastUpdatedOn = Lens.lens (\RefResource' {lastUpdatedOn} -> lastUpdatedOn) (\s@RefResource' {} a -> s {lastUpdatedOn = a} :: RefResource)

-- | The day and time that this experiment or launch started.
refResource_startTime :: Lens.Lens' RefResource (Prelude.Maybe Prelude.Text)
refResource_startTime = Lens.lens (\RefResource' {startTime} -> startTime) (\s@RefResource' {} a -> s {startTime = a} :: RefResource)

-- | The status of the experiment or launch.
refResource_status :: Lens.Lens' RefResource (Prelude.Maybe Prelude.Text)
refResource_status = Lens.lens (\RefResource' {status} -> status) (\s@RefResource' {} a -> s {status = a} :: RefResource)

-- | The name of the experiment or launch.
refResource_name :: Lens.Lens' RefResource Prelude.Text
refResource_name = Lens.lens (\RefResource' {name} -> name) (\s@RefResource' {} a -> s {name = a} :: RefResource)

-- | Specifies whether the resource that this structure contains information
-- about is an experiment or a launch.
refResource_type :: Lens.Lens' RefResource Prelude.Text
refResource_type = Lens.lens (\RefResource' {type'} -> type') (\s@RefResource' {} a -> s {type' = a} :: RefResource)

instance Data.FromJSON RefResource where
  parseJSON =
    Data.withObject
      "RefResource"
      ( \x ->
          RefResource'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "lastUpdatedOn")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable RefResource where
  hashWithSalt _salt RefResource' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` lastUpdatedOn
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RefResource where
  rnf RefResource' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf lastUpdatedOn
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
