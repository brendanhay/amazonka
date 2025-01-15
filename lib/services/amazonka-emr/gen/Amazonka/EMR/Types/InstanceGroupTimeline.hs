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
-- Module      : Amazonka.EMR.Types.InstanceGroupTimeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The timeline of the instance group lifecycle.
--
-- /See:/ 'newInstanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
  { -- | The creation date and time of the instance group.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the instance group terminated.
    endDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the instance group became ready to perform tasks.
    readyDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroupTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'instanceGroupTimeline_creationDateTime' - The creation date and time of the instance group.
--
-- 'endDateTime', 'instanceGroupTimeline_endDateTime' - The date and time when the instance group terminated.
--
-- 'readyDateTime', 'instanceGroupTimeline_readyDateTime' - The date and time when the instance group became ready to perform tasks.
newInstanceGroupTimeline ::
  InstanceGroupTimeline
newInstanceGroupTimeline =
  InstanceGroupTimeline'
    { creationDateTime =
        Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      readyDateTime = Prelude.Nothing
    }

-- | The creation date and time of the instance group.
instanceGroupTimeline_creationDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_creationDateTime = Lens.lens (\InstanceGroupTimeline' {creationDateTime} -> creationDateTime) (\s@InstanceGroupTimeline' {} a -> s {creationDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time when the instance group terminated.
instanceGroupTimeline_endDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_endDateTime = Lens.lens (\InstanceGroupTimeline' {endDateTime} -> endDateTime) (\s@InstanceGroupTimeline' {} a -> s {endDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time when the instance group became ready to perform tasks.
instanceGroupTimeline_readyDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_readyDateTime = Lens.lens (\InstanceGroupTimeline' {readyDateTime} -> readyDateTime) (\s@InstanceGroupTimeline' {} a -> s {readyDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON InstanceGroupTimeline where
  parseJSON =
    Data.withObject
      "InstanceGroupTimeline"
      ( \x ->
          InstanceGroupTimeline'
            Prelude.<$> (x Data..:? "CreationDateTime")
            Prelude.<*> (x Data..:? "EndDateTime")
            Prelude.<*> (x Data..:? "ReadyDateTime")
      )

instance Prelude.Hashable InstanceGroupTimeline where
  hashWithSalt _salt InstanceGroupTimeline' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` readyDateTime

instance Prelude.NFData InstanceGroupTimeline where
  rnf InstanceGroupTimeline' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf endDateTime `Prelude.seq`
        Prelude.rnf readyDateTime
