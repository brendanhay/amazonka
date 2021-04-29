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
-- Module      : Network.AWS.EMR.Types.InstanceGroupTimeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupTimeline where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The timeline of the instance group lifecycle.
--
-- /See:/ 'newInstanceGroupTimeline' smart constructor.
data InstanceGroupTimeline = InstanceGroupTimeline'
  { -- | The date and time when the instance group terminated.
    endDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The creation date and time of the instance group.
    creationDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the instance group became ready to perform tasks.
    readyDateTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroupTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDateTime', 'instanceGroupTimeline_endDateTime' - The date and time when the instance group terminated.
--
-- 'creationDateTime', 'instanceGroupTimeline_creationDateTime' - The creation date and time of the instance group.
--
-- 'readyDateTime', 'instanceGroupTimeline_readyDateTime' - The date and time when the instance group became ready to perform tasks.
newInstanceGroupTimeline ::
  InstanceGroupTimeline
newInstanceGroupTimeline =
  InstanceGroupTimeline'
    { endDateTime =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      readyDateTime = Prelude.Nothing
    }

-- | The date and time when the instance group terminated.
instanceGroupTimeline_endDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_endDateTime = Lens.lens (\InstanceGroupTimeline' {endDateTime} -> endDateTime) (\s@InstanceGroupTimeline' {} a -> s {endDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Prelude._Time

-- | The creation date and time of the instance group.
instanceGroupTimeline_creationDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_creationDateTime = Lens.lens (\InstanceGroupTimeline' {creationDateTime} -> creationDateTime) (\s@InstanceGroupTimeline' {} a -> s {creationDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the instance group became ready to perform tasks.
instanceGroupTimeline_readyDateTime :: Lens.Lens' InstanceGroupTimeline (Prelude.Maybe Prelude.UTCTime)
instanceGroupTimeline_readyDateTime = Lens.lens (\InstanceGroupTimeline' {readyDateTime} -> readyDateTime) (\s@InstanceGroupTimeline' {} a -> s {readyDateTime = a} :: InstanceGroupTimeline) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON InstanceGroupTimeline where
  parseJSON =
    Prelude.withObject
      "InstanceGroupTimeline"
      ( \x ->
          InstanceGroupTimeline'
            Prelude.<$> (x Prelude..:? "EndDateTime")
            Prelude.<*> (x Prelude..:? "CreationDateTime")
            Prelude.<*> (x Prelude..:? "ReadyDateTime")
      )

instance Prelude.Hashable InstanceGroupTimeline

instance Prelude.NFData InstanceGroupTimeline
