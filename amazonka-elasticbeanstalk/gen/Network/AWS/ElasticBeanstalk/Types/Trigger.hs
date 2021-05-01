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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Trigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Trigger where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a trigger.
--
-- /See:/ 'newTrigger' smart constructor.
data Trigger = Trigger'
  { -- | The name of the trigger.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Trigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'trigger_name' - The name of the trigger.
newTrigger ::
  Trigger
newTrigger = Trigger' {name = Prelude.Nothing}

-- | The name of the trigger.
trigger_name :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_name = Lens.lens (\Trigger' {name} -> name) (\s@Trigger' {} a -> s {name = a} :: Trigger)

instance Prelude.FromXML Trigger where
  parseXML x =
    Trigger' Prelude.<$> (x Prelude..@? "Name")

instance Prelude.Hashable Trigger

instance Prelude.NFData Trigger
