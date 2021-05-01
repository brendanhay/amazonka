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
-- Module      : Network.AWS.ELB.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Instance where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ID of an EC2 instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instance_instanceId' - The instance ID.
newInstance ::
  Instance
newInstance = Instance' {instanceId = Prelude.Nothing}

-- | The instance ID.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

instance Prelude.FromXML Instance where
  parseXML x =
    Instance' Prelude.<$> (x Prelude..@? "InstanceId")

instance Prelude.Hashable Instance

instance Prelude.NFData Instance

instance Prelude.ToQuery Instance where
  toQuery Instance' {..} =
    Prelude.mconcat
      ["InstanceId" Prelude.=: instanceId]
