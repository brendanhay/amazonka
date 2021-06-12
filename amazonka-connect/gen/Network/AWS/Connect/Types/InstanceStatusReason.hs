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
-- Module      : Network.AWS.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatusReason where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Relevant details why the instance was not successfully created.
--
-- /See:/ 'newInstanceStatusReason' smart constructor.
data InstanceStatusReason = InstanceStatusReason'
  { -- | The message.
    message :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceStatusReason_message' - The message.
newInstanceStatusReason ::
  InstanceStatusReason
newInstanceStatusReason =
  InstanceStatusReason' {message = Core.Nothing}

-- | The message.
instanceStatusReason_message :: Lens.Lens' InstanceStatusReason (Core.Maybe Core.Text)
instanceStatusReason_message = Lens.lens (\InstanceStatusReason' {message} -> message) (\s@InstanceStatusReason' {} a -> s {message = a} :: InstanceStatusReason)

instance Core.FromJSON InstanceStatusReason where
  parseJSON =
    Core.withObject
      "InstanceStatusReason"
      ( \x ->
          InstanceStatusReason'
            Core.<$> (x Core..:? "Message")
      )

instance Core.Hashable InstanceStatusReason

instance Core.NFData InstanceStatusReason
