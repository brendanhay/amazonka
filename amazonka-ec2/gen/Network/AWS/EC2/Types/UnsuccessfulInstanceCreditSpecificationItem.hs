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
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import qualified Network.AWS.Lens as Lens

-- | Describes the burstable performance instance whose credit option for CPU
-- usage was not modified.
--
-- /See:/ 'newUnsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The applicable error for the burstable performance instance whose credit
    -- option for CPU usage was not modified.
    error :: Core.Maybe UnsuccessfulInstanceCreditSpecificationItemError
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsuccessfulInstanceCreditSpecificationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'unsuccessfulInstanceCreditSpecificationItem_instanceId' - The ID of the instance.
--
-- 'error', 'unsuccessfulInstanceCreditSpecificationItem_error' - The applicable error for the burstable performance instance whose credit
-- option for CPU usage was not modified.
newUnsuccessfulInstanceCreditSpecificationItem ::
  UnsuccessfulInstanceCreditSpecificationItem
newUnsuccessfulInstanceCreditSpecificationItem =
  UnsuccessfulInstanceCreditSpecificationItem'
    { instanceId =
        Core.Nothing,
      error = Core.Nothing
    }

-- | The ID of the instance.
unsuccessfulInstanceCreditSpecificationItem_instanceId :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Core.Maybe Core.Text)
unsuccessfulInstanceCreditSpecificationItem_instanceId = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItem' {instanceId} -> instanceId) (\s@UnsuccessfulInstanceCreditSpecificationItem' {} a -> s {instanceId = a} :: UnsuccessfulInstanceCreditSpecificationItem)

-- | The applicable error for the burstable performance instance whose credit
-- option for CPU usage was not modified.
unsuccessfulInstanceCreditSpecificationItem_error :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Core.Maybe UnsuccessfulInstanceCreditSpecificationItemError)
unsuccessfulInstanceCreditSpecificationItem_error = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItem' {error} -> error) (\s@UnsuccessfulInstanceCreditSpecificationItem' {} a -> s {error = a} :: UnsuccessfulInstanceCreditSpecificationItem)

instance
  Core.FromXML
    UnsuccessfulInstanceCreditSpecificationItem
  where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItem'
      Core.<$> (x Core..@? "instanceId")
        Core.<*> (x Core..@? "error")

instance
  Core.Hashable
    UnsuccessfulInstanceCreditSpecificationItem

instance
  Core.NFData
    UnsuccessfulInstanceCreditSpecificationItem
