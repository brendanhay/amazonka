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
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the burstable performance instance whose credit option for CPU
-- usage was not modified.
--
-- /See:/ 'newUnsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The applicable error for the burstable performance instance whose credit
    -- option for CPU usage was not modified.
    error :: Prelude.Maybe UnsuccessfulInstanceCreditSpecificationItemError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The ID of the instance.
unsuccessfulInstanceCreditSpecificationItem_instanceId :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Prelude.Maybe Prelude.Text)
unsuccessfulInstanceCreditSpecificationItem_instanceId = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItem' {instanceId} -> instanceId) (\s@UnsuccessfulInstanceCreditSpecificationItem' {} a -> s {instanceId = a} :: UnsuccessfulInstanceCreditSpecificationItem)

-- | The applicable error for the burstable performance instance whose credit
-- option for CPU usage was not modified.
unsuccessfulInstanceCreditSpecificationItem_error :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Prelude.Maybe UnsuccessfulInstanceCreditSpecificationItemError)
unsuccessfulInstanceCreditSpecificationItem_error = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItem' {error} -> error) (\s@UnsuccessfulInstanceCreditSpecificationItem' {} a -> s {error = a} :: UnsuccessfulInstanceCreditSpecificationItem)

instance
  Prelude.FromXML
    UnsuccessfulInstanceCreditSpecificationItem
  where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItem'
      Prelude.<$> (x Prelude..@? "instanceId")
        Prelude.<*> (x Prelude..@? "error")

instance
  Prelude.Hashable
    UnsuccessfulInstanceCreditSpecificationItem

instance
  Prelude.NFData
    UnsuccessfulInstanceCreditSpecificationItem
