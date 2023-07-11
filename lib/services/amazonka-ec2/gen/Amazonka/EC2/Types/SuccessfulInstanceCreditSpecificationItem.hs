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
-- Module      : Amazonka.EC2.Types.SuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SuccessfulInstanceCreditSpecificationItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the burstable performance instance whose credit option for CPU
-- usage was successfully modified.
--
-- /See:/ 'newSuccessfulInstanceCreditSpecificationItem' smart constructor.
data SuccessfulInstanceCreditSpecificationItem = SuccessfulInstanceCreditSpecificationItem'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessfulInstanceCreditSpecificationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'successfulInstanceCreditSpecificationItem_instanceId' - The ID of the instance.
newSuccessfulInstanceCreditSpecificationItem ::
  SuccessfulInstanceCreditSpecificationItem
newSuccessfulInstanceCreditSpecificationItem =
  SuccessfulInstanceCreditSpecificationItem'
    { instanceId =
        Prelude.Nothing
    }

-- | The ID of the instance.
successfulInstanceCreditSpecificationItem_instanceId :: Lens.Lens' SuccessfulInstanceCreditSpecificationItem (Prelude.Maybe Prelude.Text)
successfulInstanceCreditSpecificationItem_instanceId = Lens.lens (\SuccessfulInstanceCreditSpecificationItem' {instanceId} -> instanceId) (\s@SuccessfulInstanceCreditSpecificationItem' {} a -> s {instanceId = a} :: SuccessfulInstanceCreditSpecificationItem)

instance
  Data.FromXML
    SuccessfulInstanceCreditSpecificationItem
  where
  parseXML x =
    SuccessfulInstanceCreditSpecificationItem'
      Prelude.<$> (x Data..@? "instanceId")

instance
  Prelude.Hashable
    SuccessfulInstanceCreditSpecificationItem
  where
  hashWithSalt
    _salt
    SuccessfulInstanceCreditSpecificationItem' {..} =
      _salt `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    SuccessfulInstanceCreditSpecificationItem
  where
  rnf SuccessfulInstanceCreditSpecificationItem' {..} =
    Prelude.rnf instanceId
