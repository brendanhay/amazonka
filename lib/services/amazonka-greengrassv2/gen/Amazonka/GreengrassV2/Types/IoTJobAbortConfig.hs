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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobAbortConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobAbortConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.IoTJobAbortCriteria
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of criteria that define when and how to cancel a
-- configuration deployment.
--
-- /See:/ 'newIoTJobAbortConfig' smart constructor.
data IoTJobAbortConfig = IoTJobAbortConfig'
  { -- | The list of criteria that define when and how to cancel the
    -- configuration deployment.
    criteriaList :: Prelude.NonEmpty IoTJobAbortCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IoTJobAbortConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criteriaList', 'ioTJobAbortConfig_criteriaList' - The list of criteria that define when and how to cancel the
-- configuration deployment.
newIoTJobAbortConfig ::
  -- | 'criteriaList'
  Prelude.NonEmpty IoTJobAbortCriteria ->
  IoTJobAbortConfig
newIoTJobAbortConfig pCriteriaList_ =
  IoTJobAbortConfig'
    { criteriaList =
        Lens.coerced Lens.# pCriteriaList_
    }

-- | The list of criteria that define when and how to cancel the
-- configuration deployment.
ioTJobAbortConfig_criteriaList :: Lens.Lens' IoTJobAbortConfig (Prelude.NonEmpty IoTJobAbortCriteria)
ioTJobAbortConfig_criteriaList = Lens.lens (\IoTJobAbortConfig' {criteriaList} -> criteriaList) (\s@IoTJobAbortConfig' {} a -> s {criteriaList = a} :: IoTJobAbortConfig) Prelude.. Lens.coerced

instance Data.FromJSON IoTJobAbortConfig where
  parseJSON =
    Data.withObject
      "IoTJobAbortConfig"
      ( \x ->
          IoTJobAbortConfig'
            Prelude.<$> (x Data..: "criteriaList")
      )

instance Prelude.Hashable IoTJobAbortConfig where
  hashWithSalt _salt IoTJobAbortConfig' {..} =
    _salt `Prelude.hashWithSalt` criteriaList

instance Prelude.NFData IoTJobAbortConfig where
  rnf IoTJobAbortConfig' {..} = Prelude.rnf criteriaList

instance Data.ToJSON IoTJobAbortConfig where
  toJSON IoTJobAbortConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("criteriaList" Data..= criteriaList)]
      )
