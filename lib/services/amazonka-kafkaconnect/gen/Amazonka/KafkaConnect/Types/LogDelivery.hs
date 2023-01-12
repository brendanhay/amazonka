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
-- Module      : Amazonka.KafkaConnect.Types.LogDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.LogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.WorkerLogDelivery
import qualified Amazonka.Prelude as Prelude

-- | Details about log delivery.
--
-- /See:/ 'newLogDelivery' smart constructor.
data LogDelivery = LogDelivery'
  { -- | The workers can send worker logs to different destination types. This
    -- configuration specifies the details of these destinations.
    workerLogDelivery :: WorkerLogDelivery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerLogDelivery', 'logDelivery_workerLogDelivery' - The workers can send worker logs to different destination types. This
-- configuration specifies the details of these destinations.
newLogDelivery ::
  -- | 'workerLogDelivery'
  WorkerLogDelivery ->
  LogDelivery
newLogDelivery pWorkerLogDelivery_ =
  LogDelivery'
    { workerLogDelivery =
        pWorkerLogDelivery_
    }

-- | The workers can send worker logs to different destination types. This
-- configuration specifies the details of these destinations.
logDelivery_workerLogDelivery :: Lens.Lens' LogDelivery WorkerLogDelivery
logDelivery_workerLogDelivery = Lens.lens (\LogDelivery' {workerLogDelivery} -> workerLogDelivery) (\s@LogDelivery' {} a -> s {workerLogDelivery = a} :: LogDelivery)

instance Prelude.Hashable LogDelivery where
  hashWithSalt _salt LogDelivery' {..} =
    _salt `Prelude.hashWithSalt` workerLogDelivery

instance Prelude.NFData LogDelivery where
  rnf LogDelivery' {..} = Prelude.rnf workerLogDelivery

instance Data.ToJSON LogDelivery where
  toJSON LogDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("workerLogDelivery" Data..= workerLogDelivery)
          ]
      )
