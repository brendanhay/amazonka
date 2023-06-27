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
-- Module      : Amazonka.SecurityLake.Types.DataLakeLifecycleExpiration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeLifecycleExpiration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provide expiration lifecycle details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeLifecycleExpiration' smart constructor.
data DataLakeLifecycleExpiration = DataLakeLifecycleExpiration'
  { -- | Number of days before data expires in the Amazon Security Lake object.
    days :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeLifecycleExpiration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'dataLakeLifecycleExpiration_days' - Number of days before data expires in the Amazon Security Lake object.
newDataLakeLifecycleExpiration ::
  DataLakeLifecycleExpiration
newDataLakeLifecycleExpiration =
  DataLakeLifecycleExpiration'
    { days =
        Prelude.Nothing
    }

-- | Number of days before data expires in the Amazon Security Lake object.
dataLakeLifecycleExpiration_days :: Lens.Lens' DataLakeLifecycleExpiration (Prelude.Maybe Prelude.Natural)
dataLakeLifecycleExpiration_days = Lens.lens (\DataLakeLifecycleExpiration' {days} -> days) (\s@DataLakeLifecycleExpiration' {} a -> s {days = a} :: DataLakeLifecycleExpiration)

instance Data.FromJSON DataLakeLifecycleExpiration where
  parseJSON =
    Data.withObject
      "DataLakeLifecycleExpiration"
      ( \x ->
          DataLakeLifecycleExpiration'
            Prelude.<$> (x Data..:? "days")
      )

instance Prelude.Hashable DataLakeLifecycleExpiration where
  hashWithSalt _salt DataLakeLifecycleExpiration' {..} =
    _salt `Prelude.hashWithSalt` days

instance Prelude.NFData DataLakeLifecycleExpiration where
  rnf DataLakeLifecycleExpiration' {..} =
    Prelude.rnf days

instance Data.ToJSON DataLakeLifecycleExpiration where
  toJSON DataLakeLifecycleExpiration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("days" Data..=) Prelude.<$> days]
      )
