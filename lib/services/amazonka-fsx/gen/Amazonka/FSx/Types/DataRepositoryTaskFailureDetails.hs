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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskFailureDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskFailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about why a data repository task failed. Only
-- populated when the task @Lifecycle@ is set to @FAILED@.
--
-- /See:/ 'newDataRepositoryTaskFailureDetails' smart constructor.
data DataRepositoryTaskFailureDetails = DataRepositoryTaskFailureDetails'
  { message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryTaskFailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'dataRepositoryTaskFailureDetails_message' - Undocumented member.
newDataRepositoryTaskFailureDetails ::
  DataRepositoryTaskFailureDetails
newDataRepositoryTaskFailureDetails =
  DataRepositoryTaskFailureDetails'
    { message =
        Prelude.Nothing
    }

-- | Undocumented member.
dataRepositoryTaskFailureDetails_message :: Lens.Lens' DataRepositoryTaskFailureDetails (Prelude.Maybe Prelude.Text)
dataRepositoryTaskFailureDetails_message = Lens.lens (\DataRepositoryTaskFailureDetails' {message} -> message) (\s@DataRepositoryTaskFailureDetails' {} a -> s {message = a} :: DataRepositoryTaskFailureDetails)

instance
  Core.FromJSON
    DataRepositoryTaskFailureDetails
  where
  parseJSON =
    Core.withObject
      "DataRepositoryTaskFailureDetails"
      ( \x ->
          DataRepositoryTaskFailureDetails'
            Prelude.<$> (x Core..:? "Message")
      )

instance
  Prelude.Hashable
    DataRepositoryTaskFailureDetails
  where
  hashWithSalt
    _salt
    DataRepositoryTaskFailureDetails' {..} =
      _salt `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    DataRepositoryTaskFailureDetails
  where
  rnf DataRepositoryTaskFailureDetails' {..} =
    Prelude.rnf message
