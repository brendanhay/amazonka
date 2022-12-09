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
-- Module      : Amazonka.FSx.Types.DataRepositoryFailureDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryFailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides detailed information about the data repository if its
-- @Lifecycle@ is set to @MISCONFIGURED@ or @FAILED@.
--
-- /See:/ 'newDataRepositoryFailureDetails' smart constructor.
data DataRepositoryFailureDetails = DataRepositoryFailureDetails'
  { message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataRepositoryFailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'dataRepositoryFailureDetails_message' - Undocumented member.
newDataRepositoryFailureDetails ::
  DataRepositoryFailureDetails
newDataRepositoryFailureDetails =
  DataRepositoryFailureDetails'
    { message =
        Prelude.Nothing
    }

-- | Undocumented member.
dataRepositoryFailureDetails_message :: Lens.Lens' DataRepositoryFailureDetails (Prelude.Maybe Prelude.Text)
dataRepositoryFailureDetails_message = Lens.lens (\DataRepositoryFailureDetails' {message} -> message) (\s@DataRepositoryFailureDetails' {} a -> s {message = a} :: DataRepositoryFailureDetails)

instance Data.FromJSON DataRepositoryFailureDetails where
  parseJSON =
    Data.withObject
      "DataRepositoryFailureDetails"
      ( \x ->
          DataRepositoryFailureDetails'
            Prelude.<$> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    DataRepositoryFailureDetails
  where
  hashWithSalt _salt DataRepositoryFailureDetails' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData DataRepositoryFailureDetails where
  rnf DataRepositoryFailureDetails' {..} =
    Prelude.rnf message
