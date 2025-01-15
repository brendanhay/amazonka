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
-- Module      : Amazonka.DynamoDB.Types.FailureException
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.FailureException where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a failure a contributor insights operation.
--
-- /See:/ 'newFailureException' smart constructor.
data FailureException = FailureException'
  { -- | Description of the failure.
    exceptionDescription :: Prelude.Maybe Prelude.Text,
    -- | Exception name.
    exceptionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionDescription', 'failureException_exceptionDescription' - Description of the failure.
--
-- 'exceptionName', 'failureException_exceptionName' - Exception name.
newFailureException ::
  FailureException
newFailureException =
  FailureException'
    { exceptionDescription =
        Prelude.Nothing,
      exceptionName = Prelude.Nothing
    }

-- | Description of the failure.
failureException_exceptionDescription :: Lens.Lens' FailureException (Prelude.Maybe Prelude.Text)
failureException_exceptionDescription = Lens.lens (\FailureException' {exceptionDescription} -> exceptionDescription) (\s@FailureException' {} a -> s {exceptionDescription = a} :: FailureException)

-- | Exception name.
failureException_exceptionName :: Lens.Lens' FailureException (Prelude.Maybe Prelude.Text)
failureException_exceptionName = Lens.lens (\FailureException' {exceptionName} -> exceptionName) (\s@FailureException' {} a -> s {exceptionName = a} :: FailureException)

instance Data.FromJSON FailureException where
  parseJSON =
    Data.withObject
      "FailureException"
      ( \x ->
          FailureException'
            Prelude.<$> (x Data..:? "ExceptionDescription")
            Prelude.<*> (x Data..:? "ExceptionName")
      )

instance Prelude.Hashable FailureException where
  hashWithSalt _salt FailureException' {..} =
    _salt
      `Prelude.hashWithSalt` exceptionDescription
      `Prelude.hashWithSalt` exceptionName

instance Prelude.NFData FailureException where
  rnf FailureException' {..} =
    Prelude.rnf exceptionDescription `Prelude.seq`
      Prelude.rnf exceptionName
