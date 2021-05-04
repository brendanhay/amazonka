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
-- Module      : Network.AWS.DynamoDB.Types.FailureException
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.FailureException where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a failure a contributor insights operation.
--
-- /See:/ 'newFailureException' smart constructor.
data FailureException = FailureException'
  { -- | Exception name.
    exceptionName :: Prelude.Maybe Prelude.Text,
    -- | Description of the failure.
    exceptionDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailureException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionName', 'failureException_exceptionName' - Exception name.
--
-- 'exceptionDescription', 'failureException_exceptionDescription' - Description of the failure.
newFailureException ::
  FailureException
newFailureException =
  FailureException'
    { exceptionName = Prelude.Nothing,
      exceptionDescription = Prelude.Nothing
    }

-- | Exception name.
failureException_exceptionName :: Lens.Lens' FailureException (Prelude.Maybe Prelude.Text)
failureException_exceptionName = Lens.lens (\FailureException' {exceptionName} -> exceptionName) (\s@FailureException' {} a -> s {exceptionName = a} :: FailureException)

-- | Description of the failure.
failureException_exceptionDescription :: Lens.Lens' FailureException (Prelude.Maybe Prelude.Text)
failureException_exceptionDescription = Lens.lens (\FailureException' {exceptionDescription} -> exceptionDescription) (\s@FailureException' {} a -> s {exceptionDescription = a} :: FailureException)

instance Prelude.FromJSON FailureException where
  parseJSON =
    Prelude.withObject
      "FailureException"
      ( \x ->
          FailureException'
            Prelude.<$> (x Prelude..:? "ExceptionName")
            Prelude.<*> (x Prelude..:? "ExceptionDescription")
      )

instance Prelude.Hashable FailureException

instance Prelude.NFData FailureException
