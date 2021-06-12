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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a failure a contributor insights operation.
--
-- /See:/ 'newFailureException' smart constructor.
data FailureException = FailureException'
  { -- | Exception name.
    exceptionName :: Core.Maybe Core.Text,
    -- | Description of the failure.
    exceptionDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { exceptionName = Core.Nothing,
      exceptionDescription = Core.Nothing
    }

-- | Exception name.
failureException_exceptionName :: Lens.Lens' FailureException (Core.Maybe Core.Text)
failureException_exceptionName = Lens.lens (\FailureException' {exceptionName} -> exceptionName) (\s@FailureException' {} a -> s {exceptionName = a} :: FailureException)

-- | Description of the failure.
failureException_exceptionDescription :: Lens.Lens' FailureException (Core.Maybe Core.Text)
failureException_exceptionDescription = Lens.lens (\FailureException' {exceptionDescription} -> exceptionDescription) (\s@FailureException' {} a -> s {exceptionDescription = a} :: FailureException)

instance Core.FromJSON FailureException where
  parseJSON =
    Core.withObject
      "FailureException"
      ( \x ->
          FailureException'
            Core.<$> (x Core..:? "ExceptionName")
            Core.<*> (x Core..:? "ExceptionDescription")
      )

instance Core.Hashable FailureException

instance Core.NFData FailureException
