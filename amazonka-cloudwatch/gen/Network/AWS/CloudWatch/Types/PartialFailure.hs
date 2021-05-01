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
-- Module      : Network.AWS.CloudWatch.Types.PartialFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.PartialFailure where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This array is empty if the API operation was successful for all the
-- rules specified in the request. If the operation could not process one
-- of the rules, the following data is returned for each of those rules.
--
-- /See:/ 'newPartialFailure' smart constructor.
data PartialFailure = PartialFailure'
  { -- | The type of error.
    exceptionType :: Prelude.Maybe Prelude.Text,
    -- | The code of the error.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | A description of the error.
    failureDescription :: Prelude.Maybe Prelude.Text,
    -- | The specified rule that could not be deleted.
    failureResource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PartialFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionType', 'partialFailure_exceptionType' - The type of error.
--
-- 'failureCode', 'partialFailure_failureCode' - The code of the error.
--
-- 'failureDescription', 'partialFailure_failureDescription' - A description of the error.
--
-- 'failureResource', 'partialFailure_failureResource' - The specified rule that could not be deleted.
newPartialFailure ::
  PartialFailure
newPartialFailure =
  PartialFailure'
    { exceptionType = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureDescription = Prelude.Nothing,
      failureResource = Prelude.Nothing
    }

-- | The type of error.
partialFailure_exceptionType :: Lens.Lens' PartialFailure (Prelude.Maybe Prelude.Text)
partialFailure_exceptionType = Lens.lens (\PartialFailure' {exceptionType} -> exceptionType) (\s@PartialFailure' {} a -> s {exceptionType = a} :: PartialFailure)

-- | The code of the error.
partialFailure_failureCode :: Lens.Lens' PartialFailure (Prelude.Maybe Prelude.Text)
partialFailure_failureCode = Lens.lens (\PartialFailure' {failureCode} -> failureCode) (\s@PartialFailure' {} a -> s {failureCode = a} :: PartialFailure)

-- | A description of the error.
partialFailure_failureDescription :: Lens.Lens' PartialFailure (Prelude.Maybe Prelude.Text)
partialFailure_failureDescription = Lens.lens (\PartialFailure' {failureDescription} -> failureDescription) (\s@PartialFailure' {} a -> s {failureDescription = a} :: PartialFailure)

-- | The specified rule that could not be deleted.
partialFailure_failureResource :: Lens.Lens' PartialFailure (Prelude.Maybe Prelude.Text)
partialFailure_failureResource = Lens.lens (\PartialFailure' {failureResource} -> failureResource) (\s@PartialFailure' {} a -> s {failureResource = a} :: PartialFailure)

instance Prelude.FromXML PartialFailure where
  parseXML x =
    PartialFailure'
      Prelude.<$> (x Prelude..@? "ExceptionType")
      Prelude.<*> (x Prelude..@? "FailureCode")
      Prelude.<*> (x Prelude..@? "FailureDescription")
      Prelude.<*> (x Prelude..@? "FailureResource")

instance Prelude.Hashable PartialFailure

instance Prelude.NFData PartialFailure
