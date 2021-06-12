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
-- Module      : Network.AWS.EC2.Types.ValidationWarning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationWarning where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ValidationError
import qualified Network.AWS.Lens as Lens

-- | The error codes and error messages that are returned for the parameters
-- or parameter combinations that are not valid when a new launch template
-- or new version of a launch template is created.
--
-- /See:/ 'newValidationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { -- | The error codes and error messages.
    errors :: Core.Maybe [ValidationError]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidationWarning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'validationWarning_errors' - The error codes and error messages.
newValidationWarning ::
  ValidationWarning
newValidationWarning =
  ValidationWarning' {errors = Core.Nothing}

-- | The error codes and error messages.
validationWarning_errors :: Lens.Lens' ValidationWarning (Core.Maybe [ValidationError])
validationWarning_errors = Lens.lens (\ValidationWarning' {errors} -> errors) (\s@ValidationWarning' {} a -> s {errors = a} :: ValidationWarning) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ValidationWarning where
  parseXML x =
    ValidationWarning'
      Core.<$> ( x Core..@? "errorSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable ValidationWarning

instance Core.NFData ValidationWarning
