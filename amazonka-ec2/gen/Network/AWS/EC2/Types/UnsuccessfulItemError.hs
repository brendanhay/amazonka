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
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItemError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItemError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the error that occurred. For more information about
-- errors, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes>.
--
-- /See:/ 'newUnsuccessfulItemError' smart constructor.
data UnsuccessfulItemError = UnsuccessfulItemError'
  { -- | The error message accompanying the error code.
    message :: Core.Maybe Core.Text,
    -- | The error code.
    code :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsuccessfulItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'unsuccessfulItemError_message' - The error message accompanying the error code.
--
-- 'code', 'unsuccessfulItemError_code' - The error code.
newUnsuccessfulItemError ::
  UnsuccessfulItemError
newUnsuccessfulItemError =
  UnsuccessfulItemError'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The error message accompanying the error code.
unsuccessfulItemError_message :: Lens.Lens' UnsuccessfulItemError (Core.Maybe Core.Text)
unsuccessfulItemError_message = Lens.lens (\UnsuccessfulItemError' {message} -> message) (\s@UnsuccessfulItemError' {} a -> s {message = a} :: UnsuccessfulItemError)

-- | The error code.
unsuccessfulItemError_code :: Lens.Lens' UnsuccessfulItemError (Core.Maybe Core.Text)
unsuccessfulItemError_code = Lens.lens (\UnsuccessfulItemError' {code} -> code) (\s@UnsuccessfulItemError' {} a -> s {code = a} :: UnsuccessfulItemError)

instance Core.FromXML UnsuccessfulItemError where
  parseXML x =
    UnsuccessfulItemError'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance Core.Hashable UnsuccessfulItemError

instance Core.NFData UnsuccessfulItemError
