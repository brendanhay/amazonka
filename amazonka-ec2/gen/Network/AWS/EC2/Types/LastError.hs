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
-- Module      : Network.AWS.EC2.Types.LastError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LastError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The last error that occurred for a VPC endpoint.
--
-- /See:/ 'newLastError' smart constructor.
data LastError = LastError'
  { -- | The error message for the VPC endpoint error.
    message :: Core.Maybe Core.Text,
    -- | The error code for the VPC endpoint error.
    code :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LastError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'lastError_message' - The error message for the VPC endpoint error.
--
-- 'code', 'lastError_code' - The error code for the VPC endpoint error.
newLastError ::
  LastError
newLastError =
  LastError'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The error message for the VPC endpoint error.
lastError_message :: Lens.Lens' LastError (Core.Maybe Core.Text)
lastError_message = Lens.lens (\LastError' {message} -> message) (\s@LastError' {} a -> s {message = a} :: LastError)

-- | The error code for the VPC endpoint error.
lastError_code :: Lens.Lens' LastError (Core.Maybe Core.Text)
lastError_code = Lens.lens (\LastError' {code} -> code) (\s@LastError' {} a -> s {code = a} :: LastError)

instance Core.FromXML LastError where
  parseXML x =
    LastError'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance Core.Hashable LastError

instance Core.NFData LastError
