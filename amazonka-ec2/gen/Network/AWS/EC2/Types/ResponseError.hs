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
-- Module      : Network.AWS.EC2.Types.ResponseError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResponseError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateErrorCode
import qualified Network.AWS.Lens as Lens

-- | Describes the error that\'s returned when you cannot delete a launch
-- template version.
--
-- /See:/ 'newResponseError' smart constructor.
data ResponseError = ResponseError'
  { -- | The error message, if applicable.
    message :: Core.Maybe Core.Text,
    -- | The error code.
    code :: Core.Maybe LaunchTemplateErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResponseError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'responseError_message' - The error message, if applicable.
--
-- 'code', 'responseError_code' - The error code.
newResponseError ::
  ResponseError
newResponseError =
  ResponseError'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The error message, if applicable.
responseError_message :: Lens.Lens' ResponseError (Core.Maybe Core.Text)
responseError_message = Lens.lens (\ResponseError' {message} -> message) (\s@ResponseError' {} a -> s {message = a} :: ResponseError)

-- | The error code.
responseError_code :: Lens.Lens' ResponseError (Core.Maybe LaunchTemplateErrorCode)
responseError_code = Lens.lens (\ResponseError' {code} -> code) (\s@ResponseError' {} a -> s {code = a} :: ResponseError)

instance Core.FromXML ResponseError where
  parseXML x =
    ResponseError'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance Core.Hashable ResponseError

instance Core.NFData ResponseError
