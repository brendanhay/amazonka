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
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReason where

import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the reason why the last image state change occurred.
--
-- /See:/ 'newImageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { -- | The state change reason message.
    message :: Core.Maybe Core.Text,
    -- | The state change reason code.
    code :: Core.Maybe ImageStateChangeReasonCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'imageStateChangeReason_message' - The state change reason message.
--
-- 'code', 'imageStateChangeReason_code' - The state change reason code.
newImageStateChangeReason ::
  ImageStateChangeReason
newImageStateChangeReason =
  ImageStateChangeReason'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The state change reason message.
imageStateChangeReason_message :: Lens.Lens' ImageStateChangeReason (Core.Maybe Core.Text)
imageStateChangeReason_message = Lens.lens (\ImageStateChangeReason' {message} -> message) (\s@ImageStateChangeReason' {} a -> s {message = a} :: ImageStateChangeReason)

-- | The state change reason code.
imageStateChangeReason_code :: Lens.Lens' ImageStateChangeReason (Core.Maybe ImageStateChangeReasonCode)
imageStateChangeReason_code = Lens.lens (\ImageStateChangeReason' {code} -> code) (\s@ImageStateChangeReason' {} a -> s {code = a} :: ImageStateChangeReason)

instance Core.FromJSON ImageStateChangeReason where
  parseJSON =
    Core.withObject
      "ImageStateChangeReason"
      ( \x ->
          ImageStateChangeReason'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance Core.Hashable ImageStateChangeReason

instance Core.NFData ImageStateChangeReason
