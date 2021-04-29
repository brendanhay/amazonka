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
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReason where

import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the reason why the last image state change occurred.
--
-- /See:/ 'newImageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { -- | The state change reason message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state change reason code.
    code :: Prelude.Maybe ImageStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The state change reason message.
imageStateChangeReason_message :: Lens.Lens' ImageStateChangeReason (Prelude.Maybe Prelude.Text)
imageStateChangeReason_message = Lens.lens (\ImageStateChangeReason' {message} -> message) (\s@ImageStateChangeReason' {} a -> s {message = a} :: ImageStateChangeReason)

-- | The state change reason code.
imageStateChangeReason_code :: Lens.Lens' ImageStateChangeReason (Prelude.Maybe ImageStateChangeReasonCode)
imageStateChangeReason_code = Lens.lens (\ImageStateChangeReason' {code} -> code) (\s@ImageStateChangeReason' {} a -> s {code = a} :: ImageStateChangeReason)

instance Prelude.FromJSON ImageStateChangeReason where
  parseJSON =
    Prelude.withObject
      "ImageStateChangeReason"
      ( \x ->
          ImageStateChangeReason'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable ImageStateChangeReason

instance Prelude.NFData ImageStateChangeReason
