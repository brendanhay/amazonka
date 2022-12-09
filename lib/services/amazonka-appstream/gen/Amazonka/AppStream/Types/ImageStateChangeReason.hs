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
-- Module      : Amazonka.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImageStateChangeReason where

import Amazonka.AppStream.Types.ImageStateChangeReasonCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the reason why the last image state change occurred.
--
-- /See:/ 'newImageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { -- | The state change reason code.
    code :: Prelude.Maybe ImageStateChangeReasonCode,
    -- | The state change reason message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'imageStateChangeReason_code' - The state change reason code.
--
-- 'message', 'imageStateChangeReason_message' - The state change reason message.
newImageStateChangeReason ::
  ImageStateChangeReason
newImageStateChangeReason =
  ImageStateChangeReason'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The state change reason code.
imageStateChangeReason_code :: Lens.Lens' ImageStateChangeReason (Prelude.Maybe ImageStateChangeReasonCode)
imageStateChangeReason_code = Lens.lens (\ImageStateChangeReason' {code} -> code) (\s@ImageStateChangeReason' {} a -> s {code = a} :: ImageStateChangeReason)

-- | The state change reason message.
imageStateChangeReason_message :: Lens.Lens' ImageStateChangeReason (Prelude.Maybe Prelude.Text)
imageStateChangeReason_message = Lens.lens (\ImageStateChangeReason' {message} -> message) (\s@ImageStateChangeReason' {} a -> s {message = a} :: ImageStateChangeReason)

instance Data.FromJSON ImageStateChangeReason where
  parseJSON =
    Data.withObject
      "ImageStateChangeReason"
      ( \x ->
          ImageStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable ImageStateChangeReason where
  hashWithSalt _salt ImageStateChangeReason' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ImageStateChangeReason where
  rnf ImageStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
