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
-- Module      : Amazonka.AppStream.Types.ImageBuilderStateChangeReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImageBuilderStateChangeReason where

import Amazonka.AppStream.Types.ImageBuilderStateChangeReasonCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the reason why the last image builder state change occurred.
--
-- /See:/ 'newImageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { -- | The state change reason message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state change reason code.
    code :: Prelude.Maybe ImageBuilderStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageBuilderStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'imageBuilderStateChangeReason_message' - The state change reason message.
--
-- 'code', 'imageBuilderStateChangeReason_code' - The state change reason code.
newImageBuilderStateChangeReason ::
  ImageBuilderStateChangeReason
newImageBuilderStateChangeReason =
  ImageBuilderStateChangeReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The state change reason message.
imageBuilderStateChangeReason_message :: Lens.Lens' ImageBuilderStateChangeReason (Prelude.Maybe Prelude.Text)
imageBuilderStateChangeReason_message = Lens.lens (\ImageBuilderStateChangeReason' {message} -> message) (\s@ImageBuilderStateChangeReason' {} a -> s {message = a} :: ImageBuilderStateChangeReason)

-- | The state change reason code.
imageBuilderStateChangeReason_code :: Lens.Lens' ImageBuilderStateChangeReason (Prelude.Maybe ImageBuilderStateChangeReasonCode)
imageBuilderStateChangeReason_code = Lens.lens (\ImageBuilderStateChangeReason' {code} -> code) (\s@ImageBuilderStateChangeReason' {} a -> s {code = a} :: ImageBuilderStateChangeReason)

instance Core.FromJSON ImageBuilderStateChangeReason where
  parseJSON =
    Core.withObject
      "ImageBuilderStateChangeReason"
      ( \x ->
          ImageBuilderStateChangeReason'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Code")
      )

instance
  Prelude.Hashable
    ImageBuilderStateChangeReason
  where
  hashWithSalt _salt ImageBuilderStateChangeReason' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData ImageBuilderStateChangeReason where
  rnf ImageBuilderStateChangeReason' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
