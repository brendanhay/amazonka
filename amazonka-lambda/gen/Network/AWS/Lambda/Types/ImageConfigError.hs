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
-- Module      : Network.AWS.Lambda.Types.ImageConfigError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ImageConfigError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Error response to GetFunctionConfiguration.
--
-- /See:/ 'newImageConfigError' smart constructor.
data ImageConfigError = ImageConfigError'
  { -- | Error message.
    message :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImageConfigError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'imageConfigError_message' - Error message.
--
-- 'errorCode', 'imageConfigError_errorCode' - Error code.
newImageConfigError ::
  ImageConfigError
newImageConfigError =
  ImageConfigError'
    { message = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | Error message.
imageConfigError_message :: Lens.Lens' ImageConfigError (Prelude.Maybe Prelude.Text)
imageConfigError_message = Lens.lens (\ImageConfigError' {message} -> message) (\s@ImageConfigError' {} a -> s {message = a} :: ImageConfigError) Prelude.. Lens.mapping Prelude._Sensitive

-- | Error code.
imageConfigError_errorCode :: Lens.Lens' ImageConfigError (Prelude.Maybe Prelude.Text)
imageConfigError_errorCode = Lens.lens (\ImageConfigError' {errorCode} -> errorCode) (\s@ImageConfigError' {} a -> s {errorCode = a} :: ImageConfigError)

instance Prelude.FromJSON ImageConfigError where
  parseJSON =
    Prelude.withObject
      "ImageConfigError"
      ( \x ->
          ImageConfigError'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable ImageConfigError

instance Prelude.NFData ImageConfigError
