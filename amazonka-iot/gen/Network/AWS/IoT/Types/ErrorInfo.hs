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
-- Module      : Network.AWS.IoT.Types.ErrorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ErrorInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Error information.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'errorInfo_message' - The error message.
--
-- 'code', 'errorInfo_code' - The error code.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message.
errorInfo_message :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_message = Lens.lens (\ErrorInfo' {message} -> message) (\s@ErrorInfo' {} a -> s {message = a} :: ErrorInfo)

-- | The error code.
errorInfo_code :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_code = Lens.lens (\ErrorInfo' {code} -> code) (\s@ErrorInfo' {} a -> s {code = a} :: ErrorInfo)

instance Prelude.FromJSON ErrorInfo where
  parseJSON =
    Prelude.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "code")
      )

instance Prelude.Hashable ErrorInfo

instance Prelude.NFData ErrorInfo
