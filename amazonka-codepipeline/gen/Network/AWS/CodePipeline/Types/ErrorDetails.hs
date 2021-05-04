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
-- Module      : Network.AWS.CodePipeline.Types.ErrorDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ErrorDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about an error in AWS CodePipeline.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The text of the error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The system ID or number code of the error.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'errorDetails_message' - The text of the error message.
--
-- 'code', 'errorDetails_code' - The system ID or number code of the error.
newErrorDetails ::
  ErrorDetails
newErrorDetails =
  ErrorDetails'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The text of the error message.
errorDetails_message :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_message = Lens.lens (\ErrorDetails' {message} -> message) (\s@ErrorDetails' {} a -> s {message = a} :: ErrorDetails)

-- | The system ID or number code of the error.
errorDetails_code :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_code = Lens.lens (\ErrorDetails' {code} -> code) (\s@ErrorDetails' {} a -> s {code = a} :: ErrorDetails)

instance Prelude.FromJSON ErrorDetails where
  parseJSON =
    Prelude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "code")
      )

instance Prelude.Hashable ErrorDetails

instance Prelude.NFData ErrorDetails
