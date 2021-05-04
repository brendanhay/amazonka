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
-- Module      : Network.AWS.ElasticSearch.Types.ErrorDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ErrorDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorType :: Prelude.Maybe Prelude.Text,
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'errorType', 'errorDetails_errorType' - Undocumented member.
--
-- 'errorMessage', 'errorDetails_errorMessage' - Undocumented member.
newErrorDetails ::
  ErrorDetails
newErrorDetails =
  ErrorDetails'
    { errorType = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Undocumented member.
errorDetails_errorType :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_errorType = Lens.lens (\ErrorDetails' {errorType} -> errorType) (\s@ErrorDetails' {} a -> s {errorType = a} :: ErrorDetails)

-- | Undocumented member.
errorDetails_errorMessage :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_errorMessage = Lens.lens (\ErrorDetails' {errorMessage} -> errorMessage) (\s@ErrorDetails' {} a -> s {errorMessage = a} :: ErrorDetails)

instance Prelude.FromJSON ErrorDetails where
  parseJSON =
    Prelude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Prelude.<$> (x Prelude..:? "ErrorType")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
      )

instance Prelude.Hashable ErrorDetails

instance Prelude.NFData ErrorDetails
