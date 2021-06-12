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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorType :: Core.Maybe Core.Text,
    errorMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { errorType = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | Undocumented member.
errorDetails_errorType :: Lens.Lens' ErrorDetails (Core.Maybe Core.Text)
errorDetails_errorType = Lens.lens (\ErrorDetails' {errorType} -> errorType) (\s@ErrorDetails' {} a -> s {errorType = a} :: ErrorDetails)

-- | Undocumented member.
errorDetails_errorMessage :: Lens.Lens' ErrorDetails (Core.Maybe Core.Text)
errorDetails_errorMessage = Lens.lens (\ErrorDetails' {errorMessage} -> errorMessage) (\s@ErrorDetails' {} a -> s {errorMessage = a} :: ErrorDetails)

instance Core.FromJSON ErrorDetails where
  parseJSON =
    Core.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Core.<$> (x Core..:? "ErrorType")
            Core.<*> (x Core..:? "ErrorMessage")
      )

instance Core.Hashable ErrorDetails

instance Core.NFData ErrorDetails
