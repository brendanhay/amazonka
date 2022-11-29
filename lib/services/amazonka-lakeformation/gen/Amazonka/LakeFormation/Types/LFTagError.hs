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
-- Module      : Amazonka.LakeFormation.Types.LFTagError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTagError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.ErrorDetail
import Amazonka.LakeFormation.Types.LFTagPair
import qualified Amazonka.Prelude as Prelude

-- | A structure containing an error related to a @TagResource@ or
-- @UnTagResource@ operation.
--
-- /See:/ 'newLFTagError' smart constructor.
data LFTagError = LFTagError'
  { -- | The key-name of the LF-tag.
    lFTag :: Prelude.Maybe LFTagPair,
    -- | An error that occurred with the attachment or detachment of the LF-tag.
    error :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTagError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lFTag', 'lFTagError_lFTag' - The key-name of the LF-tag.
--
-- 'error', 'lFTagError_error' - An error that occurred with the attachment or detachment of the LF-tag.
newLFTagError ::
  LFTagError
newLFTagError =
  LFTagError'
    { lFTag = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The key-name of the LF-tag.
lFTagError_lFTag :: Lens.Lens' LFTagError (Prelude.Maybe LFTagPair)
lFTagError_lFTag = Lens.lens (\LFTagError' {lFTag} -> lFTag) (\s@LFTagError' {} a -> s {lFTag = a} :: LFTagError)

-- | An error that occurred with the attachment or detachment of the LF-tag.
lFTagError_error :: Lens.Lens' LFTagError (Prelude.Maybe ErrorDetail)
lFTagError_error = Lens.lens (\LFTagError' {error} -> error) (\s@LFTagError' {} a -> s {error = a} :: LFTagError)

instance Core.FromJSON LFTagError where
  parseJSON =
    Core.withObject
      "LFTagError"
      ( \x ->
          LFTagError'
            Prelude.<$> (x Core..:? "LFTag")
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable LFTagError where
  hashWithSalt _salt LFTagError' {..} =
    _salt `Prelude.hashWithSalt` lFTag
      `Prelude.hashWithSalt` error

instance Prelude.NFData LFTagError where
  rnf LFTagError' {..} =
    Prelude.rnf lFTag `Prelude.seq` Prelude.rnf error
