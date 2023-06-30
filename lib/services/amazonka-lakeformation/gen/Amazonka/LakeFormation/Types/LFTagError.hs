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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTagError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.ErrorDetail
import Amazonka.LakeFormation.Types.LFTagPair
import qualified Amazonka.Prelude as Prelude

-- | A structure containing an error related to a @TagResource@ or
-- @UnTagResource@ operation.
--
-- /See:/ 'newLFTagError' smart constructor.
data LFTagError = LFTagError'
  { -- | An error that occurred with the attachment or detachment of the LF-tag.
    error :: Prelude.Maybe ErrorDetail,
    -- | The key-name of the LF-tag.
    lFTag :: Prelude.Maybe LFTagPair
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
-- 'error', 'lFTagError_error' - An error that occurred with the attachment or detachment of the LF-tag.
--
-- 'lFTag', 'lFTagError_lFTag' - The key-name of the LF-tag.
newLFTagError ::
  LFTagError
newLFTagError =
  LFTagError'
    { error = Prelude.Nothing,
      lFTag = Prelude.Nothing
    }

-- | An error that occurred with the attachment or detachment of the LF-tag.
lFTagError_error :: Lens.Lens' LFTagError (Prelude.Maybe ErrorDetail)
lFTagError_error = Lens.lens (\LFTagError' {error} -> error) (\s@LFTagError' {} a -> s {error = a} :: LFTagError)

-- | The key-name of the LF-tag.
lFTagError_lFTag :: Lens.Lens' LFTagError (Prelude.Maybe LFTagPair)
lFTagError_lFTag = Lens.lens (\LFTagError' {lFTag} -> lFTag) (\s@LFTagError' {} a -> s {lFTag = a} :: LFTagError)

instance Data.FromJSON LFTagError where
  parseJSON =
    Data.withObject
      "LFTagError"
      ( \x ->
          LFTagError'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "LFTag")
      )

instance Prelude.Hashable LFTagError where
  hashWithSalt _salt LFTagError' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` lFTag

instance Prelude.NFData LFTagError where
  rnf LFTagError' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf lFTag
