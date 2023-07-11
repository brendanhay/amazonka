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
-- Module      : Amazonka.Pinpoint.Types.VerificationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VerificationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Verify OTP Message Response.
--
-- /See:/ 'newVerificationResponse' smart constructor.
data VerificationResponse = VerificationResponse'
  { -- | Specifies whether the OTP is valid or not.
    valid :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valid', 'verificationResponse_valid' - Specifies whether the OTP is valid or not.
newVerificationResponse ::
  VerificationResponse
newVerificationResponse =
  VerificationResponse' {valid = Prelude.Nothing}

-- | Specifies whether the OTP is valid or not.
verificationResponse_valid :: Lens.Lens' VerificationResponse (Prelude.Maybe Prelude.Bool)
verificationResponse_valid = Lens.lens (\VerificationResponse' {valid} -> valid) (\s@VerificationResponse' {} a -> s {valid = a} :: VerificationResponse)

instance Data.FromJSON VerificationResponse where
  parseJSON =
    Data.withObject
      "VerificationResponse"
      ( \x ->
          VerificationResponse'
            Prelude.<$> (x Data..:? "Valid")
      )

instance Prelude.Hashable VerificationResponse where
  hashWithSalt _salt VerificationResponse' {..} =
    _salt `Prelude.hashWithSalt` valid

instance Prelude.NFData VerificationResponse where
  rnf VerificationResponse' {..} = Prelude.rnf valid
