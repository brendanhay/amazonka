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
-- Module      : Amazonka.IoT.Types.HttpAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpAuthorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.SigV4Authorization
import qualified Amazonka.Prelude as Prelude

-- | The authorization method used to send messages.
--
-- /See:/ 'newHttpAuthorization' smart constructor.
data HttpAuthorization = HttpAuthorization'
  { -- | Use Sig V4 authorization. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    sigv4 :: Prelude.Maybe SigV4Authorization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sigv4', 'httpAuthorization_sigv4' - Use Sig V4 authorization. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
newHttpAuthorization ::
  HttpAuthorization
newHttpAuthorization =
  HttpAuthorization' {sigv4 = Prelude.Nothing}

-- | Use Sig V4 authorization. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
httpAuthorization_sigv4 :: Lens.Lens' HttpAuthorization (Prelude.Maybe SigV4Authorization)
httpAuthorization_sigv4 = Lens.lens (\HttpAuthorization' {sigv4} -> sigv4) (\s@HttpAuthorization' {} a -> s {sigv4 = a} :: HttpAuthorization)

instance Data.FromJSON HttpAuthorization where
  parseJSON =
    Data.withObject
      "HttpAuthorization"
      ( \x ->
          HttpAuthorization' Prelude.<$> (x Data..:? "sigv4")
      )

instance Prelude.Hashable HttpAuthorization where
  hashWithSalt _salt HttpAuthorization' {..} =
    _salt `Prelude.hashWithSalt` sigv4

instance Prelude.NFData HttpAuthorization where
  rnf HttpAuthorization' {..} = Prelude.rnf sigv4

instance Data.ToJSON HttpAuthorization where
  toJSON HttpAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sigv4" Data..=) Prelude.<$> sigv4]
      )
