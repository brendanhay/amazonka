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
-- Module      : Network.AWS.IoT.Types.HttpAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpAuthorization where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.SigV4Authorization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON HttpAuthorization where
  parseJSON =
    Core.withObject
      "HttpAuthorization"
      ( \x ->
          HttpAuthorization' Prelude.<$> (x Core..:? "sigv4")
      )

instance Prelude.Hashable HttpAuthorization

instance Prelude.NFData HttpAuthorization

instance Core.ToJSON HttpAuthorization where
  toJSON HttpAuthorization' {..} =
    Core.object
      ( Prelude.catMaybes
          [("sigv4" Core..=) Prelude.<$> sigv4]
      )
