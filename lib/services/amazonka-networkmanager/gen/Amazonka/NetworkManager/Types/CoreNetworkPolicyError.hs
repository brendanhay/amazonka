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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkPolicyError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkPolicyError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details about an error in a core network policy.
--
-- /See:/ 'newCoreNetworkPolicyError' smart constructor.
data CoreNetworkPolicyError = CoreNetworkPolicyError'
  { -- | The JSON path where the error was discovered in the policy document.
    path :: Prelude.Maybe Prelude.Text,
    -- | The error code associated with a core network policy error.
    errorCode :: Prelude.Text,
    -- | The message associated with a core network policy error code.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkPolicyError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'coreNetworkPolicyError_path' - The JSON path where the error was discovered in the policy document.
--
-- 'errorCode', 'coreNetworkPolicyError_errorCode' - The error code associated with a core network policy error.
--
-- 'message', 'coreNetworkPolicyError_message' - The message associated with a core network policy error code.
newCoreNetworkPolicyError ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  CoreNetworkPolicyError
newCoreNetworkPolicyError pErrorCode_ pMessage_ =
  CoreNetworkPolicyError'
    { path = Prelude.Nothing,
      errorCode = pErrorCode_,
      message = pMessage_
    }

-- | The JSON path where the error was discovered in the policy document.
coreNetworkPolicyError_path :: Lens.Lens' CoreNetworkPolicyError (Prelude.Maybe Prelude.Text)
coreNetworkPolicyError_path = Lens.lens (\CoreNetworkPolicyError' {path} -> path) (\s@CoreNetworkPolicyError' {} a -> s {path = a} :: CoreNetworkPolicyError)

-- | The error code associated with a core network policy error.
coreNetworkPolicyError_errorCode :: Lens.Lens' CoreNetworkPolicyError Prelude.Text
coreNetworkPolicyError_errorCode = Lens.lens (\CoreNetworkPolicyError' {errorCode} -> errorCode) (\s@CoreNetworkPolicyError' {} a -> s {errorCode = a} :: CoreNetworkPolicyError)

-- | The message associated with a core network policy error code.
coreNetworkPolicyError_message :: Lens.Lens' CoreNetworkPolicyError Prelude.Text
coreNetworkPolicyError_message = Lens.lens (\CoreNetworkPolicyError' {message} -> message) (\s@CoreNetworkPolicyError' {} a -> s {message = a} :: CoreNetworkPolicyError)

instance Core.FromJSON CoreNetworkPolicyError where
  parseJSON =
    Core.withObject
      "CoreNetworkPolicyError"
      ( \x ->
          CoreNetworkPolicyError'
            Prelude.<$> (x Core..:? "Path")
            Prelude.<*> (x Core..: "ErrorCode")
            Prelude.<*> (x Core..: "Message")
      )

instance Prelude.Hashable CoreNetworkPolicyError where
  hashWithSalt _salt CoreNetworkPolicyError' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message

instance Prelude.NFData CoreNetworkPolicyError where
  rnf CoreNetworkPolicyError' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message
