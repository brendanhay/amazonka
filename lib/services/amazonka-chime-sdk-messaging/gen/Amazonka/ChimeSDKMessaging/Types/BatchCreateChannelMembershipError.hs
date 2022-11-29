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
-- Module      : Amazonka.ChimeSDKMessaging.Types.BatchCreateChannelMembershipError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.BatchCreateChannelMembershipError where

import Amazonka.ChimeSDKMessaging.Types.ErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of failed member ARNs, error codes, and error messages.
--
-- /See:/ 'newBatchCreateChannelMembershipError' smart constructor.
data BatchCreateChannelMembershipError = BatchCreateChannelMembershipError'
  { -- | The @AppInstanceUserArn@ of the member that the service couldn\'t add.
    memberArn :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateChannelMembershipError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberArn', 'batchCreateChannelMembershipError_memberArn' - The @AppInstanceUserArn@ of the member that the service couldn\'t add.
--
-- 'errorMessage', 'batchCreateChannelMembershipError_errorMessage' - The error message.
--
-- 'errorCode', 'batchCreateChannelMembershipError_errorCode' - The error code.
newBatchCreateChannelMembershipError ::
  BatchCreateChannelMembershipError
newBatchCreateChannelMembershipError =
  BatchCreateChannelMembershipError'
    { memberArn =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The @AppInstanceUserArn@ of the member that the service couldn\'t add.
batchCreateChannelMembershipError_memberArn :: Lens.Lens' BatchCreateChannelMembershipError (Prelude.Maybe Prelude.Text)
batchCreateChannelMembershipError_memberArn = Lens.lens (\BatchCreateChannelMembershipError' {memberArn} -> memberArn) (\s@BatchCreateChannelMembershipError' {} a -> s {memberArn = a} :: BatchCreateChannelMembershipError)

-- | The error message.
batchCreateChannelMembershipError_errorMessage :: Lens.Lens' BatchCreateChannelMembershipError (Prelude.Maybe Prelude.Text)
batchCreateChannelMembershipError_errorMessage = Lens.lens (\BatchCreateChannelMembershipError' {errorMessage} -> errorMessage) (\s@BatchCreateChannelMembershipError' {} a -> s {errorMessage = a} :: BatchCreateChannelMembershipError)

-- | The error code.
batchCreateChannelMembershipError_errorCode :: Lens.Lens' BatchCreateChannelMembershipError (Prelude.Maybe ErrorCode)
batchCreateChannelMembershipError_errorCode = Lens.lens (\BatchCreateChannelMembershipError' {errorCode} -> errorCode) (\s@BatchCreateChannelMembershipError' {} a -> s {errorCode = a} :: BatchCreateChannelMembershipError)

instance
  Core.FromJSON
    BatchCreateChannelMembershipError
  where
  parseJSON =
    Core.withObject
      "BatchCreateChannelMembershipError"
      ( \x ->
          BatchCreateChannelMembershipError'
            Prelude.<$> (x Core..:? "MemberArn")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    BatchCreateChannelMembershipError
  where
  hashWithSalt
    _salt
    BatchCreateChannelMembershipError' {..} =
      _salt `Prelude.hashWithSalt` memberArn
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    BatchCreateChannelMembershipError
  where
  rnf BatchCreateChannelMembershipError' {..} =
    Prelude.rnf memberArn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
