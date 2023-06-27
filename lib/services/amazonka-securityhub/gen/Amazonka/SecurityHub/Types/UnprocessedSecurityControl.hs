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
-- Module      : Amazonka.SecurityHub.Types.UnprocessedSecurityControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UnprocessedSecurityControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.UnprocessedErrorCode

-- | Provides details about a security control for which a response couldn\'t
-- be returned.
--
-- /See:/ 'newUnprocessedSecurityControl' smart constructor.
data UnprocessedSecurityControl = UnprocessedSecurityControl'
  { -- | The reason why the security control was unprocessed.
    errorReason :: Prelude.Maybe Prelude.Text,
    -- | The control (identified with @SecurityControlId@, @SecurityControlArn@,
    -- or a mix of both parameters) for which a response couldn\'t be returned.
    securityControlId :: Prelude.Text,
    -- | The error code for the unprocessed security control.
    errorCode :: UnprocessedErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedSecurityControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorReason', 'unprocessedSecurityControl_errorReason' - The reason why the security control was unprocessed.
--
-- 'securityControlId', 'unprocessedSecurityControl_securityControlId' - The control (identified with @SecurityControlId@, @SecurityControlArn@,
-- or a mix of both parameters) for which a response couldn\'t be returned.
--
-- 'errorCode', 'unprocessedSecurityControl_errorCode' - The error code for the unprocessed security control.
newUnprocessedSecurityControl ::
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'errorCode'
  UnprocessedErrorCode ->
  UnprocessedSecurityControl
newUnprocessedSecurityControl
  pSecurityControlId_
  pErrorCode_ =
    UnprocessedSecurityControl'
      { errorReason =
          Prelude.Nothing,
        securityControlId = pSecurityControlId_,
        errorCode = pErrorCode_
      }

-- | The reason why the security control was unprocessed.
unprocessedSecurityControl_errorReason :: Lens.Lens' UnprocessedSecurityControl (Prelude.Maybe Prelude.Text)
unprocessedSecurityControl_errorReason = Lens.lens (\UnprocessedSecurityControl' {errorReason} -> errorReason) (\s@UnprocessedSecurityControl' {} a -> s {errorReason = a} :: UnprocessedSecurityControl)

-- | The control (identified with @SecurityControlId@, @SecurityControlArn@,
-- or a mix of both parameters) for which a response couldn\'t be returned.
unprocessedSecurityControl_securityControlId :: Lens.Lens' UnprocessedSecurityControl Prelude.Text
unprocessedSecurityControl_securityControlId = Lens.lens (\UnprocessedSecurityControl' {securityControlId} -> securityControlId) (\s@UnprocessedSecurityControl' {} a -> s {securityControlId = a} :: UnprocessedSecurityControl)

-- | The error code for the unprocessed security control.
unprocessedSecurityControl_errorCode :: Lens.Lens' UnprocessedSecurityControl UnprocessedErrorCode
unprocessedSecurityControl_errorCode = Lens.lens (\UnprocessedSecurityControl' {errorCode} -> errorCode) (\s@UnprocessedSecurityControl' {} a -> s {errorCode = a} :: UnprocessedSecurityControl)

instance Data.FromJSON UnprocessedSecurityControl where
  parseJSON =
    Data.withObject
      "UnprocessedSecurityControl"
      ( \x ->
          UnprocessedSecurityControl'
            Prelude.<$> (x Data..:? "ErrorReason")
            Prelude.<*> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "ErrorCode")
      )

instance Prelude.Hashable UnprocessedSecurityControl where
  hashWithSalt _salt UnprocessedSecurityControl' {..} =
    _salt
      `Prelude.hashWithSalt` errorReason
      `Prelude.hashWithSalt` securityControlId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UnprocessedSecurityControl where
  rnf UnprocessedSecurityControl' {..} =
    Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf errorCode
