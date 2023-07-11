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
-- Module      : Amazonka.ResourceGroups.Types.FailedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.FailedResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource that failed to be added to or removed from a group.
--
-- /See:/ 'newFailedResource' smart constructor.
data FailedResource = FailedResource'
  { -- | The error code associated with the failure.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message text associated with the failure.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource that failed to be added or removed.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedResource_errorCode' - The error code associated with the failure.
--
-- 'errorMessage', 'failedResource_errorMessage' - The error message text associated with the failure.
--
-- 'resourceArn', 'failedResource_resourceArn' - The ARN of the resource that failed to be added or removed.
newFailedResource ::
  FailedResource
newFailedResource =
  FailedResource'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The error code associated with the failure.
failedResource_errorCode :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_errorCode = Lens.lens (\FailedResource' {errorCode} -> errorCode) (\s@FailedResource' {} a -> s {errorCode = a} :: FailedResource)

-- | The error message text associated with the failure.
failedResource_errorMessage :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_errorMessage = Lens.lens (\FailedResource' {errorMessage} -> errorMessage) (\s@FailedResource' {} a -> s {errorMessage = a} :: FailedResource)

-- | The ARN of the resource that failed to be added or removed.
failedResource_resourceArn :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_resourceArn = Lens.lens (\FailedResource' {resourceArn} -> resourceArn) (\s@FailedResource' {} a -> s {resourceArn = a} :: FailedResource)

instance Data.FromJSON FailedResource where
  parseJSON =
    Data.withObject
      "FailedResource"
      ( \x ->
          FailedResource'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable FailedResource where
  hashWithSalt _salt FailedResource' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData FailedResource where
  rnf FailedResource' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf resourceArn
