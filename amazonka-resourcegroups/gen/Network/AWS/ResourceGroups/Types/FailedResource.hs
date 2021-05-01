{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ResourceGroups.Types.FailedResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.FailedResource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A resource that failed to be added to or removed from a group.
--
-- /See:/ 'newFailedResource' smart constructor.
data FailedResource = FailedResource'
  { -- | The ARN of the resource that failed to be added or removed.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The error message text associated with the failure.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code associated with the failure.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'failedResource_resourceArn' - The ARN of the resource that failed to be added or removed.
--
-- 'errorMessage', 'failedResource_errorMessage' - The error message text associated with the failure.
--
-- 'errorCode', 'failedResource_errorCode' - The error code associated with the failure.
newFailedResource ::
  FailedResource
newFailedResource =
  FailedResource'
    { resourceArn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The ARN of the resource that failed to be added or removed.
failedResource_resourceArn :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_resourceArn = Lens.lens (\FailedResource' {resourceArn} -> resourceArn) (\s@FailedResource' {} a -> s {resourceArn = a} :: FailedResource)

-- | The error message text associated with the failure.
failedResource_errorMessage :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_errorMessage = Lens.lens (\FailedResource' {errorMessage} -> errorMessage) (\s@FailedResource' {} a -> s {errorMessage = a} :: FailedResource)

-- | The error code associated with the failure.
failedResource_errorCode :: Lens.Lens' FailedResource (Prelude.Maybe Prelude.Text)
failedResource_errorCode = Lens.lens (\FailedResource' {errorCode} -> errorCode) (\s@FailedResource' {} a -> s {errorCode = a} :: FailedResource)

instance Prelude.FromJSON FailedResource where
  parseJSON =
    Prelude.withObject
      "FailedResource"
      ( \x ->
          FailedResource'
            Prelude.<$> (x Prelude..:? "ResourceArn")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable FailedResource

instance Prelude.NFData FailedResource
