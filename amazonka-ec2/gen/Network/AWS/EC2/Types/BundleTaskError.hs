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
-- Module      : Network.AWS.EC2.Types.BundleTaskError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTaskError where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an error for BundleInstance.
--
-- /See:/ 'newBundleTaskError' smart constructor.
data BundleTaskError = BundleTaskError'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BundleTaskError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'bundleTaskError_message' - The error message.
--
-- 'code', 'bundleTaskError_code' - The error code.
newBundleTaskError ::
  BundleTaskError
newBundleTaskError =
  BundleTaskError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message.
bundleTaskError_message :: Lens.Lens' BundleTaskError (Prelude.Maybe Prelude.Text)
bundleTaskError_message = Lens.lens (\BundleTaskError' {message} -> message) (\s@BundleTaskError' {} a -> s {message = a} :: BundleTaskError)

-- | The error code.
bundleTaskError_code :: Lens.Lens' BundleTaskError (Prelude.Maybe Prelude.Text)
bundleTaskError_code = Lens.lens (\BundleTaskError' {code} -> code) (\s@BundleTaskError' {} a -> s {code = a} :: BundleTaskError)

instance Prelude.FromXML BundleTaskError where
  parseXML x =
    BundleTaskError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable BundleTaskError

instance Prelude.NFData BundleTaskError
