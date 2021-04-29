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
-- Module      : Network.AWS.SSM.Types.OpsItemIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemIdentity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the user or resource that created an OpsItem event.
--
-- /See:/ 'newOpsItemIdentity' smart constructor.
data OpsItemIdentity = OpsItemIdentity'
  { -- | The Amazon Resource Name (ARN) of the IAM entity that created the
    -- OpsItem event.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OpsItemIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'opsItemIdentity_arn' - The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem event.
newOpsItemIdentity ::
  OpsItemIdentity
newOpsItemIdentity =
  OpsItemIdentity' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM entity that created the
-- OpsItem event.
opsItemIdentity_arn :: Lens.Lens' OpsItemIdentity (Prelude.Maybe Prelude.Text)
opsItemIdentity_arn = Lens.lens (\OpsItemIdentity' {arn} -> arn) (\s@OpsItemIdentity' {} a -> s {arn = a} :: OpsItemIdentity)

instance Prelude.FromJSON OpsItemIdentity where
  parseJSON =
    Prelude.withObject
      "OpsItemIdentity"
      ( \x ->
          OpsItemIdentity' Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable OpsItemIdentity

instance Prelude.NFData OpsItemIdentity
