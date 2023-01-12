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
-- Module      : Amazonka.S3.Types.PolicyStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.PolicyStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | The container element for a bucket\'s policy status.
--
-- /See:/ 'newPolicyStatus' smart constructor.
data PolicyStatus = PolicyStatus'
  { -- | The policy status for this bucket. @TRUE@ indicates that this bucket is
    -- public. @FALSE@ indicates that the bucket is not public.
    isPublic :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPublic', 'policyStatus_isPublic' - The policy status for this bucket. @TRUE@ indicates that this bucket is
-- public. @FALSE@ indicates that the bucket is not public.
newPolicyStatus ::
  PolicyStatus
newPolicyStatus =
  PolicyStatus' {isPublic = Prelude.Nothing}

-- | The policy status for this bucket. @TRUE@ indicates that this bucket is
-- public. @FALSE@ indicates that the bucket is not public.
policyStatus_isPublic :: Lens.Lens' PolicyStatus (Prelude.Maybe Prelude.Bool)
policyStatus_isPublic = Lens.lens (\PolicyStatus' {isPublic} -> isPublic) (\s@PolicyStatus' {} a -> s {isPublic = a} :: PolicyStatus)

instance Data.FromXML PolicyStatus where
  parseXML x =
    PolicyStatus' Prelude.<$> (x Data..@? "IsPublic")

instance Prelude.Hashable PolicyStatus where
  hashWithSalt _salt PolicyStatus' {..} =
    _salt `Prelude.hashWithSalt` isPublic

instance Prelude.NFData PolicyStatus where
  rnf PolicyStatus' {..} = Prelude.rnf isPublic
