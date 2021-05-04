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
-- Module      : Network.AWS.S3.Types.PolicyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.PolicyStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | The container element for a bucket\'s policy status.
--
-- /See:/ 'newPolicyStatus' smart constructor.
data PolicyStatus = PolicyStatus'
  { -- | The policy status for this bucket. @TRUE@ indicates that this bucket is
    -- public. @FALSE@ indicates that the bucket is not public.
    isPublic :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML PolicyStatus where
  parseXML x =
    PolicyStatus'
      Prelude.<$> (x Prelude..@? "IsPublic")

instance Prelude.Hashable PolicyStatus

instance Prelude.NFData PolicyStatus
