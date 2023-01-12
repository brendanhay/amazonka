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
-- Module      : Amazonka.STS.Types.PolicyDescriptorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.STS.Types.PolicyDescriptorType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to the IAM managed policy that is passed as a session policy
-- for a role session or a federated user session.
--
-- /See:/ 'newPolicyDescriptorType' smart constructor.
data PolicyDescriptorType = PolicyDescriptorType'
  { -- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a
    -- session policy for the role. For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDescriptorType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'policyDescriptorType_arn' - The Amazon Resource Name (ARN) of the IAM managed policy to use as a
-- session policy for the role. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
newPolicyDescriptorType ::
  PolicyDescriptorType
newPolicyDescriptorType =
  PolicyDescriptorType' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a
-- session policy for the role. For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
policyDescriptorType_arn :: Lens.Lens' PolicyDescriptorType (Prelude.Maybe Prelude.Text)
policyDescriptorType_arn = Lens.lens (\PolicyDescriptorType' {arn} -> arn) (\s@PolicyDescriptorType' {} a -> s {arn = a} :: PolicyDescriptorType)

instance Prelude.Hashable PolicyDescriptorType where
  hashWithSalt _salt PolicyDescriptorType' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData PolicyDescriptorType where
  rnf PolicyDescriptorType' {..} = Prelude.rnf arn

instance Data.ToQuery PolicyDescriptorType where
  toQuery PolicyDescriptorType' {..} =
    Prelude.mconcat ["arn" Data.=: arn]
