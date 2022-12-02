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
-- Module      : Amazonka.SSOAdmin.Types.AttachedManagedPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.AttachedManagedPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that stores the details of the AWS managed policy.
--
-- /See:/ 'newAttachedManagedPolicy' smart constructor.
data AttachedManagedPolicy = AttachedManagedPolicy'
  { -- | The name of the AWS managed policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the AWS managed policy. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachedManagedPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attachedManagedPolicy_name' - The name of the AWS managed policy.
--
-- 'arn', 'attachedManagedPolicy_arn' - The ARN of the AWS managed policy. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newAttachedManagedPolicy ::
  AttachedManagedPolicy
newAttachedManagedPolicy =
  AttachedManagedPolicy'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name of the AWS managed policy.
attachedManagedPolicy_name :: Lens.Lens' AttachedManagedPolicy (Prelude.Maybe Prelude.Text)
attachedManagedPolicy_name = Lens.lens (\AttachedManagedPolicy' {name} -> name) (\s@AttachedManagedPolicy' {} a -> s {name = a} :: AttachedManagedPolicy)

-- | The ARN of the AWS managed policy. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
attachedManagedPolicy_arn :: Lens.Lens' AttachedManagedPolicy (Prelude.Maybe Prelude.Text)
attachedManagedPolicy_arn = Lens.lens (\AttachedManagedPolicy' {arn} -> arn) (\s@AttachedManagedPolicy' {} a -> s {arn = a} :: AttachedManagedPolicy)

instance Data.FromJSON AttachedManagedPolicy where
  parseJSON =
    Data.withObject
      "AttachedManagedPolicy"
      ( \x ->
          AttachedManagedPolicy'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Arn")
      )

instance Prelude.Hashable AttachedManagedPolicy where
  hashWithSalt _salt AttachedManagedPolicy' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData AttachedManagedPolicy where
  rnf AttachedManagedPolicy' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn
