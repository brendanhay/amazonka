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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An option group membership.
--
-- /See:/ 'newAwsRdsDbOptionGroupMembership' smart constructor.
data AwsRdsDbOptionGroupMembership = AwsRdsDbOptionGroupMembership'
  { -- | The status of the option group membership.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group.
    optionGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbOptionGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbOptionGroupMembership_status' - The status of the option group membership.
--
-- 'optionGroupName', 'awsRdsDbOptionGroupMembership_optionGroupName' - The name of the option group.
newAwsRdsDbOptionGroupMembership ::
  AwsRdsDbOptionGroupMembership
newAwsRdsDbOptionGroupMembership =
  AwsRdsDbOptionGroupMembership'
    { status =
        Prelude.Nothing,
      optionGroupName = Prelude.Nothing
    }

-- | The status of the option group membership.
awsRdsDbOptionGroupMembership_status :: Lens.Lens' AwsRdsDbOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbOptionGroupMembership_status = Lens.lens (\AwsRdsDbOptionGroupMembership' {status} -> status) (\s@AwsRdsDbOptionGroupMembership' {} a -> s {status = a} :: AwsRdsDbOptionGroupMembership)

-- | The name of the option group.
awsRdsDbOptionGroupMembership_optionGroupName :: Lens.Lens' AwsRdsDbOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbOptionGroupMembership_optionGroupName = Lens.lens (\AwsRdsDbOptionGroupMembership' {optionGroupName} -> optionGroupName) (\s@AwsRdsDbOptionGroupMembership' {} a -> s {optionGroupName = a} :: AwsRdsDbOptionGroupMembership)

instance Core.FromJSON AwsRdsDbOptionGroupMembership where
  parseJSON =
    Core.withObject
      "AwsRdsDbOptionGroupMembership"
      ( \x ->
          AwsRdsDbOptionGroupMembership'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OptionGroupName")
      )

instance
  Prelude.Hashable
    AwsRdsDbOptionGroupMembership

instance Prelude.NFData AwsRdsDbOptionGroupMembership

instance Core.ToJSON AwsRdsDbOptionGroupMembership where
  toJSON AwsRdsDbOptionGroupMembership' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("OptionGroupName" Core..=)
              Prelude.<$> optionGroupName
          ]
      )
