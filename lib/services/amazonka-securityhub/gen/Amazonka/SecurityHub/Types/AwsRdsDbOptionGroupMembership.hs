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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An option group membership.
--
-- /See:/ 'newAwsRdsDbOptionGroupMembership' smart constructor.
data AwsRdsDbOptionGroupMembership = AwsRdsDbOptionGroupMembership'
  { -- | The name of the option group.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the option group membership.
    status :: Prelude.Maybe Prelude.Text
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
-- 'optionGroupName', 'awsRdsDbOptionGroupMembership_optionGroupName' - The name of the option group.
--
-- 'status', 'awsRdsDbOptionGroupMembership_status' - The status of the option group membership.
newAwsRdsDbOptionGroupMembership ::
  AwsRdsDbOptionGroupMembership
newAwsRdsDbOptionGroupMembership =
  AwsRdsDbOptionGroupMembership'
    { optionGroupName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the option group.
awsRdsDbOptionGroupMembership_optionGroupName :: Lens.Lens' AwsRdsDbOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbOptionGroupMembership_optionGroupName = Lens.lens (\AwsRdsDbOptionGroupMembership' {optionGroupName} -> optionGroupName) (\s@AwsRdsDbOptionGroupMembership' {} a -> s {optionGroupName = a} :: AwsRdsDbOptionGroupMembership)

-- | The status of the option group membership.
awsRdsDbOptionGroupMembership_status :: Lens.Lens' AwsRdsDbOptionGroupMembership (Prelude.Maybe Prelude.Text)
awsRdsDbOptionGroupMembership_status = Lens.lens (\AwsRdsDbOptionGroupMembership' {status} -> status) (\s@AwsRdsDbOptionGroupMembership' {} a -> s {status = a} :: AwsRdsDbOptionGroupMembership)

instance Core.FromJSON AwsRdsDbOptionGroupMembership where
  parseJSON =
    Core.withObject
      "AwsRdsDbOptionGroupMembership"
      ( \x ->
          AwsRdsDbOptionGroupMembership'
            Prelude.<$> (x Core..:? "OptionGroupName")
            Prelude.<*> (x Core..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRdsDbOptionGroupMembership
  where
  hashWithSalt _salt AwsRdsDbOptionGroupMembership' {..} =
    _salt `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsRdsDbOptionGroupMembership where
  rnf AwsRdsDbOptionGroupMembership' {..} =
    Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf status

instance Core.ToJSON AwsRdsDbOptionGroupMembership where
  toJSON AwsRdsDbOptionGroupMembership' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OptionGroupName" Core..=)
              Prelude.<$> optionGroupName,
            ("Status" Core..=) Prelude.<$> status
          ]
      )
