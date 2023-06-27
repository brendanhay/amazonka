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
-- Module      : Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for a cluster logging configuration.
--
-- /See:/ 'newAwsEksClusterLoggingClusterLoggingDetails' smart constructor.
data AwsEksClusterLoggingClusterLoggingDetails = AwsEksClusterLoggingClusterLoggingDetails'
  { -- | Whether the logging types that are listed in @Types@ are enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of logging types. Valid values are as follows:
    --
    -- -   @api@
    --
    -- -   @audit@
    --
    -- -   @authenticator@
    --
    -- -   @controllerManager@
    --
    -- -   @scheduler@
    types :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEksClusterLoggingClusterLoggingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsEksClusterLoggingClusterLoggingDetails_enabled' - Whether the logging types that are listed in @Types@ are enabled.
--
-- 'types', 'awsEksClusterLoggingClusterLoggingDetails_types' - A list of logging types. Valid values are as follows:
--
-- -   @api@
--
-- -   @audit@
--
-- -   @authenticator@
--
-- -   @controllerManager@
--
-- -   @scheduler@
newAwsEksClusterLoggingClusterLoggingDetails ::
  AwsEksClusterLoggingClusterLoggingDetails
newAwsEksClusterLoggingClusterLoggingDetails =
  AwsEksClusterLoggingClusterLoggingDetails'
    { enabled =
        Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | Whether the logging types that are listed in @Types@ are enabled.
awsEksClusterLoggingClusterLoggingDetails_enabled :: Lens.Lens' AwsEksClusterLoggingClusterLoggingDetails (Prelude.Maybe Prelude.Bool)
awsEksClusterLoggingClusterLoggingDetails_enabled = Lens.lens (\AwsEksClusterLoggingClusterLoggingDetails' {enabled} -> enabled) (\s@AwsEksClusterLoggingClusterLoggingDetails' {} a -> s {enabled = a} :: AwsEksClusterLoggingClusterLoggingDetails)

-- | A list of logging types. Valid values are as follows:
--
-- -   @api@
--
-- -   @audit@
--
-- -   @authenticator@
--
-- -   @controllerManager@
--
-- -   @scheduler@
awsEksClusterLoggingClusterLoggingDetails_types :: Lens.Lens' AwsEksClusterLoggingClusterLoggingDetails (Prelude.Maybe [Prelude.Text])
awsEksClusterLoggingClusterLoggingDetails_types = Lens.lens (\AwsEksClusterLoggingClusterLoggingDetails' {types} -> types) (\s@AwsEksClusterLoggingClusterLoggingDetails' {} a -> s {types = a} :: AwsEksClusterLoggingClusterLoggingDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEksClusterLoggingClusterLoggingDetails
  where
  parseJSON =
    Data.withObject
      "AwsEksClusterLoggingClusterLoggingDetails"
      ( \x ->
          AwsEksClusterLoggingClusterLoggingDetails'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "Types" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEksClusterLoggingClusterLoggingDetails
  where
  hashWithSalt
    _salt
    AwsEksClusterLoggingClusterLoggingDetails' {..} =
      _salt
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` types

instance
  Prelude.NFData
    AwsEksClusterLoggingClusterLoggingDetails
  where
  rnf AwsEksClusterLoggingClusterLoggingDetails' {..} =
    Prelude.rnf enabled `Prelude.seq` Prelude.rnf types

instance
  Data.ToJSON
    AwsEksClusterLoggingClusterLoggingDetails
  where
  toJSON AwsEksClusterLoggingClusterLoggingDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("Types" Data..=) Prelude.<$> types
          ]
      )
