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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFormationStackDriftInformationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFormationStackDriftInformationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the stack\'s conformity to its expected
-- template configuration.
--
-- /See:/ 'newAwsCloudFormationStackDriftInformationDetails' smart constructor.
data AwsCloudFormationStackDriftInformationDetails = AwsCloudFormationStackDriftInformationDetails'
  { -- | Status of the stack\'s actual configuration compared to its expected
    -- template configuration.
    stackDriftStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFormationStackDriftInformationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackDriftStatus', 'awsCloudFormationStackDriftInformationDetails_stackDriftStatus' - Status of the stack\'s actual configuration compared to its expected
-- template configuration.
newAwsCloudFormationStackDriftInformationDetails ::
  AwsCloudFormationStackDriftInformationDetails
newAwsCloudFormationStackDriftInformationDetails =
  AwsCloudFormationStackDriftInformationDetails'
    { stackDriftStatus =
        Prelude.Nothing
    }

-- | Status of the stack\'s actual configuration compared to its expected
-- template configuration.
awsCloudFormationStackDriftInformationDetails_stackDriftStatus :: Lens.Lens' AwsCloudFormationStackDriftInformationDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDriftInformationDetails_stackDriftStatus = Lens.lens (\AwsCloudFormationStackDriftInformationDetails' {stackDriftStatus} -> stackDriftStatus) (\s@AwsCloudFormationStackDriftInformationDetails' {} a -> s {stackDriftStatus = a} :: AwsCloudFormationStackDriftInformationDetails)

instance
  Data.FromJSON
    AwsCloudFormationStackDriftInformationDetails
  where
  parseJSON =
    Data.withObject
      "AwsCloudFormationStackDriftInformationDetails"
      ( \x ->
          AwsCloudFormationStackDriftInformationDetails'
            Prelude.<$> (x Data..:? "StackDriftStatus")
      )

instance
  Prelude.Hashable
    AwsCloudFormationStackDriftInformationDetails
  where
  hashWithSalt
    _salt
    AwsCloudFormationStackDriftInformationDetails' {..} =
      _salt `Prelude.hashWithSalt` stackDriftStatus

instance
  Prelude.NFData
    AwsCloudFormationStackDriftInformationDetails
  where
  rnf
    AwsCloudFormationStackDriftInformationDetails' {..} =
      Prelude.rnf stackDriftStatus

instance
  Data.ToJSON
    AwsCloudFormationStackDriftInformationDetails
  where
  toJSON
    AwsCloudFormationStackDriftInformationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("StackDriftStatus" Data..=)
                Prelude.<$> stackDriftStatus
            ]
        )
