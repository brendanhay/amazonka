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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFormationStackOutputsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFormationStackOutputsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the CloudFormation stack output.
--
-- /See:/ 'newAwsCloudFormationStackOutputsDetails' smart constructor.
data AwsCloudFormationStackOutputsDetails = AwsCloudFormationStackOutputsDetails'
  { -- | The key associated with the output.
    outputKey :: Prelude.Maybe Prelude.Text,
    -- | A user-defined description associated with the output.
    description :: Prelude.Maybe Prelude.Text,
    -- | The value associated with the output.
    outputValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFormationStackOutputsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputKey', 'awsCloudFormationStackOutputsDetails_outputKey' - The key associated with the output.
--
-- 'description', 'awsCloudFormationStackOutputsDetails_description' - A user-defined description associated with the output.
--
-- 'outputValue', 'awsCloudFormationStackOutputsDetails_outputValue' - The value associated with the output.
newAwsCloudFormationStackOutputsDetails ::
  AwsCloudFormationStackOutputsDetails
newAwsCloudFormationStackOutputsDetails =
  AwsCloudFormationStackOutputsDetails'
    { outputKey =
        Prelude.Nothing,
      description = Prelude.Nothing,
      outputValue = Prelude.Nothing
    }

-- | The key associated with the output.
awsCloudFormationStackOutputsDetails_outputKey :: Lens.Lens' AwsCloudFormationStackOutputsDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackOutputsDetails_outputKey = Lens.lens (\AwsCloudFormationStackOutputsDetails' {outputKey} -> outputKey) (\s@AwsCloudFormationStackOutputsDetails' {} a -> s {outputKey = a} :: AwsCloudFormationStackOutputsDetails)

-- | A user-defined description associated with the output.
awsCloudFormationStackOutputsDetails_description :: Lens.Lens' AwsCloudFormationStackOutputsDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackOutputsDetails_description = Lens.lens (\AwsCloudFormationStackOutputsDetails' {description} -> description) (\s@AwsCloudFormationStackOutputsDetails' {} a -> s {description = a} :: AwsCloudFormationStackOutputsDetails)

-- | The value associated with the output.
awsCloudFormationStackOutputsDetails_outputValue :: Lens.Lens' AwsCloudFormationStackOutputsDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackOutputsDetails_outputValue = Lens.lens (\AwsCloudFormationStackOutputsDetails' {outputValue} -> outputValue) (\s@AwsCloudFormationStackOutputsDetails' {} a -> s {outputValue = a} :: AwsCloudFormationStackOutputsDetails)

instance
  Core.FromJSON
    AwsCloudFormationStackOutputsDetails
  where
  parseJSON =
    Core.withObject
      "AwsCloudFormationStackOutputsDetails"
      ( \x ->
          AwsCloudFormationStackOutputsDetails'
            Prelude.<$> (x Core..:? "OutputKey")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "OutputValue")
      )

instance
  Prelude.Hashable
    AwsCloudFormationStackOutputsDetails
  where
  hashWithSalt
    _salt
    AwsCloudFormationStackOutputsDetails' {..} =
      _salt `Prelude.hashWithSalt` outputKey
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` outputValue

instance
  Prelude.NFData
    AwsCloudFormationStackOutputsDetails
  where
  rnf AwsCloudFormationStackOutputsDetails' {..} =
    Prelude.rnf outputKey
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf outputValue

instance
  Core.ToJSON
    AwsCloudFormationStackOutputsDetails
  where
  toJSON AwsCloudFormationStackOutputsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutputKey" Core..=) Prelude.<$> outputKey,
            ("Description" Core..=) Prelude.<$> description,
            ("OutputValue" Core..=) Prelude.<$> outputValue
          ]
      )
