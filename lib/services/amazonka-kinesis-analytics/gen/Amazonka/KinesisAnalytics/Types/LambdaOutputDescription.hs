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
-- Module      : Amazonka.KinesisAnalytics.Types.LambdaOutputDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.LambdaOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For an application output, describes the AWS Lambda function configured
-- as its destination.
--
-- /See:/ 'newLambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination function.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'lambdaOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function.
--
-- 'resourceARN', 'lambdaOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the destination Lambda function.
newLambdaOutputDescription ::
  LambdaOutputDescription
newLambdaOutputDescription =
  LambdaOutputDescription'
    { roleARN = Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function.
lambdaOutputDescription_roleARN :: Lens.Lens' LambdaOutputDescription (Prelude.Maybe Prelude.Text)
lambdaOutputDescription_roleARN = Lens.lens (\LambdaOutputDescription' {roleARN} -> roleARN) (\s@LambdaOutputDescription' {} a -> s {roleARN = a} :: LambdaOutputDescription)

-- | Amazon Resource Name (ARN) of the destination Lambda function.
lambdaOutputDescription_resourceARN :: Lens.Lens' LambdaOutputDescription (Prelude.Maybe Prelude.Text)
lambdaOutputDescription_resourceARN = Lens.lens (\LambdaOutputDescription' {resourceARN} -> resourceARN) (\s@LambdaOutputDescription' {} a -> s {resourceARN = a} :: LambdaOutputDescription)

instance Data.FromJSON LambdaOutputDescription where
  parseJSON =
    Data.withObject
      "LambdaOutputDescription"
      ( \x ->
          LambdaOutputDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "ResourceARN")
      )

instance Prelude.Hashable LambdaOutputDescription where
  hashWithSalt _salt LambdaOutputDescription' {..} =
    _salt `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData LambdaOutputDescription where
  rnf LambdaOutputDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
