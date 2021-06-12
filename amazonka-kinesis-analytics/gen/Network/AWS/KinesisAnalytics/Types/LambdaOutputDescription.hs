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
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | For an application output, describes the AWS Lambda function configured
-- as its destination.
--
-- /See:/ 'newLambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARN :: Core.Maybe Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination function.
    roleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaOutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'lambdaOutputDescription_resourceARN' - Amazon Resource Name (ARN) of the destination Lambda function.
--
-- 'roleARN', 'lambdaOutputDescription_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function.
newLambdaOutputDescription ::
  LambdaOutputDescription
newLambdaOutputDescription =
  LambdaOutputDescription'
    { resourceARN =
        Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function.
lambdaOutputDescription_resourceARN :: Lens.Lens' LambdaOutputDescription (Core.Maybe Core.Text)
lambdaOutputDescription_resourceARN = Lens.lens (\LambdaOutputDescription' {resourceARN} -> resourceARN) (\s@LambdaOutputDescription' {} a -> s {resourceARN = a} :: LambdaOutputDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function.
lambdaOutputDescription_roleARN :: Lens.Lens' LambdaOutputDescription (Core.Maybe Core.Text)
lambdaOutputDescription_roleARN = Lens.lens (\LambdaOutputDescription' {roleARN} -> roleARN) (\s@LambdaOutputDescription' {} a -> s {roleARN = a} :: LambdaOutputDescription)

instance Core.FromJSON LambdaOutputDescription where
  parseJSON =
    Core.withObject
      "LambdaOutputDescription"
      ( \x ->
          LambdaOutputDescription'
            Core.<$> (x Core..:? "ResourceARN")
            Core.<*> (x Core..:? "RoleARN")
      )

instance Core.Hashable LambdaOutputDescription

instance Core.NFData LambdaOutputDescription
