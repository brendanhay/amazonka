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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.LambdaOutputDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.LambdaOutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application\'s output, describes
-- the Amazon Lambda function that is configured as its destination.
--
-- /See:/ 'newLambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { -- | The ARN of the IAM role that Kinesis Data Analytics can assume to write
    -- to the destination function.
    --
    -- Provided for backward compatibility. Applications that are created with
    -- the current API version have an application-level service execution role
    -- rather than a resource-level role.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARN :: Prelude.Text
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
-- 'roleARN', 'lambdaOutputDescription_roleARN' - The ARN of the IAM role that Kinesis Data Analytics can assume to write
-- to the destination function.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'resourceARN', 'lambdaOutputDescription_resourceARN' - The Amazon Resource Name (ARN) of the destination Lambda function.
newLambdaOutputDescription ::
  -- | 'resourceARN'
  Prelude.Text ->
  LambdaOutputDescription
newLambdaOutputDescription pResourceARN_ =
  LambdaOutputDescription'
    { roleARN = Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | The ARN of the IAM role that Kinesis Data Analytics can assume to write
-- to the destination function.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
lambdaOutputDescription_roleARN :: Lens.Lens' LambdaOutputDescription (Prelude.Maybe Prelude.Text)
lambdaOutputDescription_roleARN = Lens.lens (\LambdaOutputDescription' {roleARN} -> roleARN) (\s@LambdaOutputDescription' {} a -> s {roleARN = a} :: LambdaOutputDescription)

-- | The Amazon Resource Name (ARN) of the destination Lambda function.
lambdaOutputDescription_resourceARN :: Lens.Lens' LambdaOutputDescription Prelude.Text
lambdaOutputDescription_resourceARN = Lens.lens (\LambdaOutputDescription' {resourceARN} -> resourceARN) (\s@LambdaOutputDescription' {} a -> s {resourceARN = a} :: LambdaOutputDescription)

instance Data.FromJSON LambdaOutputDescription where
  parseJSON =
    Data.withObject
      "LambdaOutputDescription"
      ( \x ->
          LambdaOutputDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..: "ResourceARN")
      )

instance Prelude.Hashable LambdaOutputDescription where
  hashWithSalt _salt LambdaOutputDescription' {..} =
    _salt
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData LambdaOutputDescription where
  rnf LambdaOutputDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
