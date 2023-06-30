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
-- Module      : Amazonka.IoTAnalytics.Types.LambdaActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.LambdaActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An activity that runs a Lambda function to modify the message.
--
-- /See:/ 'newLambdaActivity' smart constructor.
data LambdaActivity = LambdaActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the lambda activity.
    name :: Prelude.Text,
    -- | The name of the Lambda function that is run on the message.
    lambdaName :: Prelude.Text,
    -- | The number of messages passed to the Lambda function for processing.
    --
    -- The Lambda function must be able to process all of these messages within
    -- five minutes, which is the maximum timeout duration for Lambda
    -- functions.
    batchSize :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'lambdaActivity_next' - The next activity in the pipeline.
--
-- 'name', 'lambdaActivity_name' - The name of the lambda activity.
--
-- 'lambdaName', 'lambdaActivity_lambdaName' - The name of the Lambda function that is run on the message.
--
-- 'batchSize', 'lambdaActivity_batchSize' - The number of messages passed to the Lambda function for processing.
--
-- The Lambda function must be able to process all of these messages within
-- five minutes, which is the maximum timeout duration for Lambda
-- functions.
newLambdaActivity ::
  -- | 'name'
  Prelude.Text ->
  -- | 'lambdaName'
  Prelude.Text ->
  -- | 'batchSize'
  Prelude.Natural ->
  LambdaActivity
newLambdaActivity pName_ pLambdaName_ pBatchSize_ =
  LambdaActivity'
    { next = Prelude.Nothing,
      name = pName_,
      lambdaName = pLambdaName_,
      batchSize = pBatchSize_
    }

-- | The next activity in the pipeline.
lambdaActivity_next :: Lens.Lens' LambdaActivity (Prelude.Maybe Prelude.Text)
lambdaActivity_next = Lens.lens (\LambdaActivity' {next} -> next) (\s@LambdaActivity' {} a -> s {next = a} :: LambdaActivity)

-- | The name of the lambda activity.
lambdaActivity_name :: Lens.Lens' LambdaActivity Prelude.Text
lambdaActivity_name = Lens.lens (\LambdaActivity' {name} -> name) (\s@LambdaActivity' {} a -> s {name = a} :: LambdaActivity)

-- | The name of the Lambda function that is run on the message.
lambdaActivity_lambdaName :: Lens.Lens' LambdaActivity Prelude.Text
lambdaActivity_lambdaName = Lens.lens (\LambdaActivity' {lambdaName} -> lambdaName) (\s@LambdaActivity' {} a -> s {lambdaName = a} :: LambdaActivity)

-- | The number of messages passed to the Lambda function for processing.
--
-- The Lambda function must be able to process all of these messages within
-- five minutes, which is the maximum timeout duration for Lambda
-- functions.
lambdaActivity_batchSize :: Lens.Lens' LambdaActivity Prelude.Natural
lambdaActivity_batchSize = Lens.lens (\LambdaActivity' {batchSize} -> batchSize) (\s@LambdaActivity' {} a -> s {batchSize = a} :: LambdaActivity)

instance Data.FromJSON LambdaActivity where
  parseJSON =
    Data.withObject
      "LambdaActivity"
      ( \x ->
          LambdaActivity'
            Prelude.<$> (x Data..:? "next")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "lambdaName")
            Prelude.<*> (x Data..: "batchSize")
      )

instance Prelude.Hashable LambdaActivity where
  hashWithSalt _salt LambdaActivity' {..} =
    _salt
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lambdaName
      `Prelude.hashWithSalt` batchSize

instance Prelude.NFData LambdaActivity where
  rnf LambdaActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lambdaName
      `Prelude.seq` Prelude.rnf batchSize

instance Data.ToJSON LambdaActivity where
  toJSON LambdaActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("next" Data..=) Prelude.<$> next,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("lambdaName" Data..= lambdaName),
            Prelude.Just ("batchSize" Data..= batchSize)
          ]
      )
