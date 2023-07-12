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
-- Module      : Amazonka.Snowball.Types.LambdaResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.LambdaResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.EventTriggerDefinition

-- | Identifies
--
-- /See:/ 'newLambdaResource' smart constructor.
data LambdaResource = LambdaResource'
  { -- | The array of ARNs for S3Resource objects to trigger the LambdaResource
    -- objects associated with this job.
    eventTriggers :: Prelude.Maybe [EventTriggerDefinition],
    -- | An Amazon Resource Name (ARN) that represents an Lambda function to be
    -- triggered by PUT object actions on the associated local Amazon S3
    -- resource.
    lambdaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTriggers', 'lambdaResource_eventTriggers' - The array of ARNs for S3Resource objects to trigger the LambdaResource
-- objects associated with this job.
--
-- 'lambdaArn', 'lambdaResource_lambdaArn' - An Amazon Resource Name (ARN) that represents an Lambda function to be
-- triggered by PUT object actions on the associated local Amazon S3
-- resource.
newLambdaResource ::
  LambdaResource
newLambdaResource =
  LambdaResource'
    { eventTriggers = Prelude.Nothing,
      lambdaArn = Prelude.Nothing
    }

-- | The array of ARNs for S3Resource objects to trigger the LambdaResource
-- objects associated with this job.
lambdaResource_eventTriggers :: Lens.Lens' LambdaResource (Prelude.Maybe [EventTriggerDefinition])
lambdaResource_eventTriggers = Lens.lens (\LambdaResource' {eventTriggers} -> eventTriggers) (\s@LambdaResource' {} a -> s {eventTriggers = a} :: LambdaResource) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon Resource Name (ARN) that represents an Lambda function to be
-- triggered by PUT object actions on the associated local Amazon S3
-- resource.
lambdaResource_lambdaArn :: Lens.Lens' LambdaResource (Prelude.Maybe Prelude.Text)
lambdaResource_lambdaArn = Lens.lens (\LambdaResource' {lambdaArn} -> lambdaArn) (\s@LambdaResource' {} a -> s {lambdaArn = a} :: LambdaResource)

instance Data.FromJSON LambdaResource where
  parseJSON =
    Data.withObject
      "LambdaResource"
      ( \x ->
          LambdaResource'
            Prelude.<$> (x Data..:? "EventTriggers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LambdaArn")
      )

instance Prelude.Hashable LambdaResource where
  hashWithSalt _salt LambdaResource' {..} =
    _salt
      `Prelude.hashWithSalt` eventTriggers
      `Prelude.hashWithSalt` lambdaArn

instance Prelude.NFData LambdaResource where
  rnf LambdaResource' {..} =
    Prelude.rnf eventTriggers
      `Prelude.seq` Prelude.rnf lambdaArn

instance Data.ToJSON LambdaResource where
  toJSON LambdaResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventTriggers" Data..=) Prelude.<$> eventTriggers,
            ("LambdaArn" Data..=) Prelude.<$> lambdaArn
          ]
      )
