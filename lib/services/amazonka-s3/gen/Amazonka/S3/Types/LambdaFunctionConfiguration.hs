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
-- Module      : Amazonka.S3.Types.LambdaFunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LambdaFunctionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Event
import Amazonka.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for Lambda notifications.
--
-- /See:/ 'newLambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { id :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe NotificationConfigurationFilter,
    -- | The Amazon Resource Name (ARN) of the Lambda function that Amazon S3
    -- invokes when the specified event type occurs.
    lambdaFunctionArn :: Prelude.Text,
    -- | The Amazon S3 bucket event for which to invoke the Lambda function. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
    -- in the /Amazon S3 User Guide/.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'lambdaFunctionConfiguration_id' - Undocumented member.
--
-- 'filter'', 'lambdaFunctionConfiguration_filter' - Undocumented member.
--
-- 'lambdaFunctionArn', 'lambdaFunctionConfiguration_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the Lambda function that Amazon S3
-- invokes when the specified event type occurs.
--
-- 'events', 'lambdaFunctionConfiguration_events' - The Amazon S3 bucket event for which to invoke the Lambda function. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon S3 User Guide/.
newLambdaFunctionConfiguration ::
  -- | 'lambdaFunctionArn'
  Prelude.Text ->
  LambdaFunctionConfiguration
newLambdaFunctionConfiguration pLambdaFunctionArn_ =
  LambdaFunctionConfiguration'
    { id = Prelude.Nothing,
      filter' = Prelude.Nothing,
      lambdaFunctionArn = pLambdaFunctionArn_,
      events = Prelude.mempty
    }

-- | Undocumented member.
lambdaFunctionConfiguration_id :: Lens.Lens' LambdaFunctionConfiguration (Prelude.Maybe Prelude.Text)
lambdaFunctionConfiguration_id = Lens.lens (\LambdaFunctionConfiguration' {id} -> id) (\s@LambdaFunctionConfiguration' {} a -> s {id = a} :: LambdaFunctionConfiguration)

-- | Undocumented member.
lambdaFunctionConfiguration_filter :: Lens.Lens' LambdaFunctionConfiguration (Prelude.Maybe NotificationConfigurationFilter)
lambdaFunctionConfiguration_filter = Lens.lens (\LambdaFunctionConfiguration' {filter'} -> filter') (\s@LambdaFunctionConfiguration' {} a -> s {filter' = a} :: LambdaFunctionConfiguration)

-- | The Amazon Resource Name (ARN) of the Lambda function that Amazon S3
-- invokes when the specified event type occurs.
lambdaFunctionConfiguration_lambdaFunctionArn :: Lens.Lens' LambdaFunctionConfiguration Prelude.Text
lambdaFunctionConfiguration_lambdaFunctionArn = Lens.lens (\LambdaFunctionConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaFunctionConfiguration' {} a -> s {lambdaFunctionArn = a} :: LambdaFunctionConfiguration)

-- | The Amazon S3 bucket event for which to invoke the Lambda function. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon S3 User Guide/.
lambdaFunctionConfiguration_events :: Lens.Lens' LambdaFunctionConfiguration [Event]
lambdaFunctionConfiguration_events = Lens.lens (\LambdaFunctionConfiguration' {events} -> events) (\s@LambdaFunctionConfiguration' {} a -> s {events = a} :: LambdaFunctionConfiguration) Prelude.. Lens.coerced

instance Core.FromXML LambdaFunctionConfiguration where
  parseXML x =
    LambdaFunctionConfiguration'
      Prelude.<$> (x Core..@? "Id")
      Prelude.<*> (x Core..@? "Filter")
      Prelude.<*> (x Core..@ "CloudFunction")
      Prelude.<*> (Core.parseXMLList "Event" x)

instance Prelude.Hashable LambdaFunctionConfiguration where
  hashWithSalt _salt LambdaFunctionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` lambdaFunctionArn
      `Prelude.hashWithSalt` events

instance Prelude.NFData LambdaFunctionConfiguration where
  rnf LambdaFunctionConfiguration' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf lambdaFunctionArn
      `Prelude.seq` Prelude.rnf events

instance Core.ToXML LambdaFunctionConfiguration where
  toXML LambdaFunctionConfiguration' {..} =
    Prelude.mconcat
      [ "Id" Core.@= id,
        "Filter" Core.@= filter',
        "CloudFunction" Core.@= lambdaFunctionArn,
        Core.toXMLList "Event" events
      ]
