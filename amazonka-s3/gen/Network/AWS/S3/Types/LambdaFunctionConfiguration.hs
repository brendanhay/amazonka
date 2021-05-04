{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.LambdaFunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LambdaFunctionConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for AWS Lambda
-- notifications.
--
-- /See:/ 'newLambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { id :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe NotificationConfigurationFilter,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3
    -- invokes when the specified event type occurs.
    lambdaFunctionArn :: Prelude.Text,
    -- | The Amazon S3 bucket event for which to invoke the AWS Lambda function.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'lambdaFunctionArn', 'lambdaFunctionConfiguration_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3
-- invokes when the specified event type occurs.
--
-- 'events', 'lambdaFunctionConfiguration_events' - The Amazon S3 bucket event for which to invoke the AWS Lambda function.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon Simple Storage Service Developer Guide/.
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

-- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3
-- invokes when the specified event type occurs.
lambdaFunctionConfiguration_lambdaFunctionArn :: Lens.Lens' LambdaFunctionConfiguration Prelude.Text
lambdaFunctionConfiguration_lambdaFunctionArn = Lens.lens (\LambdaFunctionConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaFunctionConfiguration' {} a -> s {lambdaFunctionArn = a} :: LambdaFunctionConfiguration)

-- | The Amazon S3 bucket event for which to invoke the AWS Lambda function.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types>
-- in the /Amazon Simple Storage Service Developer Guide/.
lambdaFunctionConfiguration_events :: Lens.Lens' LambdaFunctionConfiguration [Event]
lambdaFunctionConfiguration_events = Lens.lens (\LambdaFunctionConfiguration' {events} -> events) (\s@LambdaFunctionConfiguration' {} a -> s {events = a} :: LambdaFunctionConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromXML LambdaFunctionConfiguration where
  parseXML x =
    LambdaFunctionConfiguration'
      Prelude.<$> (x Prelude..@? "Id")
      Prelude.<*> (x Prelude..@? "Filter")
      Prelude.<*> (x Prelude..@ "CloudFunction")
      Prelude.<*> (Prelude.parseXMLList "Event" x)

instance Prelude.Hashable LambdaFunctionConfiguration

instance Prelude.NFData LambdaFunctionConfiguration

instance Prelude.ToXML LambdaFunctionConfiguration where
  toXML LambdaFunctionConfiguration' {..} =
    Prelude.mconcat
      [ "Id" Prelude.@= id,
        "Filter" Prelude.@= filter',
        "CloudFunction" Prelude.@= lambdaFunctionArn,
        Prelude.toXMLList "Event" events
      ]
