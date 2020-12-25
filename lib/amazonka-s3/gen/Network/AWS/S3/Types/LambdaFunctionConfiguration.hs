{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LambdaFunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LambdaFunctionConfiguration
  ( LambdaFunctionConfiguration (..),

    -- * Smart constructor
    mkLambdaFunctionConfiguration,

    -- * Lenses
    lfcLambdaFunctionArn,
    lfcEvents,
    lfcFilter,
    lfcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Event as Types
import qualified Network.AWS.S3.Types.Id as Types
import qualified Network.AWS.S3.Types.LambdaFunctionArn as Types
import qualified Network.AWS.S3.Types.NotificationConfigurationFilter as Types

-- | A container for specifying the configuration for AWS Lambda notifications.
--
-- /See:/ 'mkLambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
    lambdaFunctionArn :: Types.LambdaFunctionArn,
    -- | The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
    events :: [Types.Event],
    filter :: Core.Maybe Types.NotificationConfigurationFilter,
    id :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaFunctionConfiguration' value with any optional fields omitted.
mkLambdaFunctionConfiguration ::
  -- | 'lambdaFunctionArn'
  Types.LambdaFunctionArn ->
  LambdaFunctionConfiguration
mkLambdaFunctionConfiguration lambdaFunctionArn =
  LambdaFunctionConfiguration'
    { lambdaFunctionArn,
      events = Core.mempty,
      filter = Core.Nothing,
      id = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcLambdaFunctionArn :: Lens.Lens' LambdaFunctionConfiguration Types.LambdaFunctionArn
lfcLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# DEPRECATED lfcLambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead." #-}

-- | The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcEvents :: Lens.Lens' LambdaFunctionConfiguration [Types.Event]
lfcEvents = Lens.field @"events"
{-# DEPRECATED lfcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcFilter :: Lens.Lens' LambdaFunctionConfiguration (Core.Maybe Types.NotificationConfigurationFilter)
lfcFilter = Lens.field @"filter"
{-# DEPRECATED lfcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcId :: Lens.Lens' LambdaFunctionConfiguration (Core.Maybe Types.Id)
lfcId = Lens.field @"id"
{-# DEPRECATED lfcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.ToXML LambdaFunctionConfiguration where
  toXML LambdaFunctionConfiguration {..} =
    Core.toXMLNode "CloudFunction" lambdaFunctionArn
      Core.<> Core.toXMLList "Event" events
      Core.<> Core.toXMLNode "Filter" Core.<$> filter
      Core.<> Core.toXMLNode "Id" Core.<$> id

instance Core.FromXML LambdaFunctionConfiguration where
  parseXML x =
    LambdaFunctionConfiguration'
      Core.<$> (x Core..@ "CloudFunction")
      Core.<*> (x Core..@? "Event" Core..@! Core.mempty)
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@? "Id")
