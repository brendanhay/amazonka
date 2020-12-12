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
    lfcId,
    lfcFilter,
    lfcLambdaFunctionARN,
    lfcEvents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for AWS Lambda notifications.
--
-- /See:/ 'mkLambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { id ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe
        NotificationConfigurationFilter,
    lambdaFunctionARN :: Lude.Text,
    events :: [Event]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaFunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'events' - The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'filter' - Undocumented field.
-- * 'id' - Undocumented field.
-- * 'lambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
mkLambdaFunctionConfiguration ::
  -- | 'lambdaFunctionARN'
  Lude.Text ->
  LambdaFunctionConfiguration
mkLambdaFunctionConfiguration pLambdaFunctionARN_ =
  LambdaFunctionConfiguration'
    { id = Lude.Nothing,
      filter = Lude.Nothing,
      lambdaFunctionARN = pLambdaFunctionARN_,
      events = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcId :: Lens.Lens' LambdaFunctionConfiguration (Lude.Maybe Lude.Text)
lfcId = Lens.lens (id :: LambdaFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: LambdaFunctionConfiguration)
{-# DEPRECATED lfcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcFilter :: Lens.Lens' LambdaFunctionConfiguration (Lude.Maybe NotificationConfigurationFilter)
lfcFilter = Lens.lens (filter :: LambdaFunctionConfiguration -> Lude.Maybe NotificationConfigurationFilter) (\s a -> s {filter = a} :: LambdaFunctionConfiguration)
{-# DEPRECATED lfcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcLambdaFunctionARN :: Lens.Lens' LambdaFunctionConfiguration Lude.Text
lfcLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: LambdaFunctionConfiguration -> Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: LambdaFunctionConfiguration)
{-# DEPRECATED lfcLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

-- | The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfcEvents :: Lens.Lens' LambdaFunctionConfiguration [Event]
lfcEvents = Lens.lens (events :: LambdaFunctionConfiguration -> [Event]) (\s a -> s {events = a} :: LambdaFunctionConfiguration)
{-# DEPRECATED lfcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.FromXML LambdaFunctionConfiguration where
  parseXML x =
    LambdaFunctionConfiguration'
      Lude.<$> (x Lude..@? "Id")
      Lude.<*> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@ "CloudFunction")
      Lude.<*> (Lude.parseXMLList "Event" x)

instance Lude.ToXML LambdaFunctionConfiguration where
  toXML LambdaFunctionConfiguration' {..} =
    Lude.mconcat
      [ "Id" Lude.@= id,
        "Filter" Lude.@= filter,
        "CloudFunction" Lude.@= lambdaFunctionARN,
        Lude.toXMLList "Event" events
      ]
