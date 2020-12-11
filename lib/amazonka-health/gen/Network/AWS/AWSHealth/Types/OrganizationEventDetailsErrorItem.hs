-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
  ( OrganizationEventDetailsErrorItem (..),

    -- * Smart constructor
    mkOrganizationEventDetailsErrorItem,

    -- * Lenses
    oedeiAwsAccountId,
    oedeiEventARN,
    oedeiErrorName,
    oedeiErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
-- /See:/ 'mkOrganizationEventDetailsErrorItem' smart constructor.
data OrganizationEventDetailsErrorItem = OrganizationEventDetailsErrorItem'
  { awsAccountId ::
      Lude.Maybe Lude.Text,
    eventARN ::
      Lude.Maybe Lude.Text,
    errorName ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationEventDetailsErrorItem' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
-- * 'errorMessage' - A message that describes the error.
-- * 'errorName' - The name of the error.
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
mkOrganizationEventDetailsErrorItem ::
  OrganizationEventDetailsErrorItem
mkOrganizationEventDetailsErrorItem =
  OrganizationEventDetailsErrorItem'
    { awsAccountId = Lude.Nothing,
      eventARN = Lude.Nothing,
      errorName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiAwsAccountId :: Lens.Lens' OrganizationEventDetailsErrorItem (Lude.Maybe Lude.Text)
oedeiAwsAccountId = Lens.lens (awsAccountId :: OrganizationEventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: OrganizationEventDetailsErrorItem)
{-# DEPRECATED oedeiAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiEventARN :: Lens.Lens' OrganizationEventDetailsErrorItem (Lude.Maybe Lude.Text)
oedeiEventARN = Lens.lens (eventARN :: OrganizationEventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {eventARN = a} :: OrganizationEventDetailsErrorItem)
{-# DEPRECATED oedeiEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

-- | The name of the error.
--
-- /Note:/ Consider using 'errorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiErrorName :: Lens.Lens' OrganizationEventDetailsErrorItem (Lude.Maybe Lude.Text)
oedeiErrorName = Lens.lens (errorName :: OrganizationEventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorName = a} :: OrganizationEventDetailsErrorItem)
{-# DEPRECATED oedeiErrorName "Use generic-lens or generic-optics with 'errorName' instead." #-}

-- | A message that describes the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiErrorMessage :: Lens.Lens' OrganizationEventDetailsErrorItem (Lude.Maybe Lude.Text)
oedeiErrorMessage = Lens.lens (errorMessage :: OrganizationEventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationEventDetailsErrorItem)
{-# DEPRECATED oedeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON OrganizationEventDetailsErrorItem where
  parseJSON =
    Lude.withObject
      "OrganizationEventDetailsErrorItem"
      ( \x ->
          OrganizationEventDetailsErrorItem'
            Lude.<$> (x Lude..:? "awsAccountId")
            Lude.<*> (x Lude..:? "eventArn")
            Lude.<*> (x Lude..:? "errorName")
            Lude.<*> (x Lude..:? "errorMessage")
      )
