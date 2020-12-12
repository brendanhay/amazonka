{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
  ( OrganizationAffectedEntitiesErrorItem (..),

    -- * Smart constructor
    mkOrganizationAffectedEntitiesErrorItem,

    -- * Lenses
    oaeeiAwsAccountId,
    oaeeiEventARN,
    oaeeiErrorName,
    oaeeiErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operation cannot find or process a specific entity.
--
-- /See:/ 'mkOrganizationAffectedEntitiesErrorItem' smart constructor.
data OrganizationAffectedEntitiesErrorItem = OrganizationAffectedEntitiesErrorItem'
  { awsAccountId ::
      Lude.Maybe
        Lude.Text,
    eventARN ::
      Lude.Maybe
        Lude.Text,
    errorName ::
      Lude.Maybe
        Lude.Text,
    errorMessage ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationAffectedEntitiesErrorItem' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
-- * 'errorMessage' - The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
-- * 'errorName' - The name of the error.
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
mkOrganizationAffectedEntitiesErrorItem ::
  OrganizationAffectedEntitiesErrorItem
mkOrganizationAffectedEntitiesErrorItem =
  OrganizationAffectedEntitiesErrorItem'
    { awsAccountId =
        Lude.Nothing,
      eventARN = Lude.Nothing,
      errorName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiAwsAccountId :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Lude.Maybe Lude.Text)
oaeeiAwsAccountId = Lens.lens (awsAccountId :: OrganizationAffectedEntitiesErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: OrganizationAffectedEntitiesErrorItem)
{-# DEPRECATED oaeeiAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiEventARN :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Lude.Maybe Lude.Text)
oaeeiEventARN = Lens.lens (eventARN :: OrganizationAffectedEntitiesErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {eventARN = a} :: OrganizationAffectedEntitiesErrorItem)
{-# DEPRECATED oaeeiEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

-- | The name of the error.
--
-- /Note:/ Consider using 'errorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiErrorName :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Lude.Maybe Lude.Text)
oaeeiErrorName = Lens.lens (errorName :: OrganizationAffectedEntitiesErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorName = a} :: OrganizationAffectedEntitiesErrorItem)
{-# DEPRECATED oaeeiErrorName "Use generic-lens or generic-optics with 'errorName' instead." #-}

-- | The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiErrorMessage :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Lude.Maybe Lude.Text)
oaeeiErrorMessage = Lens.lens (errorMessage :: OrganizationAffectedEntitiesErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationAffectedEntitiesErrorItem)
{-# DEPRECATED oaeeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON OrganizationAffectedEntitiesErrorItem where
  parseJSON =
    Lude.withObject
      "OrganizationAffectedEntitiesErrorItem"
      ( \x ->
          OrganizationAffectedEntitiesErrorItem'
            Lude.<$> (x Lude..:? "awsAccountId")
            Lude.<*> (x Lude..:? "eventArn")
            Lude.<*> (x Lude..:? "errorName")
            Lude.<*> (x Lude..:? "errorMessage")
      )
