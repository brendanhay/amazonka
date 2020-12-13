{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.AffectedEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.AffectedEntity
  ( AffectedEntity (..),

    -- * Smart constructor
    mkAffectedEntity,

    -- * Lenses
    aeLastUpdatedTime,
    aeEntityValue,
    aeEntityURL,
    aeAwsAccountId,
    aeEventARN,
    aeEntityARN,
    aeTags,
    aeStatusCode,
  )
where

import Network.AWS.AWSHealth.Types.EntityStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an entity that is affected by a Health event.
--
-- /See:/ 'mkAffectedEntity' smart constructor.
data AffectedEntity = AffectedEntity'
  { -- | The most recent time that the entity was updated.
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the affected entity.
    entityValue :: Lude.Maybe Lude.Text,
    -- | The URL of the affected entity.
    entityURL :: Lude.Maybe Lude.Text,
    -- | The 12-digit AWS account number that contains the affected entity.
    awsAccountId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventARN :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
    entityARN :: Lude.Maybe Lude.Text,
    -- | A map of entity tags attached to the affected entity.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
    statusCode :: Lude.Maybe EntityStatusCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AffectedEntity' with the minimum fields required to make a request.
--
-- * 'lastUpdatedTime' - The most recent time that the entity was updated.
-- * 'entityValue' - The ID of the affected entity.
-- * 'entityURL' - The URL of the affected entity.
-- * 'awsAccountId' - The 12-digit AWS account number that contains the affected entity.
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
-- * 'entityARN' - The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
-- * 'tags' - A map of entity tags attached to the affected entity.
-- * 'statusCode' - The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
mkAffectedEntity ::
  AffectedEntity
mkAffectedEntity =
  AffectedEntity'
    { lastUpdatedTime = Lude.Nothing,
      entityValue = Lude.Nothing,
      entityURL = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      eventARN = Lude.Nothing,
      entityARN = Lude.Nothing,
      tags = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The most recent time that the entity was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastUpdatedTime :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Timestamp)
aeLastUpdatedTime = Lens.lens (lastUpdatedTime :: AffectedEntity -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: AffectedEntity)
{-# DEPRECATED aeLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The ID of the affected entity.
--
-- /Note:/ Consider using 'entityValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityValue :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Text)
aeEntityValue = Lens.lens (entityValue :: AffectedEntity -> Lude.Maybe Lude.Text) (\s a -> s {entityValue = a} :: AffectedEntity)
{-# DEPRECATED aeEntityValue "Use generic-lens or generic-optics with 'entityValue' instead." #-}

-- | The URL of the affected entity.
--
-- /Note:/ Consider using 'entityURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityURL :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Text)
aeEntityURL = Lens.lens (entityURL :: AffectedEntity -> Lude.Maybe Lude.Text) (\s a -> s {entityURL = a} :: AffectedEntity)
{-# DEPRECATED aeEntityURL "Use generic-lens or generic-optics with 'entityURL' instead." #-}

-- | The 12-digit AWS account number that contains the affected entity.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAwsAccountId :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Text)
aeAwsAccountId = Lens.lens (awsAccountId :: AffectedEntity -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: AffectedEntity)
{-# DEPRECATED aeAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEventARN :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Text)
aeEventARN = Lens.lens (eventARN :: AffectedEntity -> Lude.Maybe Lude.Text) (\s a -> s {eventARN = a} :: AffectedEntity)
{-# DEPRECATED aeEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

-- | The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
--
-- /Note:/ Consider using 'entityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityARN :: Lens.Lens' AffectedEntity (Lude.Maybe Lude.Text)
aeEntityARN = Lens.lens (entityARN :: AffectedEntity -> Lude.Maybe Lude.Text) (\s a -> s {entityARN = a} :: AffectedEntity)
{-# DEPRECATED aeEntityARN "Use generic-lens or generic-optics with 'entityARN' instead." #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTags :: Lens.Lens' AffectedEntity (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aeTags = Lens.lens (tags :: AffectedEntity -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: AffectedEntity)
{-# DEPRECATED aeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatusCode :: Lens.Lens' AffectedEntity (Lude.Maybe EntityStatusCode)
aeStatusCode = Lens.lens (statusCode :: AffectedEntity -> Lude.Maybe EntityStatusCode) (\s a -> s {statusCode = a} :: AffectedEntity)
{-# DEPRECATED aeStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON AffectedEntity where
  parseJSON =
    Lude.withObject
      "AffectedEntity"
      ( \x ->
          AffectedEntity'
            Lude.<$> (x Lude..:? "lastUpdatedTime")
            Lude.<*> (x Lude..:? "entityValue")
            Lude.<*> (x Lude..:? "entityUrl")
            Lude.<*> (x Lude..:? "awsAccountId")
            Lude.<*> (x Lude..:? "eventArn")
            Lude.<*> (x Lude..:? "entityArn")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statusCode")
      )
