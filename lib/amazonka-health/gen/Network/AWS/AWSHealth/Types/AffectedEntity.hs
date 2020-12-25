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
    aeAwsAccountId,
    aeEntityArn,
    aeEntityUrl,
    aeEntityValue,
    aeEventArn,
    aeLastUpdatedTime,
    aeStatusCode,
    aeTags,
  )
where

import qualified Network.AWS.AWSHealth.Types.AccountId as Types
import qualified Network.AWS.AWSHealth.Types.EntityArn as Types
import qualified Network.AWS.AWSHealth.Types.EntityStatusCode as Types
import qualified Network.AWS.AWSHealth.Types.EntityUrl as Types
import qualified Network.AWS.AWSHealth.Types.EntityValue as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.AWSHealth.Types.TagKey as Types
import qualified Network.AWS.AWSHealth.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an entity that is affected by a Health event.
--
-- /See:/ 'mkAffectedEntity' smart constructor.
data AffectedEntity = AffectedEntity'
  { -- | The 12-digit AWS account number that contains the affected entity.
    awsAccountId :: Core.Maybe Types.AccountId,
    -- | The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
    entityArn :: Core.Maybe Types.EntityArn,
    -- | The URL of the affected entity.
    entityUrl :: Core.Maybe Types.EntityUrl,
    -- | The ID of the affected entity.
    entityValue :: Core.Maybe Types.EntityValue,
    -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Core.Maybe Types.EventArn,
    -- | The most recent time that the entity was updated.
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
    statusCode :: Core.Maybe Types.EntityStatusCode,
    -- | A map of entity tags attached to the affected entity.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AffectedEntity' value with any optional fields omitted.
mkAffectedEntity ::
  AffectedEntity
mkAffectedEntity =
  AffectedEntity'
    { awsAccountId = Core.Nothing,
      entityArn = Core.Nothing,
      entityUrl = Core.Nothing,
      entityValue = Core.Nothing,
      eventArn = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      statusCode = Core.Nothing,
      tags = Core.Nothing
    }

-- | The 12-digit AWS account number that contains the affected entity.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAwsAccountId :: Lens.Lens' AffectedEntity (Core.Maybe Types.AccountId)
aeAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED aeAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The unique identifier for the entity. Format: @arn:aws:health:/entity-region/ :/aws-account/ :entity//entity-id/ @ . Example: @arn:aws:health:us-east-1:111222333444:entity/AVh5GGT7ul1arKr1sE1K@
--
-- /Note:/ Consider using 'entityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityArn :: Lens.Lens' AffectedEntity (Core.Maybe Types.EntityArn)
aeEntityArn = Lens.field @"entityArn"
{-# DEPRECATED aeEntityArn "Use generic-lens or generic-optics with 'entityArn' instead." #-}

-- | The URL of the affected entity.
--
-- /Note:/ Consider using 'entityUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityUrl :: Lens.Lens' AffectedEntity (Core.Maybe Types.EntityUrl)
aeEntityUrl = Lens.field @"entityUrl"
{-# DEPRECATED aeEntityUrl "Use generic-lens or generic-optics with 'entityUrl' instead." #-}

-- | The ID of the affected entity.
--
-- /Note:/ Consider using 'entityValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEntityValue :: Lens.Lens' AffectedEntity (Core.Maybe Types.EntityValue)
aeEntityValue = Lens.field @"entityValue"
{-# DEPRECATED aeEntityValue "Use generic-lens or generic-optics with 'entityValue' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeEventArn :: Lens.Lens' AffectedEntity (Core.Maybe Types.EventArn)
aeEventArn = Lens.field @"eventArn"
{-# DEPRECATED aeEventArn "Use generic-lens or generic-optics with 'eventArn' instead." #-}

-- | The most recent time that the entity was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastUpdatedTime :: Lens.Lens' AffectedEntity (Core.Maybe Core.NominalDiffTime)
aeLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED aeLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The most recent status of the entity affected by the event. The possible values are @IMPAIRED@ , @UNIMPAIRED@ , and @UNKNOWN@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatusCode :: Lens.Lens' AffectedEntity (Core.Maybe Types.EntityStatusCode)
aeStatusCode = Lens.field @"statusCode"
{-# DEPRECATED aeStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTags :: Lens.Lens' AffectedEntity (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
aeTags = Lens.field @"tags"
{-# DEPRECATED aeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON AffectedEntity where
  parseJSON =
    Core.withObject "AffectedEntity" Core.$
      \x ->
        AffectedEntity'
          Core.<$> (x Core..:? "awsAccountId")
          Core.<*> (x Core..:? "entityArn")
          Core.<*> (x Core..:? "entityUrl")
          Core.<*> (x Core..:? "entityValue")
          Core.<*> (x Core..:? "eventArn")
          Core.<*> (x Core..:? "lastUpdatedTime")
          Core.<*> (x Core..:? "statusCode")
          Core.<*> (x Core..:? "tags")
