{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessKeyLastUsed
  ( AccessKeyLastUsed (..),

    -- * Smart constructor
    mkAccessKeyLastUsed,

    -- * Lenses
    akluLastUsedDate,
    akluServiceName,
    akluRegion,
  )
where

import qualified Network.AWS.IAM.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the last time an AWS access key was used since IAM began tracking this information on April 22, 2015.
--
-- This data type is used as a response element in the 'GetAccessKeyLastUsed' operation.
--
-- /See:/ 'mkAccessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:
    --
    --
    --     * The user does not have an access key.
    --
    --
    --     * An access key exists but has not been used since IAM began tracking this information.
    --
    --
    --     * There is no sign-in data associated with the user.
    lastUsedDate :: Core.UTCTime,
    -- | The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:
    --
    --
    --     * The user does not have an access key.
    --
    --
    --     * An access key exists but has not been used since IAM started tracking this information.
    --
    --
    --     * There is no sign-in data associated with the user.
    serviceName :: Types.StringType,
    -- | The AWS Region where this access key was most recently used. The value for this field is "N/A" in the following situations:
    --
    --
    --     * The user does not have an access key.
    --
    --
    --     * An access key exists but has not been used since IAM began tracking this information.
    --
    --
    --     * There is no sign-in data associated with the user.
    --
    --
    -- For more information about AWS Regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
    region :: Types.StringType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AccessKeyLastUsed' value with any optional fields omitted.
mkAccessKeyLastUsed ::
  -- | 'lastUsedDate'
  Core.UTCTime ->
  -- | 'serviceName'
  Types.StringType ->
  -- | 'region'
  Types.StringType ->
  AccessKeyLastUsed
mkAccessKeyLastUsed lastUsedDate serviceName region =
  AccessKeyLastUsed' {lastUsedDate, serviceName, region}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:
--
--
--     * The user does not have an access key.
--
--
--     * An access key exists but has not been used since IAM began tracking this information.
--
--
--     * There is no sign-in data associated with the user.
--
--
--
-- /Note:/ Consider using 'lastUsedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akluLastUsedDate :: Lens.Lens' AccessKeyLastUsed Core.UTCTime
akluLastUsedDate = Lens.field @"lastUsedDate"
{-# DEPRECATED akluLastUsedDate "Use generic-lens or generic-optics with 'lastUsedDate' instead." #-}

-- | The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:
--
--
--     * The user does not have an access key.
--
--
--     * An access key exists but has not been used since IAM started tracking this information.
--
--
--     * There is no sign-in data associated with the user.
--
--
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akluServiceName :: Lens.Lens' AccessKeyLastUsed Types.StringType
akluServiceName = Lens.field @"serviceName"
{-# DEPRECATED akluServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The AWS Region where this access key was most recently used. The value for this field is "N/A" in the following situations:
--
--
--     * The user does not have an access key.
--
--
--     * An access key exists but has not been used since IAM began tracking this information.
--
--
--     * There is no sign-in data associated with the user.
--
--
-- For more information about AWS Regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akluRegion :: Lens.Lens' AccessKeyLastUsed Types.StringType
akluRegion = Lens.field @"region"
{-# DEPRECATED akluRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Core.FromXML AccessKeyLastUsed where
  parseXML x =
    AccessKeyLastUsed'
      Core.<$> (x Core..@ "LastUsedDate")
      Core.<*> (x Core..@ "ServiceName")
      Core.<*> (x Core..@ "Region")
