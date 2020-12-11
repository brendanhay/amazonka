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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the last time an AWS access key was used since IAM began tracking this information on April 22, 2015.
--
-- This data type is used as a response element in the 'GetAccessKeyLastUsed' operation.
--
-- /See:/ 'mkAccessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { lastUsedDate ::
      Lude.ISO8601,
    serviceName :: Lude.Text,
    region :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessKeyLastUsed' with the minimum fields required to make a request.
--
-- * 'lastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:
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
-- * 'region' - The AWS Region where this access key was most recently used. The value for this field is "N/A" in the following situations:
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
-- * 'serviceName' - The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:
--
--
--     * The user does not have an access key.
--
--
--     * An access key exists but has not been used since IAM started tracking this information.
--
--
--     * There is no sign-in data associated with the user.
mkAccessKeyLastUsed ::
  -- | 'lastUsedDate'
  Lude.ISO8601 ->
  -- | 'serviceName'
  Lude.Text ->
  -- | 'region'
  Lude.Text ->
  AccessKeyLastUsed
mkAccessKeyLastUsed pLastUsedDate_ pServiceName_ pRegion_ =
  AccessKeyLastUsed'
    { lastUsedDate = pLastUsedDate_,
      serviceName = pServiceName_,
      region = pRegion_
    }

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
akluLastUsedDate :: Lens.Lens' AccessKeyLastUsed Lude.ISO8601
akluLastUsedDate = Lens.lens (lastUsedDate :: AccessKeyLastUsed -> Lude.ISO8601) (\s a -> s {lastUsedDate = a} :: AccessKeyLastUsed)
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
akluServiceName :: Lens.Lens' AccessKeyLastUsed Lude.Text
akluServiceName = Lens.lens (serviceName :: AccessKeyLastUsed -> Lude.Text) (\s a -> s {serviceName = a} :: AccessKeyLastUsed)
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
akluRegion :: Lens.Lens' AccessKeyLastUsed Lude.Text
akluRegion = Lens.lens (region :: AccessKeyLastUsed -> Lude.Text) (\s a -> s {region = a} :: AccessKeyLastUsed)
{-# DEPRECATED akluRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromXML AccessKeyLastUsed where
  parseXML x =
    AccessKeyLastUsed'
      Lude.<$> (x Lude..@ "LastUsedDate")
      Lude.<*> (x Lude..@ "ServiceName")
      Lude.<*> (x Lude..@ "Region")
