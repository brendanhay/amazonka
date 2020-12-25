{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeInfo
  ( ChangeInfo (..),

    -- * Smart constructor
    mkChangeInfo,

    -- * Lenses
    ciId,
    ciStatus,
    ciSubmittedAt,
    ciComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ChangeStatus as Types
import qualified Network.AWS.Route53.Types.ResourceDescription as Types

-- | A complex type that describes change information about changes made to your hosted zone.
--
-- /See:/ 'mkChangeInfo' smart constructor.
data ChangeInfo = ChangeInfo'
  { -- | The ID of the request.
    id :: Types.ResourceId,
    -- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
    status :: Types.ChangeStatus,
    -- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
    submittedAt :: Core.UTCTime,
    -- | A complex type that describes change information about changes made to your hosted zone.
    --
    -- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
    comment :: Core.Maybe Types.ResourceDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ChangeInfo' value with any optional fields omitted.
mkChangeInfo ::
  -- | 'id'
  Types.ResourceId ->
  -- | 'status'
  Types.ChangeStatus ->
  -- | 'submittedAt'
  Core.UTCTime ->
  ChangeInfo
mkChangeInfo id status submittedAt =
  ChangeInfo' {id, status, submittedAt, comment = Core.Nothing}

-- | The ID of the request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ChangeInfo Types.ResourceId
ciId = Lens.field @"id"
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' ChangeInfo Types.ChangeStatus
ciStatus = Lens.field @"status"
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
--
-- /Note:/ Consider using 'submittedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSubmittedAt :: Lens.Lens' ChangeInfo Core.UTCTime
ciSubmittedAt = Lens.field @"submittedAt"
{-# DEPRECATED ciSubmittedAt "Use generic-lens or generic-optics with 'submittedAt' instead." #-}

-- | A complex type that describes change information about changes made to your hosted zone.
--
-- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComment :: Lens.Lens' ChangeInfo (Core.Maybe Types.ResourceDescription)
ciComment = Lens.field @"comment"
{-# DEPRECATED ciComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.FromXML ChangeInfo where
  parseXML x =
    ChangeInfo'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "SubmittedAt")
      Core.<*> (x Core..@? "Comment")
