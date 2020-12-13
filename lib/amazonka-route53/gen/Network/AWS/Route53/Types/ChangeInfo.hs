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
    ciStatus,
    ciSubmittedAt,
    ciId,
    ciComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeStatus

-- | A complex type that describes change information about changes made to your hosted zone.
--
-- /See:/ 'mkChangeInfo' smart constructor.
data ChangeInfo = ChangeInfo'
  { -- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
    status :: ChangeStatus,
    -- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
    submittedAt :: Lude.DateTime,
    -- | The ID of the request.
    id :: ResourceId,
    -- | A complex type that describes change information about changes made to your hosted zone.
    --
    -- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeInfo' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
-- * 'submittedAt' - The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
-- * 'id' - The ID of the request.
-- * 'comment' - A complex type that describes change information about changes made to your hosted zone.
--
-- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
mkChangeInfo ::
  -- | 'status'
  ChangeStatus ->
  -- | 'submittedAt'
  Lude.DateTime ->
  -- | 'id'
  ResourceId ->
  ChangeInfo
mkChangeInfo pStatus_ pSubmittedAt_ pId_ =
  ChangeInfo'
    { status = pStatus_,
      submittedAt = pSubmittedAt_,
      id = pId_,
      comment = Lude.Nothing
    }

-- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' ChangeInfo ChangeStatus
ciStatus = Lens.lens (status :: ChangeInfo -> ChangeStatus) (\s a -> s {status = a} :: ChangeInfo)
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
--
-- /Note:/ Consider using 'submittedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSubmittedAt :: Lens.Lens' ChangeInfo Lude.DateTime
ciSubmittedAt = Lens.lens (submittedAt :: ChangeInfo -> Lude.DateTime) (\s a -> s {submittedAt = a} :: ChangeInfo)
{-# DEPRECATED ciSubmittedAt "Use generic-lens or generic-optics with 'submittedAt' instead." #-}

-- | The ID of the request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ChangeInfo ResourceId
ciId = Lens.lens (id :: ChangeInfo -> ResourceId) (\s a -> s {id = a} :: ChangeInfo)
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A complex type that describes change information about changes made to your hosted zone.
--
-- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComment :: Lens.Lens' ChangeInfo (Lude.Maybe Lude.Text)
ciComment = Lens.lens (comment :: ChangeInfo -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: ChangeInfo)
{-# DEPRECATED ciComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML ChangeInfo where
  parseXML x =
    ChangeInfo'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "SubmittedAt")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "Comment")
