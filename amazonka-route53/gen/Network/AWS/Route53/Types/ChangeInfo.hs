{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeStatus

-- | A complex type that describes change information about changes made to
-- your hosted zone.
--
-- /See:/ 'newChangeInfo' smart constructor.
data ChangeInfo = ChangeInfo'
  { -- | A complex type that describes change information about changes made to
    -- your hosted zone.
    --
    -- This element contains an ID that you use when performing a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
    -- action to get detailed information about the change.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The ID of the request.
    id :: ResourceId,
    -- | The current state of the request. @PENDING@ indicates that this request
    -- has not yet been applied to all Amazon Route 53 DNS servers.
    status :: ChangeStatus,
    -- | The date and time that the change request was submitted in
    -- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
    -- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
    -- represents March 27, 2017 at 17:48:16.751 UTC.
    submittedAt :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChangeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'changeInfo_comment' - A complex type that describes change information about changes made to
-- your hosted zone.
--
-- This element contains an ID that you use when performing a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
-- action to get detailed information about the change.
--
-- 'id', 'changeInfo_id' - The ID of the request.
--
-- 'status', 'changeInfo_status' - The current state of the request. @PENDING@ indicates that this request
-- has not yet been applied to all Amazon Route 53 DNS servers.
--
-- 'submittedAt', 'changeInfo_submittedAt' - The date and time that the change request was submitted in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
newChangeInfo ::
  -- | 'id'
  ResourceId ->
  -- | 'status'
  ChangeStatus ->
  -- | 'submittedAt'
  Prelude.UTCTime ->
  ChangeInfo
newChangeInfo pId_ pStatus_ pSubmittedAt_ =
  ChangeInfo'
    { comment = Prelude.Nothing,
      id = pId_,
      status = pStatus_,
      submittedAt = Prelude._Time Lens.# pSubmittedAt_
    }

-- | A complex type that describes change information about changes made to
-- your hosted zone.
--
-- This element contains an ID that you use when performing a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
-- action to get detailed information about the change.
changeInfo_comment :: Lens.Lens' ChangeInfo (Prelude.Maybe Prelude.Text)
changeInfo_comment = Lens.lens (\ChangeInfo' {comment} -> comment) (\s@ChangeInfo' {} a -> s {comment = a} :: ChangeInfo)

-- | The ID of the request.
changeInfo_id :: Lens.Lens' ChangeInfo ResourceId
changeInfo_id = Lens.lens (\ChangeInfo' {id} -> id) (\s@ChangeInfo' {} a -> s {id = a} :: ChangeInfo)

-- | The current state of the request. @PENDING@ indicates that this request
-- has not yet been applied to all Amazon Route 53 DNS servers.
changeInfo_status :: Lens.Lens' ChangeInfo ChangeStatus
changeInfo_status = Lens.lens (\ChangeInfo' {status} -> status) (\s@ChangeInfo' {} a -> s {status = a} :: ChangeInfo)

-- | The date and time that the change request was submitted in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
changeInfo_submittedAt :: Lens.Lens' ChangeInfo Prelude.UTCTime
changeInfo_submittedAt = Lens.lens (\ChangeInfo' {submittedAt} -> submittedAt) (\s@ChangeInfo' {} a -> s {submittedAt = a} :: ChangeInfo) Prelude.. Prelude._Time

instance Prelude.FromXML ChangeInfo where
  parseXML x =
    ChangeInfo'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "Status")
      Prelude.<*> (x Prelude..@ "SubmittedAt")

instance Prelude.Hashable ChangeInfo

instance Prelude.NFData ChangeInfo
