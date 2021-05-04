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
-- Module      : Network.AWS.CloudFront.Types.OriginGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroup where

import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
import Network.AWS.CloudFront.Types.OriginGroupMembers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An origin group includes two origins (a primary origin and a second
-- origin to failover to) and a failover criteria that you specify. You
-- create an origin group to support origin failover in CloudFront. When
-- you create or update a distribution, you can specifiy the origin group
-- instead of a single origin, and CloudFront will failover from the
-- primary origin to the second origin under the failover conditions that
-- you\'ve chosen.
--
-- /See:/ 'newOriginGroup' smart constructor.
data OriginGroup = OriginGroup'
  { -- | The origin group\'s ID.
    id :: Prelude.Text,
    -- | A complex type that contains information about the failover criteria for
    -- an origin group.
    failoverCriteria :: OriginGroupFailoverCriteria,
    -- | A complex type that contains information about the origins in an origin
    -- group.
    members :: OriginGroupMembers
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OriginGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'originGroup_id' - The origin group\'s ID.
--
-- 'failoverCriteria', 'originGroup_failoverCriteria' - A complex type that contains information about the failover criteria for
-- an origin group.
--
-- 'members', 'originGroup_members' - A complex type that contains information about the origins in an origin
-- group.
newOriginGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'failoverCriteria'
  OriginGroupFailoverCriteria ->
  -- | 'members'
  OriginGroupMembers ->
  OriginGroup
newOriginGroup pId_ pFailoverCriteria_ pMembers_ =
  OriginGroup'
    { id = pId_,
      failoverCriteria = pFailoverCriteria_,
      members = pMembers_
    }

-- | The origin group\'s ID.
originGroup_id :: Lens.Lens' OriginGroup Prelude.Text
originGroup_id = Lens.lens (\OriginGroup' {id} -> id) (\s@OriginGroup' {} a -> s {id = a} :: OriginGroup)

-- | A complex type that contains information about the failover criteria for
-- an origin group.
originGroup_failoverCriteria :: Lens.Lens' OriginGroup OriginGroupFailoverCriteria
originGroup_failoverCriteria = Lens.lens (\OriginGroup' {failoverCriteria} -> failoverCriteria) (\s@OriginGroup' {} a -> s {failoverCriteria = a} :: OriginGroup)

-- | A complex type that contains information about the origins in an origin
-- group.
originGroup_members :: Lens.Lens' OriginGroup OriginGroupMembers
originGroup_members = Lens.lens (\OriginGroup' {members} -> members) (\s@OriginGroup' {} a -> s {members = a} :: OriginGroup)

instance Prelude.FromXML OriginGroup where
  parseXML x =
    OriginGroup'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "FailoverCriteria")
      Prelude.<*> (x Prelude..@ "Members")

instance Prelude.Hashable OriginGroup

instance Prelude.NFData OriginGroup

instance Prelude.ToXML OriginGroup where
  toXML OriginGroup' {..} =
    Prelude.mconcat
      [ "Id" Prelude.@= id,
        "FailoverCriteria" Prelude.@= failoverCriteria,
        "Members" Prelude.@= members
      ]
