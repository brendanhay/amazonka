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
-- Module      : Amazonka.CloudFront.Types.OriginGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginGroup where

import Amazonka.CloudFront.Types.OriginGroupFailoverCriteria
import Amazonka.CloudFront.Types.OriginGroupMembers
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML OriginGroup where
  parseXML x =
    OriginGroup'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "FailoverCriteria")
      Prelude.<*> (x Data..@ "Members")

instance Prelude.Hashable OriginGroup where
  hashWithSalt _salt OriginGroup' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` failoverCriteria
      `Prelude.hashWithSalt` members

instance Prelude.NFData OriginGroup where
  rnf OriginGroup' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf failoverCriteria
      `Prelude.seq` Prelude.rnf members

instance Data.ToXML OriginGroup where
  toXML OriginGroup' {..} =
    Prelude.mconcat
      [ "Id" Data.@= id,
        "FailoverCriteria" Data.@= failoverCriteria,
        "Members" Data.@= members
      ]
