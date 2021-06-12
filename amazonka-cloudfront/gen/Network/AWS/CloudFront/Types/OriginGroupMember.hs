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
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An origin in an origin group.
--
-- /See:/ 'newOriginGroupMember' smart constructor.
data OriginGroupMember = OriginGroupMember'
  { -- | The ID for an origin in an origin group.
    originId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginGroupMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originId', 'originGroupMember_originId' - The ID for an origin in an origin group.
newOriginGroupMember ::
  -- | 'originId'
  Core.Text ->
  OriginGroupMember
newOriginGroupMember pOriginId_ =
  OriginGroupMember' {originId = pOriginId_}

-- | The ID for an origin in an origin group.
originGroupMember_originId :: Lens.Lens' OriginGroupMember Core.Text
originGroupMember_originId = Lens.lens (\OriginGroupMember' {originId} -> originId) (\s@OriginGroupMember' {} a -> s {originId = a} :: OriginGroupMember)

instance Core.FromXML OriginGroupMember where
  parseXML x =
    OriginGroupMember' Core.<$> (x Core..@ "OriginId")

instance Core.Hashable OriginGroupMember

instance Core.NFData OriginGroupMember

instance Core.ToXML OriginGroupMember where
  toXML OriginGroupMember' {..} =
    Core.mconcat ["OriginId" Core.@= originId]
