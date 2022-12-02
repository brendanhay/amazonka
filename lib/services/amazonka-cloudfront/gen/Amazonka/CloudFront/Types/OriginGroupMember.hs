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
-- Module      : Amazonka.CloudFront.Types.OriginGroupMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginGroupMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An origin in an origin group.
--
-- /See:/ 'newOriginGroupMember' smart constructor.
data OriginGroupMember = OriginGroupMember'
  { -- | The ID for an origin in an origin group.
    originId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  OriginGroupMember
newOriginGroupMember pOriginId_ =
  OriginGroupMember' {originId = pOriginId_}

-- | The ID for an origin in an origin group.
originGroupMember_originId :: Lens.Lens' OriginGroupMember Prelude.Text
originGroupMember_originId = Lens.lens (\OriginGroupMember' {originId} -> originId) (\s@OriginGroupMember' {} a -> s {originId = a} :: OriginGroupMember)

instance Data.FromXML OriginGroupMember where
  parseXML x =
    OriginGroupMember'
      Prelude.<$> (x Data..@ "OriginId")

instance Prelude.Hashable OriginGroupMember where
  hashWithSalt _salt OriginGroupMember' {..} =
    _salt `Prelude.hashWithSalt` originId

instance Prelude.NFData OriginGroupMember where
  rnf OriginGroupMember' {..} = Prelude.rnf originId

instance Data.ToXML OriginGroupMember where
  toXML OriginGroupMember' {..} =
    Prelude.mconcat ["OriginId" Data.@= originId]
