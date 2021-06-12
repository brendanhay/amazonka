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
-- Module      : Network.AWS.Config.Types.QueryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.QueryInfo where

import Network.AWS.Config.Types.FieldInfo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the query.
--
-- /See:/ 'newQueryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { -- | Returns a @FieldInfo@ object.
    selectFields :: Core.Maybe [FieldInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectFields', 'queryInfo_selectFields' - Returns a @FieldInfo@ object.
newQueryInfo ::
  QueryInfo
newQueryInfo =
  QueryInfo' {selectFields = Core.Nothing}

-- | Returns a @FieldInfo@ object.
queryInfo_selectFields :: Lens.Lens' QueryInfo (Core.Maybe [FieldInfo])
queryInfo_selectFields = Lens.lens (\QueryInfo' {selectFields} -> selectFields) (\s@QueryInfo' {} a -> s {selectFields = a} :: QueryInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON QueryInfo where
  parseJSON =
    Core.withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            Core.<$> (x Core..:? "SelectFields" Core..!= Core.mempty)
      )

instance Core.Hashable QueryInfo

instance Core.NFData QueryInfo
