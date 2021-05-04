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
-- Module      : Network.AWS.Config.Types.QueryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.QueryInfo where

import Network.AWS.Config.Types.FieldInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the query.
--
-- /See:/ 'newQueryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { -- | Returns a @FieldInfo@ object.
    selectFields :: Prelude.Maybe [FieldInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  QueryInfo' {selectFields = Prelude.Nothing}

-- | Returns a @FieldInfo@ object.
queryInfo_selectFields :: Lens.Lens' QueryInfo (Prelude.Maybe [FieldInfo])
queryInfo_selectFields = Lens.lens (\QueryInfo' {selectFields} -> selectFields) (\s@QueryInfo' {} a -> s {selectFields = a} :: QueryInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON QueryInfo where
  parseJSON =
    Prelude.withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            Prelude.<$> ( x Prelude..:? "SelectFields"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable QueryInfo

instance Prelude.NFData QueryInfo
