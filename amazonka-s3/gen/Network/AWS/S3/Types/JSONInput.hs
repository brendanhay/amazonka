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
-- Module      : Network.AWS.S3.Types.JSONInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.JSONType

-- | Specifies JSON as object\'s input serialization format.
--
-- /See:/ 'newJSONInput' smart constructor.
data JSONInput = JSONInput'
  { -- | The type of JSON. Valid values: Document, Lines.
    type' :: Core.Maybe JSONType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JSONInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'jSONInput_type' - The type of JSON. Valid values: Document, Lines.
newJSONInput ::
  JSONInput
newJSONInput = JSONInput' {type' = Core.Nothing}

-- | The type of JSON. Valid values: Document, Lines.
jSONInput_type :: Lens.Lens' JSONInput (Core.Maybe JSONType)
jSONInput_type = Lens.lens (\JSONInput' {type'} -> type') (\s@JSONInput' {} a -> s {type' = a} :: JSONInput)

instance Core.Hashable JSONInput

instance Core.NFData JSONInput

instance Core.ToXML JSONInput where
  toXML JSONInput' {..} =
    Core.mconcat ["Type" Core.@= type']
