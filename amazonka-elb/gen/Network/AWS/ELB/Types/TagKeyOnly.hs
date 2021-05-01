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
-- Module      : Network.AWS.ELB.Types.TagKeyOnly
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagKeyOnly where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The key of a tag.
--
-- /See:/ 'newTagKeyOnly' smart constructor.
data TagKeyOnly = TagKeyOnly'
  { -- | The name of the key.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagKeyOnly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagKeyOnly_key' - The name of the key.
newTagKeyOnly ::
  TagKeyOnly
newTagKeyOnly = TagKeyOnly' {key = Prelude.Nothing}

-- | The name of the key.
tagKeyOnly_key :: Lens.Lens' TagKeyOnly (Prelude.Maybe Prelude.Text)
tagKeyOnly_key = Lens.lens (\TagKeyOnly' {key} -> key) (\s@TagKeyOnly' {} a -> s {key = a} :: TagKeyOnly)

instance Prelude.Hashable TagKeyOnly

instance Prelude.NFData TagKeyOnly

instance Prelude.ToQuery TagKeyOnly where
  toQuery TagKeyOnly' {..} =
    Prelude.mconcat ["Key" Prelude.=: key]
