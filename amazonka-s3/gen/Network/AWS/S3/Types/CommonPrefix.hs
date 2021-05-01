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
-- Module      : Network.AWS.S3.Types.CommonPrefix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CommonPrefix where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for all (if there are any) keys between Prefix and the next
-- occurrence of the string specified by a delimiter. CommonPrefixes lists
-- keys that act like subdirectories in the directory specified by Prefix.
-- For example, if the prefix is notes\/ and the delimiter is a slash (\/)
-- as in notes\/summer\/july, the common prefix is notes\/summer\/.
--
-- /See:/ 'newCommonPrefix' smart constructor.
data CommonPrefix = CommonPrefix'
  { -- | Container for the specified common prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CommonPrefix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'commonPrefix_prefix' - Container for the specified common prefix.
newCommonPrefix ::
  CommonPrefix
newCommonPrefix =
  CommonPrefix' {prefix = Prelude.Nothing}

-- | Container for the specified common prefix.
commonPrefix_prefix :: Lens.Lens' CommonPrefix (Prelude.Maybe Prelude.Text)
commonPrefix_prefix = Lens.lens (\CommonPrefix' {prefix} -> prefix) (\s@CommonPrefix' {} a -> s {prefix = a} :: CommonPrefix)

instance Prelude.FromXML CommonPrefix where
  parseXML x =
    CommonPrefix' Prelude.<$> (x Prelude..@? "Prefix")

instance Prelude.Hashable CommonPrefix

instance Prelude.NFData CommonPrefix
