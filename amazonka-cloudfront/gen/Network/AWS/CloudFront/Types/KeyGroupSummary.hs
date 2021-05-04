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
-- Module      : Network.AWS.CloudFront.Types.KeyGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupSummary where

import Network.AWS.CloudFront.Types.KeyGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a key group.
--
-- /See:/ 'newKeyGroupSummary' smart constructor.
data KeyGroupSummary = KeyGroupSummary'
  { -- | A key group.
    keyGroup :: KeyGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyGroup', 'keyGroupSummary_keyGroup' - A key group.
newKeyGroupSummary ::
  -- | 'keyGroup'
  KeyGroup ->
  KeyGroupSummary
newKeyGroupSummary pKeyGroup_ =
  KeyGroupSummary' {keyGroup = pKeyGroup_}

-- | A key group.
keyGroupSummary_keyGroup :: Lens.Lens' KeyGroupSummary KeyGroup
keyGroupSummary_keyGroup = Lens.lens (\KeyGroupSummary' {keyGroup} -> keyGroup) (\s@KeyGroupSummary' {} a -> s {keyGroup = a} :: KeyGroupSummary)

instance Prelude.FromXML KeyGroupSummary where
  parseXML x =
    KeyGroupSummary'
      Prelude.<$> (x Prelude..@ "KeyGroup")

instance Prelude.Hashable KeyGroupSummary

instance Prelude.NFData KeyGroupSummary
