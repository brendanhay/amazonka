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
-- Module      : Network.AWS.ConnectContactLens.Types.IssueDetected
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectContactLens.Types.IssueDetected where

import Network.AWS.ConnectContactLens.Types.CharacterOffsets
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Potential issues that are detected based on an artificial intelligence
-- analysis of each turn in the conversation.
--
-- /See:/ 'newIssueDetected' smart constructor.
data IssueDetected = IssueDetected'
  { -- | The offset for when the issue was detected in the segment.
    characterOffsets :: CharacterOffsets
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IssueDetected' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'characterOffsets', 'issueDetected_characterOffsets' - The offset for when the issue was detected in the segment.
newIssueDetected ::
  -- | 'characterOffsets'
  CharacterOffsets ->
  IssueDetected
newIssueDetected pCharacterOffsets_ =
  IssueDetected'
    { characterOffsets =
        pCharacterOffsets_
    }

-- | The offset for when the issue was detected in the segment.
issueDetected_characterOffsets :: Lens.Lens' IssueDetected CharacterOffsets
issueDetected_characterOffsets = Lens.lens (\IssueDetected' {characterOffsets} -> characterOffsets) (\s@IssueDetected' {} a -> s {characterOffsets = a} :: IssueDetected)

instance Core.FromJSON IssueDetected where
  parseJSON =
    Core.withObject
      "IssueDetected"
      ( \x ->
          IssueDetected'
            Prelude.<$> (x Core..: "CharacterOffsets")
      )

instance Prelude.Hashable IssueDetected

instance Prelude.NFData IssueDetected
