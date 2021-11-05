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
-- Module      : Network.AWS.SSMIncidents.Types.EmptyChatChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.EmptyChatChannel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to remove the chat channel from an incident record or response
-- plan.
--
-- /See:/ 'newEmptyChatChannel' smart constructor.
data EmptyChatChannel = EmptyChatChannel'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmptyChatChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEmptyChatChannel ::
  EmptyChatChannel
newEmptyChatChannel = EmptyChatChannel'

instance Core.FromJSON EmptyChatChannel where
  parseJSON =
    Core.withObject
      "EmptyChatChannel"
      (\x -> Prelude.pure EmptyChatChannel')

instance Prelude.Hashable EmptyChatChannel

instance Prelude.NFData EmptyChatChannel

instance Core.ToJSON EmptyChatChannel where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
