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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserQSearchBarEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Q search bar embedding experience.
--
-- /See:/ 'newRegisteredUserQSearchBarEmbeddingConfiguration' smart constructor.
data RegisteredUserQSearchBarEmbeddingConfiguration = RegisteredUserQSearchBarEmbeddingConfiguration'
  { -- | The ID of the Q topic that you want to make the starting topic in the Q
    -- search bar. You can find a topic ID by navigating to the Topics pane in
    -- the Amazon QuickSight application and opening a topic. The ID is in the
    -- URL for the topic that you open.
    --
    -- If you don\'t specify an initial topic, a list of all shared topics is
    -- shown in the Q bar for your readers. When you select an initial topic,
    -- you can specify whether or not readers are allowed to select other
    -- topics from the available ones in the list.
    initialTopicId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserQSearchBarEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialTopicId', 'registeredUserQSearchBarEmbeddingConfiguration_initialTopicId' - The ID of the Q topic that you want to make the starting topic in the Q
-- search bar. You can find a topic ID by navigating to the Topics pane in
-- the Amazon QuickSight application and opening a topic. The ID is in the
-- URL for the topic that you open.
--
-- If you don\'t specify an initial topic, a list of all shared topics is
-- shown in the Q bar for your readers. When you select an initial topic,
-- you can specify whether or not readers are allowed to select other
-- topics from the available ones in the list.
newRegisteredUserQSearchBarEmbeddingConfiguration ::
  RegisteredUserQSearchBarEmbeddingConfiguration
newRegisteredUserQSearchBarEmbeddingConfiguration =
  RegisteredUserQSearchBarEmbeddingConfiguration'
    { initialTopicId =
        Prelude.Nothing
    }

-- | The ID of the Q topic that you want to make the starting topic in the Q
-- search bar. You can find a topic ID by navigating to the Topics pane in
-- the Amazon QuickSight application and opening a topic. The ID is in the
-- URL for the topic that you open.
--
-- If you don\'t specify an initial topic, a list of all shared topics is
-- shown in the Q bar for your readers. When you select an initial topic,
-- you can specify whether or not readers are allowed to select other
-- topics from the available ones in the list.
registeredUserQSearchBarEmbeddingConfiguration_initialTopicId :: Lens.Lens' RegisteredUserQSearchBarEmbeddingConfiguration (Prelude.Maybe Prelude.Text)
registeredUserQSearchBarEmbeddingConfiguration_initialTopicId = Lens.lens (\RegisteredUserQSearchBarEmbeddingConfiguration' {initialTopicId} -> initialTopicId) (\s@RegisteredUserQSearchBarEmbeddingConfiguration' {} a -> s {initialTopicId = a} :: RegisteredUserQSearchBarEmbeddingConfiguration)

instance
  Prelude.Hashable
    RegisteredUserQSearchBarEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    RegisteredUserQSearchBarEmbeddingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` initialTopicId

instance
  Prelude.NFData
    RegisteredUserQSearchBarEmbeddingConfiguration
  where
  rnf
    RegisteredUserQSearchBarEmbeddingConfiguration' {..} =
      Prelude.rnf initialTopicId

instance
  Data.ToJSON
    RegisteredUserQSearchBarEmbeddingConfiguration
  where
  toJSON
    RegisteredUserQSearchBarEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("InitialTopicId" Data..=)
                Prelude.<$> initialTopicId
            ]
        )
