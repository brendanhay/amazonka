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
-- Module      : Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings that you want to use with the Q search bar.
--
-- /See:/ 'newAnonymousUserQSearchBarEmbeddingConfiguration' smart constructor.
data AnonymousUserQSearchBarEmbeddingConfiguration = AnonymousUserQSearchBarEmbeddingConfiguration'
  { -- | The QuickSight Q topic ID of the topic that you want the anonymous user
    -- to see first. This ID is included in the output URL. When the URL in
    -- response is accessed, Amazon QuickSight renders the Q search bar with
    -- this topic pre-selected.
    --
    -- The Amazon Resource Name (ARN) of this Q topic must be included in the
    -- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
    -- with @InvalidParameterValueException@.
    initialTopicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnonymousUserQSearchBarEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialTopicId', 'anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId' - The QuickSight Q topic ID of the topic that you want the anonymous user
-- to see first. This ID is included in the output URL. When the URL in
-- response is accessed, Amazon QuickSight renders the Q search bar with
-- this topic pre-selected.
--
-- The Amazon Resource Name (ARN) of this Q topic must be included in the
-- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
-- with @InvalidParameterValueException@.
newAnonymousUserQSearchBarEmbeddingConfiguration ::
  -- | 'initialTopicId'
  Prelude.Text ->
  AnonymousUserQSearchBarEmbeddingConfiguration
newAnonymousUserQSearchBarEmbeddingConfiguration
  pInitialTopicId_ =
    AnonymousUserQSearchBarEmbeddingConfiguration'
      { initialTopicId =
          pInitialTopicId_
      }

-- | The QuickSight Q topic ID of the topic that you want the anonymous user
-- to see first. This ID is included in the output URL. When the URL in
-- response is accessed, Amazon QuickSight renders the Q search bar with
-- this topic pre-selected.
--
-- The Amazon Resource Name (ARN) of this Q topic must be included in the
-- @AuthorizedResourceArns@ parameter. Otherwise, the request will fail
-- with @InvalidParameterValueException@.
anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId :: Lens.Lens' AnonymousUserQSearchBarEmbeddingConfiguration Prelude.Text
anonymousUserQSearchBarEmbeddingConfiguration_initialTopicId = Lens.lens (\AnonymousUserQSearchBarEmbeddingConfiguration' {initialTopicId} -> initialTopicId) (\s@AnonymousUserQSearchBarEmbeddingConfiguration' {} a -> s {initialTopicId = a} :: AnonymousUserQSearchBarEmbeddingConfiguration)

instance
  Prelude.Hashable
    AnonymousUserQSearchBarEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    AnonymousUserQSearchBarEmbeddingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` initialTopicId

instance
  Prelude.NFData
    AnonymousUserQSearchBarEmbeddingConfiguration
  where
  rnf
    AnonymousUserQSearchBarEmbeddingConfiguration' {..} =
      Prelude.rnf initialTopicId

instance
  Data.ToJSON
    AnonymousUserQSearchBarEmbeddingConfiguration
  where
  toJSON
    AnonymousUserQSearchBarEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("InitialTopicId" Data..= initialTopicId)
            ]
        )
