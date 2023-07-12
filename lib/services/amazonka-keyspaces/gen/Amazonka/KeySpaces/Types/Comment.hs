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
-- Module      : Amazonka.KeySpaces.Types.Comment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.Comment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An optional comment that describes the table.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | An optional description of the table.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Comment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'comment_message' - An optional description of the table.
newComment ::
  -- | 'message'
  Prelude.Text ->
  Comment
newComment pMessage_ = Comment' {message = pMessage_}

-- | An optional description of the table.
comment_message :: Lens.Lens' Comment Prelude.Text
comment_message = Lens.lens (\Comment' {message} -> message) (\s@Comment' {} a -> s {message = a} :: Comment)

instance Data.FromJSON Comment where
  parseJSON =
    Data.withObject
      "Comment"
      (\x -> Comment' Prelude.<$> (x Data..: "message"))

instance Prelude.Hashable Comment where
  hashWithSalt _salt Comment' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData Comment where
  rnf Comment' {..} = Prelude.rnf message

instance Data.ToJSON Comment where
  toJSON Comment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("message" Data..= message)]
      )
