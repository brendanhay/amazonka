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
-- Module      : Amazonka.SESV2.Types.ReplacementEmailContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ReplacementEmailContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ReplacementTemplate

-- | The @ReplaceEmailContent@ object to be used for a specific
-- @BulkEmailEntry@. The @ReplacementTemplate@ can be specified within this
-- object.
--
-- /See:/ 'newReplacementEmailContent' smart constructor.
data ReplacementEmailContent = ReplacementEmailContent'
  { -- | The @ReplacementTemplate@ associated with @ReplacementEmailContent@.
    replacementTemplate :: Prelude.Maybe ReplacementTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplacementEmailContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementTemplate', 'replacementEmailContent_replacementTemplate' - The @ReplacementTemplate@ associated with @ReplacementEmailContent@.
newReplacementEmailContent ::
  ReplacementEmailContent
newReplacementEmailContent =
  ReplacementEmailContent'
    { replacementTemplate =
        Prelude.Nothing
    }

-- | The @ReplacementTemplate@ associated with @ReplacementEmailContent@.
replacementEmailContent_replacementTemplate :: Lens.Lens' ReplacementEmailContent (Prelude.Maybe ReplacementTemplate)
replacementEmailContent_replacementTemplate = Lens.lens (\ReplacementEmailContent' {replacementTemplate} -> replacementTemplate) (\s@ReplacementEmailContent' {} a -> s {replacementTemplate = a} :: ReplacementEmailContent)

instance Prelude.Hashable ReplacementEmailContent where
  hashWithSalt _salt ReplacementEmailContent' {..} =
    _salt `Prelude.hashWithSalt` replacementTemplate

instance Prelude.NFData ReplacementEmailContent where
  rnf ReplacementEmailContent' {..} =
    Prelude.rnf replacementTemplate

instance Data.ToJSON ReplacementEmailContent where
  toJSON ReplacementEmailContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReplacementTemplate" Data..=)
              Prelude.<$> replacementTemplate
          ]
      )
