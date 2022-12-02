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
-- Module      : Amazonka.SESV2.Types.BulkEmailContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.BulkEmailContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.Template

-- | An object that contains the body of the message. You can specify a
-- template message.
--
-- /See:/ 'newBulkEmailContent' smart constructor.
data BulkEmailContent = BulkEmailContent'
  { -- | The template to use for the bulk email message.
    template :: Prelude.Maybe Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkEmailContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'bulkEmailContent_template' - The template to use for the bulk email message.
newBulkEmailContent ::
  BulkEmailContent
newBulkEmailContent =
  BulkEmailContent' {template = Prelude.Nothing}

-- | The template to use for the bulk email message.
bulkEmailContent_template :: Lens.Lens' BulkEmailContent (Prelude.Maybe Template)
bulkEmailContent_template = Lens.lens (\BulkEmailContent' {template} -> template) (\s@BulkEmailContent' {} a -> s {template = a} :: BulkEmailContent)

instance Prelude.Hashable BulkEmailContent where
  hashWithSalt _salt BulkEmailContent' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData BulkEmailContent where
  rnf BulkEmailContent' {..} = Prelude.rnf template

instance Data.ToJSON BulkEmailContent where
  toJSON BulkEmailContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Template" Data..=) Prelude.<$> template]
      )
