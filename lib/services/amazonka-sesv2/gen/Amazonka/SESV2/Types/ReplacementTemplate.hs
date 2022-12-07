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
-- Module      : Amazonka.SESV2.Types.ReplacementTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ReplacementTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object which contains @ReplacementTemplateData@ to be used for a
-- specific @BulkEmailEntry@.
--
-- /See:/ 'newReplacementTemplate' smart constructor.
data ReplacementTemplate = ReplacementTemplate'
  { -- | A list of replacement values to apply to the template. This parameter is
    -- a JSON object, typically consisting of key-value pairs in which the keys
    -- correspond to replacement tags in the email template.
    replacementTemplateData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplacementTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementTemplateData', 'replacementTemplate_replacementTemplateData' - A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
newReplacementTemplate ::
  ReplacementTemplate
newReplacementTemplate =
  ReplacementTemplate'
    { replacementTemplateData =
        Prelude.Nothing
    }

-- | A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
replacementTemplate_replacementTemplateData :: Lens.Lens' ReplacementTemplate (Prelude.Maybe Prelude.Text)
replacementTemplate_replacementTemplateData = Lens.lens (\ReplacementTemplate' {replacementTemplateData} -> replacementTemplateData) (\s@ReplacementTemplate' {} a -> s {replacementTemplateData = a} :: ReplacementTemplate)

instance Prelude.Hashable ReplacementTemplate where
  hashWithSalt _salt ReplacementTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` replacementTemplateData

instance Prelude.NFData ReplacementTemplate where
  rnf ReplacementTemplate' {..} =
    Prelude.rnf replacementTemplateData

instance Data.ToJSON ReplacementTemplate where
  toJSON ReplacementTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReplacementTemplateData" Data..=)
              Prelude.<$> replacementTemplateData
          ]
      )
