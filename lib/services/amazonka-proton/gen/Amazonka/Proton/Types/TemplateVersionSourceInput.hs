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
-- Module      : Amazonka.Proton.Types.TemplateVersionSourceInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.TemplateVersionSourceInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.S3ObjectSource

-- | Template version source data.
--
-- /See:/ 'newTemplateVersionSourceInput' smart constructor.
data TemplateVersionSourceInput = TemplateVersionSourceInput'
  { -- | An S3 source object that includes the template bundle S3 path and name
    -- for a template minor version.
    s3 :: Prelude.Maybe S3ObjectSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionSourceInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'templateVersionSourceInput_s3' - An S3 source object that includes the template bundle S3 path and name
-- for a template minor version.
newTemplateVersionSourceInput ::
  TemplateVersionSourceInput
newTemplateVersionSourceInput =
  TemplateVersionSourceInput' {s3 = Prelude.Nothing}

-- | An S3 source object that includes the template bundle S3 path and name
-- for a template minor version.
templateVersionSourceInput_s3 :: Lens.Lens' TemplateVersionSourceInput (Prelude.Maybe S3ObjectSource)
templateVersionSourceInput_s3 = Lens.lens (\TemplateVersionSourceInput' {s3} -> s3) (\s@TemplateVersionSourceInput' {} a -> s {s3 = a} :: TemplateVersionSourceInput)

instance Prelude.Hashable TemplateVersionSourceInput where
  hashWithSalt _salt TemplateVersionSourceInput' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData TemplateVersionSourceInput where
  rnf TemplateVersionSourceInput' {..} = Prelude.rnf s3

instance Data.ToJSON TemplateVersionSourceInput where
  toJSON TemplateVersionSourceInput' {..} =
    Data.object
      (Prelude.catMaybes [("s3" Data..=) Prelude.<$> s3])
