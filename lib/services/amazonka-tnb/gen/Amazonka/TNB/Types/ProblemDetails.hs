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
-- Module      : Amazonka.TNB.Types.ProblemDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ProblemDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details related to problems with AWS TNB resources.
--
-- /See:/ 'newProblemDetails' smart constructor.
data ProblemDetails = ProblemDetails'
  { -- | A human-readable title of the problem type.
    title :: Prelude.Maybe Prelude.Text,
    -- | A human-readable explanation specific to this occurrence of the problem.
    detail :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProblemDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'problemDetails_title' - A human-readable title of the problem type.
--
-- 'detail', 'problemDetails_detail' - A human-readable explanation specific to this occurrence of the problem.
newProblemDetails ::
  -- | 'detail'
  Prelude.Text ->
  ProblemDetails
newProblemDetails pDetail_ =
  ProblemDetails'
    { title = Prelude.Nothing,
      detail = pDetail_
    }

-- | A human-readable title of the problem type.
problemDetails_title :: Lens.Lens' ProblemDetails (Prelude.Maybe Prelude.Text)
problemDetails_title = Lens.lens (\ProblemDetails' {title} -> title) (\s@ProblemDetails' {} a -> s {title = a} :: ProblemDetails)

-- | A human-readable explanation specific to this occurrence of the problem.
problemDetails_detail :: Lens.Lens' ProblemDetails Prelude.Text
problemDetails_detail = Lens.lens (\ProblemDetails' {detail} -> detail) (\s@ProblemDetails' {} a -> s {detail = a} :: ProblemDetails)

instance Data.FromJSON ProblemDetails where
  parseJSON =
    Data.withObject
      "ProblemDetails"
      ( \x ->
          ProblemDetails'
            Prelude.<$> (x Data..:? "title")
            Prelude.<*> (x Data..: "detail")
      )

instance Prelude.Hashable ProblemDetails where
  hashWithSalt _salt ProblemDetails' {..} =
    _salt
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` detail

instance Prelude.NFData ProblemDetails where
  rnf ProblemDetails' {..} =
    Prelude.rnf title `Prelude.seq` Prelude.rnf detail
