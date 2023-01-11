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
-- Module      : Amazonka.ResilienceHub.Types.ResourceErrorsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceErrorsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.ResourceError

-- | A list of errors retrieving an application\'s resources.
--
-- /See:/ 'newResourceErrorsDetails' smart constructor.
data ResourceErrorsDetails = ResourceErrorsDetails'
  { -- | This indicates if there are more errors not listed in the resourceErrors
    -- list.
    hasMoreErrors :: Prelude.Maybe Prelude.Bool,
    -- | A list of errors retrieving an application\'s resources.
    resourceErrors :: Prelude.Maybe [ResourceError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceErrorsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hasMoreErrors', 'resourceErrorsDetails_hasMoreErrors' - This indicates if there are more errors not listed in the resourceErrors
-- list.
--
-- 'resourceErrors', 'resourceErrorsDetails_resourceErrors' - A list of errors retrieving an application\'s resources.
newResourceErrorsDetails ::
  ResourceErrorsDetails
newResourceErrorsDetails =
  ResourceErrorsDetails'
    { hasMoreErrors =
        Prelude.Nothing,
      resourceErrors = Prelude.Nothing
    }

-- | This indicates if there are more errors not listed in the resourceErrors
-- list.
resourceErrorsDetails_hasMoreErrors :: Lens.Lens' ResourceErrorsDetails (Prelude.Maybe Prelude.Bool)
resourceErrorsDetails_hasMoreErrors = Lens.lens (\ResourceErrorsDetails' {hasMoreErrors} -> hasMoreErrors) (\s@ResourceErrorsDetails' {} a -> s {hasMoreErrors = a} :: ResourceErrorsDetails)

-- | A list of errors retrieving an application\'s resources.
resourceErrorsDetails_resourceErrors :: Lens.Lens' ResourceErrorsDetails (Prelude.Maybe [ResourceError])
resourceErrorsDetails_resourceErrors = Lens.lens (\ResourceErrorsDetails' {resourceErrors} -> resourceErrors) (\s@ResourceErrorsDetails' {} a -> s {resourceErrors = a} :: ResourceErrorsDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceErrorsDetails where
  parseJSON =
    Data.withObject
      "ResourceErrorsDetails"
      ( \x ->
          ResourceErrorsDetails'
            Prelude.<$> (x Data..:? "hasMoreErrors")
            Prelude.<*> ( x Data..:? "resourceErrors"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ResourceErrorsDetails where
  hashWithSalt _salt ResourceErrorsDetails' {..} =
    _salt `Prelude.hashWithSalt` hasMoreErrors
      `Prelude.hashWithSalt` resourceErrors

instance Prelude.NFData ResourceErrorsDetails where
  rnf ResourceErrorsDetails' {..} =
    Prelude.rnf hasMoreErrors
      `Prelude.seq` Prelude.rnf resourceErrors
