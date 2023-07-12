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
-- Module      : Amazonka.Translate.Types.AppliedTerminology
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.AppliedTerminology where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.Term

-- | The custom terminology applied to the input text by Amazon Translate for
-- the translated text response. This is optional in the response and will
-- only be present if you specified terminology input in the request.
-- Currently, only one terminology can be applied per TranslateText
-- request.
--
-- /See:/ 'newAppliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { -- | The name of the custom terminology applied to the input text by Amazon
    -- Translate for the translated text response.
    name :: Prelude.Maybe Prelude.Text,
    -- | The specific terms of the custom terminology applied to the input text
    -- by Amazon Translate for the translated text response. A maximum of 250
    -- terms will be returned, and the specific terms applied will be the first
    -- 250 terms in the source text.
    terms :: Prelude.Maybe [Term]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppliedTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'appliedTerminology_name' - The name of the custom terminology applied to the input text by Amazon
-- Translate for the translated text response.
--
-- 'terms', 'appliedTerminology_terms' - The specific terms of the custom terminology applied to the input text
-- by Amazon Translate for the translated text response. A maximum of 250
-- terms will be returned, and the specific terms applied will be the first
-- 250 terms in the source text.
newAppliedTerminology ::
  AppliedTerminology
newAppliedTerminology =
  AppliedTerminology'
    { name = Prelude.Nothing,
      terms = Prelude.Nothing
    }

-- | The name of the custom terminology applied to the input text by Amazon
-- Translate for the translated text response.
appliedTerminology_name :: Lens.Lens' AppliedTerminology (Prelude.Maybe Prelude.Text)
appliedTerminology_name = Lens.lens (\AppliedTerminology' {name} -> name) (\s@AppliedTerminology' {} a -> s {name = a} :: AppliedTerminology)

-- | The specific terms of the custom terminology applied to the input text
-- by Amazon Translate for the translated text response. A maximum of 250
-- terms will be returned, and the specific terms applied will be the first
-- 250 terms in the source text.
appliedTerminology_terms :: Lens.Lens' AppliedTerminology (Prelude.Maybe [Term])
appliedTerminology_terms = Lens.lens (\AppliedTerminology' {terms} -> terms) (\s@AppliedTerminology' {} a -> s {terms = a} :: AppliedTerminology) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AppliedTerminology where
  parseJSON =
    Data.withObject
      "AppliedTerminology"
      ( \x ->
          AppliedTerminology'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Terms" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AppliedTerminology where
  hashWithSalt _salt AppliedTerminology' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` terms

instance Prelude.NFData AppliedTerminology where
  rnf AppliedTerminology' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf terms
