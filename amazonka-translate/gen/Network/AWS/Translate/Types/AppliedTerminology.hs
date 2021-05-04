{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Translate.Types.AppliedTerminology
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.AppliedTerminology where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Translate.Types.Term

-- | The custom terminology applied to the input text by Amazon Translate for
-- the translated text response. This is optional in the response and will
-- only be present if you specified terminology input in the request.
-- Currently, only one terminology can be applied per TranslateText
-- request.
--
-- /See:/ 'newAppliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { -- | The specific terms of the custom terminology applied to the input text
    -- by Amazon Translate for the translated text response. A maximum of 250
    -- terms will be returned, and the specific terms applied will be the first
    -- 250 terms in the source text.
    terms :: Prelude.Maybe [Term],
    -- | The name of the custom terminology applied to the input text by Amazon
    -- Translate for the translated text response.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AppliedTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terms', 'appliedTerminology_terms' - The specific terms of the custom terminology applied to the input text
-- by Amazon Translate for the translated text response. A maximum of 250
-- terms will be returned, and the specific terms applied will be the first
-- 250 terms in the source text.
--
-- 'name', 'appliedTerminology_name' - The name of the custom terminology applied to the input text by Amazon
-- Translate for the translated text response.
newAppliedTerminology ::
  AppliedTerminology
newAppliedTerminology =
  AppliedTerminology'
    { terms = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The specific terms of the custom terminology applied to the input text
-- by Amazon Translate for the translated text response. A maximum of 250
-- terms will be returned, and the specific terms applied will be the first
-- 250 terms in the source text.
appliedTerminology_terms :: Lens.Lens' AppliedTerminology (Prelude.Maybe [Term])
appliedTerminology_terms = Lens.lens (\AppliedTerminology' {terms} -> terms) (\s@AppliedTerminology' {} a -> s {terms = a} :: AppliedTerminology) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the custom terminology applied to the input text by Amazon
-- Translate for the translated text response.
appliedTerminology_name :: Lens.Lens' AppliedTerminology (Prelude.Maybe Prelude.Text)
appliedTerminology_name = Lens.lens (\AppliedTerminology' {name} -> name) (\s@AppliedTerminology' {} a -> s {name = a} :: AppliedTerminology)

instance Prelude.FromJSON AppliedTerminology where
  parseJSON =
    Prelude.withObject
      "AppliedTerminology"
      ( \x ->
          AppliedTerminology'
            Prelude.<$> (x Prelude..:? "Terms" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable AppliedTerminology

instance Prelude.NFData AppliedTerminology
