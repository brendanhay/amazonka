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
-- Module      : Amazonka.Inspector.Types.ExclusionPreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.ExclusionPreview where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.Attribute
import Amazonka.Inspector.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | Contains information about what is excluded from an assessment run given
-- the current state of the assessment template.
--
-- /See:/ 'newExclusionPreview' smart constructor.
data ExclusionPreview = ExclusionPreview'
  { -- | The system-defined attributes for the exclusion preview.
    attributes :: Prelude.Maybe [Attribute],
    -- | The name of the exclusion preview.
    title :: Prelude.Text,
    -- | The description of the exclusion preview.
    description :: Prelude.Text,
    -- | The recommendation for the exclusion preview.
    recommendation :: Prelude.Text,
    -- | The AWS resources for which the exclusion preview pertains.
    scopes :: Prelude.NonEmpty Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExclusionPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'exclusionPreview_attributes' - The system-defined attributes for the exclusion preview.
--
-- 'title', 'exclusionPreview_title' - The name of the exclusion preview.
--
-- 'description', 'exclusionPreview_description' - The description of the exclusion preview.
--
-- 'recommendation', 'exclusionPreview_recommendation' - The recommendation for the exclusion preview.
--
-- 'scopes', 'exclusionPreview_scopes' - The AWS resources for which the exclusion preview pertains.
newExclusionPreview ::
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'recommendation'
  Prelude.Text ->
  -- | 'scopes'
  Prelude.NonEmpty Scope ->
  ExclusionPreview
newExclusionPreview
  pTitle_
  pDescription_
  pRecommendation_
  pScopes_ =
    ExclusionPreview'
      { attributes = Prelude.Nothing,
        title = pTitle_,
        description = pDescription_,
        recommendation = pRecommendation_,
        scopes = Lens.coerced Lens.# pScopes_
      }

-- | The system-defined attributes for the exclusion preview.
exclusionPreview_attributes :: Lens.Lens' ExclusionPreview (Prelude.Maybe [Attribute])
exclusionPreview_attributes = Lens.lens (\ExclusionPreview' {attributes} -> attributes) (\s@ExclusionPreview' {} a -> s {attributes = a} :: ExclusionPreview) Prelude.. Lens.mapping Lens.coerced

-- | The name of the exclusion preview.
exclusionPreview_title :: Lens.Lens' ExclusionPreview Prelude.Text
exclusionPreview_title = Lens.lens (\ExclusionPreview' {title} -> title) (\s@ExclusionPreview' {} a -> s {title = a} :: ExclusionPreview)

-- | The description of the exclusion preview.
exclusionPreview_description :: Lens.Lens' ExclusionPreview Prelude.Text
exclusionPreview_description = Lens.lens (\ExclusionPreview' {description} -> description) (\s@ExclusionPreview' {} a -> s {description = a} :: ExclusionPreview)

-- | The recommendation for the exclusion preview.
exclusionPreview_recommendation :: Lens.Lens' ExclusionPreview Prelude.Text
exclusionPreview_recommendation = Lens.lens (\ExclusionPreview' {recommendation} -> recommendation) (\s@ExclusionPreview' {} a -> s {recommendation = a} :: ExclusionPreview)

-- | The AWS resources for which the exclusion preview pertains.
exclusionPreview_scopes :: Lens.Lens' ExclusionPreview (Prelude.NonEmpty Scope)
exclusionPreview_scopes = Lens.lens (\ExclusionPreview' {scopes} -> scopes) (\s@ExclusionPreview' {} a -> s {scopes = a} :: ExclusionPreview) Prelude.. Lens.coerced

instance Data.FromJSON ExclusionPreview where
  parseJSON =
    Data.withObject
      "ExclusionPreview"
      ( \x ->
          ExclusionPreview'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "title")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "recommendation")
            Prelude.<*> (x Data..: "scopes")
      )

instance Prelude.Hashable ExclusionPreview where
  hashWithSalt _salt ExclusionPreview' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendation
      `Prelude.hashWithSalt` scopes

instance Prelude.NFData ExclusionPreview where
  rnf ExclusionPreview' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendation
      `Prelude.seq` Prelude.rnf scopes
