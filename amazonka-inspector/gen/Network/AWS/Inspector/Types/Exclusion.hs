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
-- Module      : Network.AWS.Inspector.Types.Exclusion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Exclusion where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Scope
import qualified Network.AWS.Lens as Lens

-- | Contains information about what was excluded from an assessment run.
--
-- /See:/ 'newExclusion' smart constructor.
data Exclusion = Exclusion'
  { -- | The system-defined attributes for the exclusion.
    attributes :: Core.Maybe [Attribute],
    -- | The ARN that specifies the exclusion.
    arn :: Core.Text,
    -- | The name of the exclusion.
    title :: Core.Text,
    -- | The description of the exclusion.
    description :: Core.Text,
    -- | The recommendation for the exclusion.
    recommendation :: Core.Text,
    -- | The AWS resources for which the exclusion pertains.
    scopes :: Core.NonEmpty Scope
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Exclusion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'exclusion_attributes' - The system-defined attributes for the exclusion.
--
-- 'arn', 'exclusion_arn' - The ARN that specifies the exclusion.
--
-- 'title', 'exclusion_title' - The name of the exclusion.
--
-- 'description', 'exclusion_description' - The description of the exclusion.
--
-- 'recommendation', 'exclusion_recommendation' - The recommendation for the exclusion.
--
-- 'scopes', 'exclusion_scopes' - The AWS resources for which the exclusion pertains.
newExclusion ::
  -- | 'arn'
  Core.Text ->
  -- | 'title'
  Core.Text ->
  -- | 'description'
  Core.Text ->
  -- | 'recommendation'
  Core.Text ->
  -- | 'scopes'
  Core.NonEmpty Scope ->
  Exclusion
newExclusion
  pArn_
  pTitle_
  pDescription_
  pRecommendation_
  pScopes_ =
    Exclusion'
      { attributes = Core.Nothing,
        arn = pArn_,
        title = pTitle_,
        description = pDescription_,
        recommendation = pRecommendation_,
        scopes = Lens._Coerce Lens.# pScopes_
      }

-- | The system-defined attributes for the exclusion.
exclusion_attributes :: Lens.Lens' Exclusion (Core.Maybe [Attribute])
exclusion_attributes = Lens.lens (\Exclusion' {attributes} -> attributes) (\s@Exclusion' {} a -> s {attributes = a} :: Exclusion) Core.. Lens.mapping Lens._Coerce

-- | The ARN that specifies the exclusion.
exclusion_arn :: Lens.Lens' Exclusion Core.Text
exclusion_arn = Lens.lens (\Exclusion' {arn} -> arn) (\s@Exclusion' {} a -> s {arn = a} :: Exclusion)

-- | The name of the exclusion.
exclusion_title :: Lens.Lens' Exclusion Core.Text
exclusion_title = Lens.lens (\Exclusion' {title} -> title) (\s@Exclusion' {} a -> s {title = a} :: Exclusion)

-- | The description of the exclusion.
exclusion_description :: Lens.Lens' Exclusion Core.Text
exclusion_description = Lens.lens (\Exclusion' {description} -> description) (\s@Exclusion' {} a -> s {description = a} :: Exclusion)

-- | The recommendation for the exclusion.
exclusion_recommendation :: Lens.Lens' Exclusion Core.Text
exclusion_recommendation = Lens.lens (\Exclusion' {recommendation} -> recommendation) (\s@Exclusion' {} a -> s {recommendation = a} :: Exclusion)

-- | The AWS resources for which the exclusion pertains.
exclusion_scopes :: Lens.Lens' Exclusion (Core.NonEmpty Scope)
exclusion_scopes = Lens.lens (\Exclusion' {scopes} -> scopes) (\s@Exclusion' {} a -> s {scopes = a} :: Exclusion) Core.. Lens._Coerce

instance Core.FromJSON Exclusion where
  parseJSON =
    Core.withObject
      "Exclusion"
      ( \x ->
          Exclusion'
            Core.<$> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..: "arn")
            Core.<*> (x Core..: "title")
            Core.<*> (x Core..: "description")
            Core.<*> (x Core..: "recommendation")
            Core.<*> (x Core..: "scopes")
      )

instance Core.Hashable Exclusion

instance Core.NFData Exclusion
